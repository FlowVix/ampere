use std::rc::Rc;

use ahash::AHashMap;
use itertools::Itertools;
use lasso::{Rodeo, Spur};

use crate::{
    parser::{
        ast::{
            AssignPatternNode, AssignPatternType, ExprNode, ExprType, LetPatternNode,
            LetPatternType, StmtNode, StmtType, Stmts,
        },
        operators::{BinOp, UnaryOp},
    },
    source::{AmpereSource, CodeArea, CodeSpan},
    util::{slabmap::SlabMap, unique_register::UniqueRegister},
};

use self::{
    builder::CodeBuilder,
    bytecode::Constant,
    error::CompilerError,
    opcodes::{Opcode, VarID},
    proto::{BlockID, JumpType, ProtoBytecode},
    scope::{Scope, ScopeID, ScopeType, VarData},
};

pub mod builder;
pub mod bytecode;
pub mod error;
pub mod opcodes;
pub mod proto;
pub mod scope;

pub struct Compiler<'a> {
    src: &'a Rc<AmpereSource>,
    interner: &'a mut Rodeo,
    scopes: SlabMap<ScopeID, Scope>,
}

pub type CompileResult<T> = Result<T, CompilerError>;

impl<'a> Compiler<'a> {
    pub fn new(src: &'a Rc<AmpereSource>, interner: &'a mut Rodeo) -> Self {
        Self {
            src,
            interner,
            scopes: SlabMap::new(),
        }
    }
    pub fn resolve(&self, s: &Spur) -> &str {
        self.interner.resolve(s)
    }
    pub fn get_var(&self, var: &Spur, scope: ScopeID) -> Option<VarData> {
        match self.scopes[scope].vars.get(var) {
            Some(v) => Some(*v),
            None => match self.scopes[scope].parent {
                Some(s) => self.get_var(var, s),
                None => None,
            },
        }
    }
    pub fn for_accessible_vars<F>(&self, scope: ScopeID, f: &mut F)
    where
        F: FnMut(Spur, VarData),
    {
        let s = &self.scopes[scope];
        if let Some(t) = s.parent {
            self.for_accessible_vars(t, f);
        }
        for (&a, &b) in &s.vars {
            f(a, b)
        }
    }
    pub fn new_var(
        &mut self,
        var: Spur,
        def_span: CodeSpan,
        scope: ScopeID,
        builder: &mut CodeBuilder,
    ) -> VarID {
        let id = builder.next_var_id();
        self.scopes[scope]
            .vars
            .insert(var, VarData { def_span, id });
        id
    }
    pub fn make_area(&self, span: CodeSpan) -> CodeArea {
        CodeArea {
            span,
            src: self.src.clone(),
        }
    }

    pub fn derive_scope(&mut self, scope: ScopeID, typ: ScopeType) -> ScopeID {
        let new = Scope {
            vars: AHashMap::new(),
            parent: Some(scope),
            typ,
        };
        self.scopes.insert(new)
    }

    pub fn get_loop(&self, scope: ScopeID) -> Option<BlockID> {
        match self.scopes[scope].typ {
            ScopeType::Loop(v) => Some(v),
            _ => match self.scopes[scope].parent {
                Some(s) => self.get_loop(s),
                None => None,
            },
        }
    }

    pub fn do_let(
        &mut self,
        pat: &LetPatternNode,
        builder: &mut CodeBuilder,
        scope: ScopeID,
    ) -> CompileResult<()> {
        match &pat.typ {
            LetPatternType::Var(v) => {
                let id = self.new_var(*v, pat.span, scope, builder);
                builder.push_raw_opcode(Opcode::SetVar(id), pat.span);
            }
            LetPatternType::ArrayDestructure(pats) => {
                builder.push_raw_opcode(Opcode::UnwrapArray(pats.len() as u16), pat.span);
                for pat in pats.iter().rev() {
                    self.do_let(pat, builder, scope)?;
                }
            }
            LetPatternType::TupleDestructure(pats) => {
                builder.push_raw_opcode(Opcode::UnwrapTuple(pats.len() as u16), pat.span);
                for pat in pats.iter().rev() {
                    self.do_let(pat, builder, scope)?;
                }
            }
        }
        Ok(())
    }

    pub fn do_assign(
        &mut self,
        pat: &AssignPatternNode,
        builder: &mut CodeBuilder,
        scope: ScopeID,
    ) -> CompileResult<()> {
        match &pat.typ {
            AssignPatternType::Path { var, path } => match self.get_var(var, scope) {
                Some(v) => {
                    builder.push_raw_opcode(Opcode::SetVar(v.id), pat.span);
                }
                None => {
                    return Err(CompilerError::NonexistentVariable(
                        self.resolve(var).into(),
                        self.make_area(pat.span),
                    ))
                }
            },
            AssignPatternType::ArrayDestructure(pats) => {
                builder.push_raw_opcode(Opcode::UnwrapArray(pats.len() as u16), pat.span);
                for pat in pats.iter().rev() {
                    self.do_assign(pat, builder, scope)?;
                }
            }
            AssignPatternType::TupleDestructure(pats) => {
                builder.push_raw_opcode(Opcode::UnwrapTuple(pats.len() as u16), pat.span);
                for pat in pats.iter().rev() {
                    self.do_assign(pat, builder, scope)?;
                }
            }
        }
        Ok(())
    }

    pub fn compile_expr(
        &mut self,
        expr: &ExprNode,
        builder: &mut CodeBuilder,
        scope: ScopeID,
    ) -> CompileResult<()> {
        match &expr.typ {
            ExprType::Int(v) => builder.load_const(Constant::Int(*v), expr.span),
            ExprType::Float(v) => builder.load_const(Constant::Float(*v), expr.span),
            ExprType::String(v) => {
                builder.load_const(Constant::String(self.resolve(v).into()), expr.span)
            }
            ExprType::Bool(v) => builder.load_const(Constant::Bool(*v), expr.span),
            ExprType::Var(name) => match self.get_var(name, scope) {
                Some(v) => builder.push_raw_opcode(Opcode::LoadVar(v.id), expr.span),
                None => {
                    return Err(CompilerError::NonexistentVariable(
                        self.resolve(name).into(),
                        self.make_area(expr.span),
                    ))
                }
            },
            ExprType::Unary(op, v) => {
                self.compile_expr(v, builder, scope)?;
                match op {
                    UnaryOp::Not => builder.push_raw_opcode(Opcode::UnaryNot, expr.span),
                    UnaryOp::Minus => builder.push_raw_opcode(Opcode::UnaryMinus, expr.span),
                }
            }
            ExprType::Op(a, op, b) => {
                self.compile_expr(a, builder, scope)?;
                self.compile_expr(b, builder, scope)?;
                match op {
                    BinOp::Plus => builder.push_raw_opcode(Opcode::Plus, expr.span),
                    BinOp::Minus => builder.push_raw_opcode(Opcode::Minus, expr.span),
                    BinOp::Mult => builder.push_raw_opcode(Opcode::Mult, expr.span),
                    BinOp::Div => builder.push_raw_opcode(Opcode::Div, expr.span),
                    BinOp::Mod => builder.push_raw_opcode(Opcode::Modulo, expr.span),
                    BinOp::Pow => builder.push_raw_opcode(Opcode::Pow, expr.span),
                    BinOp::Eq => builder.push_raw_opcode(Opcode::Eq, expr.span),
                    BinOp::NotEq => builder.push_raw_opcode(Opcode::NotEq, expr.span),
                    BinOp::Gt => builder.push_raw_opcode(Opcode::Gt, expr.span),
                    BinOp::Gte => builder.push_raw_opcode(Opcode::Gte, expr.span),
                    BinOp::Lt => builder.push_raw_opcode(Opcode::Lt, expr.span),
                    BinOp::Lte => builder.push_raw_opcode(Opcode::Lte, expr.span),
                }
            }
            ExprType::Index { base, index } => todo!(),
            ExprType::Member { base, member } => todo!(),
            ExprType::Associated { base, member } => todo!(),
            ExprType::Call { base, args } => {
                self.compile_expr(base, builder, scope)?;
                for i in args {
                    self.compile_expr(i, builder, scope)?;
                }
                builder.push_raw_opcode(Opcode::Call(args.len() as u16), expr.span);
            }
            ExprType::Array(v) => {
                for i in v {
                    self.compile_expr(i, builder, scope)?;
                }
                builder.push_raw_opcode(Opcode::WrapArray(v.len() as u16), expr.span);
            }
            ExprType::Tuple(v) => {
                for i in v {
                    self.compile_expr(i, builder, scope)?;
                }
                builder.push_raw_opcode(Opcode::WrapTuple(v.len() as u16), expr.span);
            }
            ExprType::Block(v) => {
                let derived = self.derive_scope(scope, ScopeType::Normal);
                self.compile_stmts(v, builder, derived, expr.span)?;
            }
            ExprType::If {
                cond,
                then,
                otherwise,
            } => {
                builder.new_block(|builder| {
                    let outer = builder.block;

                    builder.new_block(|builder| {
                        let derived = self.derive_scope(scope, ScopeType::Normal);

                        self.compile_expr(cond, builder, derived)?;
                        builder.push_jump(None, JumpType::EndIfFalse, expr.span);

                        self.compile_expr(then, builder, derived)?;
                        builder.push_jump(Some(outer), JumpType::End, expr.span);
                        Ok(())
                    })?;
                    if let Some(s) = otherwise {
                        let derived = self.derive_scope(scope, ScopeType::Normal);

                        self.compile_expr(s, builder, derived)?;
                    } else {
                        builder.push_raw_opcode(Opcode::PushUnit, expr.span);
                    }

                    Ok(())
                })?;
            }
            ExprType::While { cond, body } => {
                builder.push_raw_opcode(Opcode::PushUnit, expr.span);

                builder.new_block(|builder| {
                    let derived = self.derive_scope(scope, ScopeType::Loop(builder.block));

                    self.compile_expr(cond, builder, derived)?;
                    builder.push_jump(None, JumpType::EndIfFalse, expr.span);
                    builder.push_raw_opcode(Opcode::PopTop, expr.span);

                    self.compile_expr(body, builder, derived)?;
                    builder.push_jump(None, JumpType::Start, expr.span);
                    Ok(())
                })?;
            }
            ExprType::Function {
                params,
                ret_type,
                body,
            } => {
                let mut captured_names = AHashMap::new();
                self.for_accessible_vars(scope, &mut |name, data| {
                    captured_names.insert(name, data);
                });

                let id = builder.new_func(
                    |builder| {
                        let func_scope = self.scopes.insert(Scope {
                            vars: AHashMap::new(),
                            parent: None,
                            typ: ScopeType::FuncBody,
                        });
                        let mut captured_map = vec![];

                        for (name, data) in captured_names {
                            let new_id = self.new_var(name, data.def_span, func_scope, builder);
                            captured_map.push((data.id, new_id));
                        }

                        for (pat, _) in params.iter().rev() {
                            self.do_let(pat, builder, func_scope)?;
                        }

                        self.compile_expr(body, builder, func_scope)?;

                        Ok(captured_map)
                    },
                    expr.span,
                )?;
                builder.push_raw_opcode(Opcode::PushFunc(id), expr.span);
                builder.push_raw_opcode(Opcode::SetArgAmount(params.len() as u16), expr.span);
                for (idx, (_, t)) in params.iter().enumerate() {
                    if let Some(t) = t {
                        self.compile_expr(t, builder, scope)?;
                        builder.push_raw_opcode(Opcode::SetArgType(idx as u16), t.span);
                    }
                }
                if let Some(t) = ret_type {
                    self.compile_expr(t, builder, scope)?;
                    builder.push_raw_opcode(Opcode::SetReturnType, t.span);
                }
            }
            ExprType::Dbg(v) => {
                self.compile_expr(v, builder, scope)?;
                builder.push_raw_opcode(Opcode::Dbg, expr.span);
            }
            ExprType::Return(_) => todo!(),
            ExprType::Break(v) => {
                if let Some(v) = v {
                    self.compile_expr(v, builder, scope)?
                } else {
                    builder.push_raw_opcode(Opcode::PushUnit, expr.span);
                }
                if let Some(b) = self.get_loop(scope) {
                    builder.push_jump(Some(b), JumpType::End, expr.span);
                } else {
                    return Err(CompilerError::BreakOutsideLoop(self.make_area(expr.span)));
                }
            }
            ExprType::Continue(v) => {
                if let Some(v) = v {
                    self.compile_expr(v, builder, scope)?
                } else {
                    builder.push_raw_opcode(Opcode::PushUnit, expr.span);
                }
                if let Some(b) = self.get_loop(scope) {
                    builder.push_jump(Some(b), JumpType::Start, expr.span);
                } else {
                    return Err(CompilerError::ContinueOutsideLoop(
                        self.make_area(expr.span),
                    ));
                }
            }
        }

        Ok(())
    }
    pub fn compile_stmt(
        &mut self,
        stmt: &StmtNode,
        builder: &mut CodeBuilder,
        scope: ScopeID,
        ret: bool,
    ) -> CompileResult<()> {
        match &stmt.typ {
            StmtType::Expr(v) => {
                self.compile_expr(v, builder, scope)?;
                if !ret {
                    builder.push_raw_opcode(Opcode::PopTop, stmt.span);
                }
                return Ok(());
            }
            StmtType::Let(pat, v) => {
                let b1 = builder.new_block(|_| Ok(()))?;
                let b2 = builder.new_block(|_| Ok(()))?;

                builder.in_block(b2, |builder| {
                    self.do_let(pat, builder, scope)?;
                    Ok(())
                })?;
                builder.in_block(b1, |builder| {
                    self.compile_expr(v, builder, scope)?;
                    Ok(())
                })?;
            }
            StmtType::Assign(pat, v) => {
                self.compile_expr(v, builder, scope)?;
                self.do_assign(pat, builder, scope)?;
            }
            StmtType::AssignOp(_, _, _) => todo!(),
        }

        if ret {
            builder.push_raw_opcode(Opcode::PushUnit, stmt.span);
        }

        Ok(())
    }
    pub fn compile_stmts(
        &mut self,
        stmts: &Stmts,
        builder: &mut CodeBuilder,
        scope: ScopeID,
        span: CodeSpan,
    ) -> CompileResult<()> {
        let (v, r) = match stmts {
            Stmts::Normal(v) => (v, None),
            Stmts::Ret(v, r) => (v, Some(r)),
        };

        for i in v {
            self.compile_stmt(i, builder, scope, false)?;
        }
        if let Some(r) = r {
            self.compile_stmt(r, builder, scope, true)?;
        } else {
            builder.push_raw_opcode(Opcode::PushUnit, span);
        }

        Ok(())
    }
    pub fn new_compile_file(
        stmts: &Stmts,
        src: &'a Rc<AmpereSource>,
        interner: &'a mut Rodeo,
        span: CodeSpan,
    ) -> CompileResult<ProtoBytecode> {
        let mut compiler = Self::new(src, interner);

        let mut code = ProtoBytecode {
            consts: UniqueRegister::new(),
            functions: vec![],
            blocks: SlabMap::new(),
        };
        code.new_func(
            |builder| {
                let global_scope = compiler.scopes.insert(Scope {
                    vars: AHashMap::new(),
                    parent: None,
                    typ: ScopeType::File,
                });
                compiler.compile_stmts(stmts, builder, global_scope, span)?;
                Ok(vec![])
            },
            // vec![],
            // false,
            span,
        )?;

        Ok(code)
    }
}
