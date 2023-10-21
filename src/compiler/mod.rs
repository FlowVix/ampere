use std::rc::Rc;

use ahash::AHashMap;
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
    proto::{JumpType, ProtoBytecode},
    scope::{Scope, ScopeID, VarData},
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

    pub fn derive_scope(&mut self, scope: ScopeID) -> ScopeID {
        let new = Scope {
            vars: AHashMap::new(),
            parent: Some(scope),
        };
        self.scopes.insert(new)
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
            ExprType::Call { base, args } => todo!(),
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
                let derived = self.derive_scope(scope);
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
                        let derived = self.derive_scope(scope);

                        self.compile_expr(cond, builder, derived)?;
                        builder.push_jump(None, JumpType::EndIfFalse, expr.span);

                        self.compile_expr(then, builder, derived)?;
                        builder.push_jump(Some(outer), JumpType::End, expr.span);
                        Ok(())
                    })?;
                    if let Some(s) = otherwise {
                        let derived = self.derive_scope(scope);

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
                    let derived = self.derive_scope(scope);

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
            } => todo!(),
            ExprType::Dbg(v) => {
                self.compile_expr(v, builder, scope)?;
                builder.push_raw_opcode(Opcode::Dbg, expr.span);
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
                self.compile_expr(v, builder, scope)?;
                self.do_let(pat, builder, scope)?;
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
                });
                compiler.compile_stmts(stmts, builder, global_scope, span)?;
                Ok(())
            },
            span,
        )?;

        Ok(code)
    }
}
