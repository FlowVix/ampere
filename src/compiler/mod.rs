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
    opcodes::{Opcode, Register},
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
    ) -> &VarData {
        let reg = builder.next_reg();
        let data = VarData { def_span, reg };
        self.scopes[scope].vars.insert(var, data);
        &self.scopes[scope].vars[&var]
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

    pub fn get_loop(&self, scope: ScopeID) -> Option<(BlockID, Register)> {
        match self.scopes[scope].typ {
            ScopeType::Loop(v, r) => Some((v, r)),
            _ => match self.scopes[scope].parent {
                Some(s) => self.get_loop(s),
                None => None,
            },
        }
    }

    pub fn do_let(
        &mut self,
        pat: &LetPatternNode,
        reg: Register,
        builder: &mut CodeBuilder,
        scope: ScopeID,
    ) -> CompileResult<()> {
        match &pat.typ {
            LetPatternType::Var(v) => {
                let var = self.new_var(*v, pat.span, scope, builder);
                builder.push_raw_opcode(Opcode::CopyDeep(reg, var.reg), pat.span);
            }
            LetPatternType::ArrayDestructure(pats) => {
                let regs = (0..pats.len()).map(|_| builder.next_reg()).collect_vec();
                builder.push_raw_opcode(
                    Opcode::UnwrapArray {
                        v: reg,
                        start: regs[0],
                        len: pats.len() as u16,
                    },
                    pat.span,
                );
                for (i, pat) in pats.iter().enumerate() {
                    self.do_let(pat, regs[i], builder, scope)?
                }
            }
            LetPatternType::TupleDestructure(pats) => {
                let regs = (0..pats.len()).map(|_| builder.next_reg()).collect_vec();
                builder.push_raw_opcode(
                    Opcode::UnwrapTuple {
                        v: reg,
                        start: regs[0],
                        len: pats.len() as u16,
                    },
                    pat.span,
                );
                for (i, pat) in pats.iter().enumerate() {
                    self.do_let(pat, regs[i], builder, scope)?
                }
            }
        }
        Ok(())
    }

    pub fn do_assign(
        &mut self,
        pat: &AssignPatternNode,
        reg: Register,
        builder: &mut CodeBuilder,
        scope: ScopeID,
    ) -> CompileResult<()> {
        match &pat.typ {
            AssignPatternType::Path { var, path } => match self.get_var(var, scope) {
                Some(var) => {
                    // builder.push_raw_opcode(Opcode::SetVar(v.id), pat.span);
                    builder.push_raw_opcode(Opcode::CopyDeep(reg, var.reg), pat.span);
                }
                None => {
                    return Err(CompilerError::NonexistentVariable(
                        self.resolve(var).into(),
                        self.make_area(pat.span),
                    ))
                }
            },
            AssignPatternType::ArrayDestructure(pats) => {
                let regs = (0..pats.len()).map(|_| builder.next_reg()).collect_vec();
                builder.push_raw_opcode(
                    Opcode::UnwrapArray {
                        v: reg,
                        start: regs[0],
                        len: pats.len() as u16,
                    },
                    pat.span,
                );
                for (i, pat) in pats.iter().enumerate() {
                    self.do_assign(pat, regs[i], builder, scope)?
                }
            }
            AssignPatternType::TupleDestructure(pats) => {
                let regs = (0..pats.len()).map(|_| builder.next_reg()).collect_vec();
                builder.push_raw_opcode(
                    Opcode::UnwrapTuple {
                        v: reg,
                        start: regs[0],
                        len: pats.len() as u16,
                    },
                    pat.span,
                );
                for (i, pat) in pats.iter().enumerate() {
                    self.do_assign(pat, regs[i], builder, scope)?
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
    ) -> CompileResult<Register> {
        Ok(match &expr.typ {
            ExprType::Int(v) => {
                let out = builder.next_reg();
                builder.load_const(Constant::Int(*v), out, expr.span);
                out
            }
            ExprType::Float(v) => {
                let out = builder.next_reg();
                builder.load_const(Constant::Float(*v), out, expr.span);
                out
            }
            ExprType::String(v) => {
                let out = builder.next_reg();
                builder.load_const(Constant::String(self.resolve(v).into()), out, expr.span);
                out
            }
            ExprType::Bool(v) => {
                let out = builder.next_reg();
                builder.load_const(Constant::Bool(*v), out, expr.span);
                out
            }
            ExprType::Var(name) => match self.get_var(name, scope) {
                Some(v) => v.reg,
                None => {
                    return Err(CompilerError::NonexistentVariable(
                        self.resolve(name).into(),
                        self.make_area(expr.span),
                    ))
                }
            },
            ExprType::Unary(op, v) => {
                let to = builder.next_reg();
                let v = self.compile_expr(v, builder, scope)?;
                match op {
                    UnaryOp::Not => builder.push_raw_opcode(Opcode::UnaryNot { v, to }, expr.span),
                    UnaryOp::Minus => {
                        builder.push_raw_opcode(Opcode::UnaryMinus { v, to }, expr.span)
                    }
                }
                to
            }
            ExprType::Op(a, op, b) => {
                let to = builder.next_reg();
                let a = self.compile_expr(a, builder, scope)?;
                let b = self.compile_expr(b, builder, scope)?;
                match op {
                    BinOp::Plus => builder.push_raw_opcode(Opcode::Plus { a, b, to }, expr.span),
                    BinOp::Minus => builder.push_raw_opcode(Opcode::Minus { a, b, to }, expr.span),
                    BinOp::Mult => builder.push_raw_opcode(Opcode::Mult { a, b, to }, expr.span),
                    BinOp::Div => builder.push_raw_opcode(Opcode::Div { a, b, to }, expr.span),
                    BinOp::Mod => builder.push_raw_opcode(Opcode::Modulo { a, b, to }, expr.span),
                    BinOp::Pow => builder.push_raw_opcode(Opcode::Pow { a, b, to }, expr.span),
                    BinOp::Eq => builder.push_raw_opcode(Opcode::Eq { a, b, to }, expr.span),
                    BinOp::NotEq => builder.push_raw_opcode(Opcode::NotEq { a, b, to }, expr.span),
                    BinOp::Gt => builder.push_raw_opcode(Opcode::Gt { a, b, to }, expr.span),
                    BinOp::Gte => builder.push_raw_opcode(Opcode::Gte { a, b, to }, expr.span),
                    BinOp::Lt => builder.push_raw_opcode(Opcode::Lt { a, b, to }, expr.span),
                    BinOp::Lte => builder.push_raw_opcode(Opcode::Lte { a, b, to }, expr.span),
                }
                to
            }
            ExprType::Index { base, index } => todo!(),
            ExprType::Member { base, member } => todo!(),
            ExprType::Associated { base, member } => todo!(),
            ExprType::Call { base, args } => {
                todo!()
                // self.compile_expr(base, builder, scope)?;
                // for i in args {
                //     self.compile_expr(i, builder, scope)?;
                // }
                // builder.push_raw_opcode(Opcode::Call(args.len() as u16), expr.span);
            }
            ExprType::Array(v) => {
                let out = builder.next_reg();
                builder.push_raw_opcode(Opcode::CreateArray(out, v.len() as u16), expr.span);
                for i in v {
                    let r = self.compile_expr(i, builder, scope)?;
                    builder.push_raw_opcode(Opcode::PushElem { from: r, arr: out }, expr.span);
                }
                out
            }
            ExprType::Tuple(v) => {
                let out = builder.next_reg();
                builder.push_raw_opcode(Opcode::CreateTuple(out, v.len() as u16), expr.span);
                for i in v {
                    let r = self.compile_expr(i, builder, scope)?;
                    builder.push_raw_opcode(Opcode::PushElem { from: r, arr: out }, expr.span);
                }
                out
            }
            ExprType::Block(v) => {
                let derived = self.derive_scope(scope, ScopeType::Normal);
                self.compile_stmts(v, builder, derived, expr.span)?
            }
            ExprType::If {
                cond,
                then,
                otherwise,
            } => {
                let out = builder.next_reg();

                builder.new_block(|builder| {
                    let outer = builder.block;

                    builder.new_block(|builder| {
                        let derived = self.derive_scope(scope, ScopeType::Normal);

                        let cond = self.compile_expr(cond, builder, derived)?;
                        builder.push_jump(None, JumpType::EndIfFalse(cond), expr.span);

                        let r = self.compile_expr(then, builder, derived)?;
                        builder.push_raw_opcode(Opcode::CopyShallow(r, out), expr.span);
                        builder.push_jump(Some(outer), JumpType::End, expr.span);
                        Ok(())
                    })?;
                    if let Some(s) = otherwise {
                        let derived = self.derive_scope(scope, ScopeType::Normal);

                        let r = self.compile_expr(s, builder, derived)?;
                        builder.push_raw_opcode(Opcode::CopyShallow(r, out), expr.span);
                    } else {
                        builder.push_raw_opcode(Opcode::LoadUnit(out), expr.span);
                    }

                    Ok(())
                })?;

                out
            }
            ExprType::While { cond, body } => {
                let out = builder.next_reg();
                builder.push_raw_opcode(Opcode::LoadUnit(out), expr.span);

                builder.new_block(|builder| {
                    let derived = self.derive_scope(scope, ScopeType::Loop(builder.block, out));

                    let cond = self.compile_expr(cond, builder, derived)?;
                    builder.push_jump(None, JumpType::EndIfFalse(cond), expr.span);

                    let r = self.compile_expr(body, builder, derived)?;
                    builder.push_raw_opcode(Opcode::CopyShallow(r, out), expr.span);
                    builder.push_jump(None, JumpType::Start, expr.span);
                    Ok(())
                })?;

                out
            }
            ExprType::Function {
                params,
                ret_type,
                body,
            } => {
                todo!()
                // let mut captured_names = AHashMap::new();
                // self.for_accessible_vars(scope, &mut |name, data| {
                //     captured_names.insert(name, data);
                // });

                // let id = builder.new_func(
                //     |builder| {
                //         let func_scope = self.scopes.insert(Scope {
                //             vars: AHashMap::new(),
                //             parent: None,
                //             typ: ScopeType::FuncBody,
                //         });
                //         let mut captured_map = vec![];

                //         for (name, data) in captured_names {
                //             let new_id = self.new_var(name, data.def_span, func_scope, builder);
                //             captured_map.push((data.id, new_id));
                //         }

                //         for (pat, _) in params.iter().rev() {
                //             self.do_let(pat, builder, func_scope)?;
                //         }

                //         self.compile_expr(body, builder, func_scope)?;

                //         Ok(captured_map)
                //     },
                //     expr.span,
                // )?;
                // builder.push_raw_opcode(Opcode::PushFunc(id), expr.span);
                // builder.push_raw_opcode(Opcode::SetArgAmount(params.len() as u16), expr.span);
                // for (idx, (_, t)) in params.iter().enumerate() {
                //     if let Some(t) = t {
                //         self.compile_expr(t, builder, scope)?;
                //         builder.push_raw_opcode(Opcode::SetArgType(idx as u16), t.span);
                //     }
                // }
                // if let Some(t) = ret_type {
                //     self.compile_expr(t, builder, scope)?;
                //     builder.push_raw_opcode(Opcode::SetReturnType, t.span);
                // }
            }
            ExprType::Dbg(v) => {
                let out = self.compile_expr(v, builder, scope)?;
                builder.push_raw_opcode(Opcode::Dbg(out), expr.span);
                out
            }
            ExprType::Return(_) => todo!(),
            ExprType::Break(v) => {
                if let Some((block, reg)) = self.get_loop(scope) {
                    if let Some(v) = v {
                        let r = self.compile_expr(v, builder, scope)?;
                        builder.push_raw_opcode(Opcode::CopyShallow(r, reg), expr.span);
                    } else {
                        builder.push_raw_opcode(Opcode::LoadUnit(reg), expr.span);
                    }

                    builder.push_jump(Some(block), JumpType::End, expr.span);
                } else {
                    return Err(CompilerError::BreakOutsideLoop(self.make_area(expr.span)));
                }

                builder.next_reg()
            }
            ExprType::Continue(v) => {
                if let Some((block, reg)) = self.get_loop(scope) {
                    if let Some(v) = v {
                        let r = self.compile_expr(v, builder, scope)?;
                        builder.push_raw_opcode(Opcode::CopyShallow(r, reg), expr.span);
                    } else {
                        builder.push_raw_opcode(Opcode::LoadUnit(reg), expr.span);
                    }

                    builder.push_jump(Some(block), JumpType::Start, expr.span);
                } else {
                    return Err(CompilerError::BreakOutsideLoop(self.make_area(expr.span)));
                }

                builder.next_reg()
            }
        })
    }
    pub fn compile_stmt(
        &mut self,
        stmt: &StmtNode,
        builder: &mut CodeBuilder,
        scope: ScopeID,
    ) -> CompileResult<Option<Register>> {
        match &stmt.typ {
            StmtType::Expr(v) => {
                let r = self.compile_expr(v, builder, scope)?;
                return Ok(Some(r));
            }
            StmtType::Let(pat, v) => {
                let b1 = builder.new_block(|_| Ok(()))?;
                let b2 = builder.new_block(|_| Ok(()))?;

                let middle = builder.next_reg();

                builder.in_block(b2, |builder| {
                    self.do_let(pat, middle, builder, scope)?;
                    Ok(())
                })?;
                builder.in_block(b1, |builder| {
                    let r = self.compile_expr(v, builder, scope)?;
                    builder.push_raw_opcode(Opcode::CopyShallow(r, middle), v.span);
                    Ok(())
                })?;
            }
            StmtType::Assign(pat, v) => {
                let r = self.compile_expr(v, builder, scope)?;
                self.do_assign(pat, r, builder, scope)?;
            }
            StmtType::AssignOp(_, _, _) => todo!(),
        }

        Ok(None)
    }
    pub fn compile_stmts(
        &mut self,
        stmts: &Stmts,
        builder: &mut CodeBuilder,
        scope: ScopeID,
        span: CodeSpan,
    ) -> CompileResult<Register> {
        let (v, r) = match stmts {
            Stmts::Normal(v) => (v, None),
            Stmts::Ret(v, r) => (v, Some(r)),
        };

        for i in v {
            self.compile_stmt(i, builder, scope)?;
        }
        let ret = if let Some(r) = r {
            self.compile_stmt(r, builder, scope)?
        } else {
            None
        };

        Ok(if let Some(r) = ret {
            r
        } else {
            let out = builder.next_reg();
            builder.push_raw_opcode(Opcode::LoadUnit(out), span);
            out
        })
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
