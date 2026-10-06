//! Projection inlining.
//!
//! A projection call opens a session: the callee's ramp runs at the call and
//! grants its yielded place to the caller, and the callee's slide runs where
//! the caller ends the session. Every projection call is inlined into its
//! caller: the ramp replaces the call, and each `End` of the session runs its
//! own copy of the slide, dispatched on the yield site the ramp took when the
//! callee yields at more than one. Callee bodies are inlined first, so the
//! sessions they open are already resolved, and recursion through
//! projections is rejected before lowering.

use cranelift_entity::EntityRef;
use hir::analysis::{semantic::SemOrigin, ty::ty_def::TyId};

use crate::{
    db::MirDb,
    instance::runtime::runtime_instance_lowered_body,
    runtime::{
        LowerError, PlaceRoot, RBlock, RBlockId, RExpr, RLocal, RLocalId, RStmt, RTerminator,
        RuntimeBody, RuntimeCarrier, RuntimeClass, RuntimeLocalRoot, RuntimeProviderBindingId,
        ScalarClass, ScalarRepr, ScalarRole, synthetic::uint_scalar,
    },
};

pub(crate) fn inline_projections<'db>(
    db: &'db dyn MirDb,
    body: &mut RuntimeBody<'db>,
) -> Result<(), LowerError> {
    while let Some((block, stmt)) = find_stmt(body, |stmt| {
        matches!(
            stmt,
            RStmt::Assign { expr: RExpr::Call { callee, .. }, .. }
                if callee.key(db).semantic(db).is_some_and(|semantic| semantic.is_projection(db))
        )
    }) {
        let RStmt::Assign {
            dst,
            expr: RExpr::Call { callee, args },
        } = body.blocks[block.index()].stmts[stmt].clone()
        else {
            unreachable!("found a projection call")
        };
        let callee = runtime_instance_lowered_body(db, callee)?.body(db);
        inline_session(db, body, block, stmt, dst, &args, &callee);
    }
    prune_unreachable_blocks(body);
    Ok(())
}

/// Replaces the call at `stmt` of `block`, which assigns the session `dst`,
/// with the callee's ramp, and each `End` of the session with its slide.
fn inline_session<'db>(
    db: &'db dyn MirDb,
    body: &mut RuntimeBody<'db>,
    block: RBlockId,
    stmt: usize,
    dst: RLocalId,
    args: &[RLocalId],
    callee: &RuntimeBody<'db>,
) {
    let splice = Splice {
        callee,
        locals: body.locals.len() as u32,
        bindings: body.provider_bindings.len() as u32,
        origin: body.stmt_origins[block.index()][stmt],
    };
    body.locals.extend(callee.locals.iter().cloned());
    body.provider_bindings
        .extend(callee.provider_bindings.iter().cloned().map(|mut binding| {
            binding.value = splice.local(binding.value);
            binding
        }));
    let yields: Vec<RBlockId> = callee
        .blocks
        .iter()
        .filter_map(|block| match block.terminator {
            RTerminator::Yield { resume, .. } => Some(resume),
            _ => None,
        })
        .collect();
    let site_class = RuntimeClass::Scalar(ScalarClass {
        repr: ScalarRepr::Int {
            bits: 32,
            signed: false,
        },
        role: ScalarRole::Plain,
    });
    let site = (yields.len() > 1).then(|| {
        body.locals.push(RLocal {
            semantic_ty: TyId::u256(db),
            carrier: RuntimeCarrier::Value(site_class),
            root: RuntimeLocalRoot::None,
        });
        RLocalId::new(body.locals.len() - 1)
    });

    // The ramp runs in place of the call and continues after it.
    let after_call = split_block(body, block, stmt);
    // With several yield sites, every slide is reachable from every yield:
    // the locals a slide uses need a definition on each path, though the
    // dispatch only enters the slide of the site the ramp took.
    if site.is_some() {
        for (local, class) in slide_locals(callee, &yields) {
            push_stmt(
                body,
                block,
                splice.origin,
                RStmt::Assign {
                    dst: splice.local(local),
                    expr: RExpr::Placeholder { class },
                },
            );
        }
    }
    assert_eq!(args.len(), callee.signature.params.len());
    for (param, arg) in callee.signature.params.iter().zip(args) {
        push_stmt(
            body,
            block,
            splice.origin,
            RStmt::Assign {
                dst: splice.local(param.local),
                expr: RExpr::Use(*arg),
            },
        );
    }
    let mut site_index = 0;
    let ramp = splice.copy(body, |stmts, terminator| match terminator {
        RTerminator::Yield { value, .. } => {
            if let Some(value) = value {
                stmts.push(RStmt::Assign {
                    dst,
                    expr: RExpr::Use(*value),
                });
            }
            if let Some(site) = site {
                stmts.push(RStmt::Assign {
                    dst: site,
                    expr: RExpr::ConstScalar(uint_scalar(32, site_index)),
                });
            }
            site_index += 1;
            RTerminator::Goto(after_call)
        }
        _ => RTerminator::Trap,
    });
    body.blocks[block.index()].terminator = RTerminator::Goto(ramp[0]);

    // Each `End` runs its own copy of the slide, which continues after it.
    while let Some((block, stmt)) = find_stmt(
        body,
        |stmt| matches!(stmt, RStmt::End { session } if *session == dst),
    ) {
        let after_end = split_block(body, block, stmt);
        let slide = splice.copy(body, |_, terminator| match terminator {
            RTerminator::Return(_) => RTerminator::Goto(after_end),
            _ => RTerminator::Trap,
        });
        let mut resumes = yields.iter().map(|resume| slide[resume.index()]);
        let dispatch = match site {
            Some(site) => {
                let default = resumes.next_back().expect("a projection yields");
                RTerminator::SwitchScalar {
                    discr: site,
                    cases: resumes
                        .enumerate()
                        .map(|(index, resume)| (uint_scalar(32, index as u64), resume))
                        .collect(),
                    default,
                }
            }
            None => RTerminator::Goto(resumes.next().expect("a projection yields")),
        };
        body.blocks[block.index()].terminator = dispatch;
    }
}

/// The SSA locals of `callee` its slides mention, with their classes.
fn slide_locals<'db>(
    callee: &RuntimeBody<'db>,
    resumes: &[RBlockId],
) -> Vec<(RLocalId, RuntimeClass<'db>)> {
    let mut seen = vec![false; callee.blocks.len()];
    let mut stack = resumes.to_vec();
    let mut locals = std::collections::BTreeSet::new();
    while let Some(block) = stack.pop() {
        if std::mem::replace(&mut seen[block.index()], true) {
            continue;
        }
        let mut data = callee.blocks[block.index()].clone();
        for stmt in &mut data.stmts {
            locals.extend(stmt.locals_mut().into_iter().map(|local| *local));
        }
        locals.extend(data.terminator.values_mut().into_iter().map(|value| *value));
        stack.extend(data.terminator.successors());
    }
    locals
        .into_iter()
        .filter_map(|local| {
            let data = &callee.locals[local.index()];
            match (&data.carrier, &data.root) {
                (RuntimeCarrier::Value(class), root)
                    if !matches!(root, RuntimeLocalRoot::Slot(_)) =>
                {
                    Some((local, class.clone()))
                }
                _ => None,
            }
        })
        .collect()
}

/// A callee body being copied into a caller.
struct Splice<'a, 'db> {
    callee: &'a RuntimeBody<'db>,
    /// The caller local of the callee's first local.
    locals: u32,
    /// The caller provider binding of the callee's first binding.
    bindings: u32,
    /// The call site, which every copied statement is attributed to.
    origin: SemOrigin<'db>,
}

impl<'db> Splice<'_, 'db> {
    fn local(&self, local: RLocalId) -> RLocalId {
        RLocalId::from_u32(self.locals + local.as_u32())
    }

    /// Appends a copy of the callee's blocks to `body`, with `exit` choosing
    /// what each `Yield` and `Return` becomes, and returns the copies by
    /// callee block.
    fn copy(
        &self,
        body: &mut RuntimeBody<'db>,
        mut exit: impl FnMut(&mut Vec<RStmt<'db>>, &RTerminator<'db>) -> RTerminator<'db>,
    ) -> Vec<RBlockId> {
        let blocks: Vec<_> = (body.blocks.len()..)
            .take(self.callee.blocks.len())
            .map(RBlockId::new)
            .collect();
        for block in &self.callee.blocks {
            let mut stmts: Vec<_> = block
                .stmts
                .iter()
                .cloned()
                .map(|mut stmt| {
                    for local in stmt.locals_mut() {
                        *local = self.local(*local);
                    }
                    if let Some(place) = stmt.place_mut()
                        && let PlaceRoot::Provider(binding) = &mut place.root
                    {
                        *binding =
                            RuntimeProviderBindingId::from_u32(self.bindings + binding.as_u32());
                    }
                    stmt
                })
                .collect();
            let mut terminator = block.terminator.clone();
            for value in terminator.values_mut() {
                *value = self.local(*value);
            }
            let terminator = match terminator {
                RTerminator::Yield { .. } | RTerminator::Return(_) => exit(&mut stmts, &terminator),
                mut terminator => {
                    for successor in terminator.successors_mut() {
                        *successor = blocks[successor.index()];
                    }
                    terminator
                }
            };
            body.stmt_origins.push(vec![self.origin; stmts.len()]);
            body.terminator_origins.push(self.origin);
            body.blocks.push(RBlock { stmts, terminator });
        }
        blocks
    }
}

fn find_stmt(
    body: &RuntimeBody<'_>,
    mut pred: impl FnMut(&RStmt<'_>) -> bool,
) -> Option<(RBlockId, usize)> {
    body.blocks.iter().enumerate().find_map(|(block, data)| {
        let stmt = data.stmts.iter().position(&mut pred)?;
        Some((RBlockId::new(block), stmt))
    })
}

/// Removes statement `stmt` of `block`, and moves the statements after it and
/// the block's terminator into a new block, which it returns.
fn split_block(body: &mut RuntimeBody<'_>, block: RBlockId, stmt: usize) -> RBlockId {
    let index = block.index();
    let stmts = body.blocks[index].stmts.split_off(stmt + 1);
    let origins = body.stmt_origins[index].split_off(stmt + 1);
    body.blocks[index].stmts.pop();
    body.stmt_origins[index].pop();
    let terminator = std::mem::replace(&mut body.blocks[index].terminator, RTerminator::Trap);
    body.stmt_origins.push(origins);
    body.terminator_origins.push(body.terminator_origins[index]);
    body.blocks.push(RBlock { stmts, terminator });
    RBlockId::new(body.blocks.len() - 1)
}

fn push_stmt<'db>(
    body: &mut RuntimeBody<'db>,
    block: RBlockId,
    origin: SemOrigin<'db>,
    stmt: RStmt<'db>,
) {
    body.blocks[block.index()].stmts.push(stmt);
    body.stmt_origins[block.index()].push(origin);
}

/// Drops the blocks the entry cannot reach: copies of ramps and slides
/// include each other's blocks.
fn prune_unreachable_blocks(body: &mut RuntimeBody<'_>) {
    fn reachable_only<T>(items: Vec<T>, reachable: &[bool]) -> Vec<T> {
        items
            .into_iter()
            .zip(reachable)
            .filter_map(|(item, reachable)| reachable.then_some(item))
            .collect()
    }
    let mut reachable = vec![false; body.blocks.len()];
    let mut stack = vec![RBlockId::new(0)];
    while let Some(block) = stack.pop() {
        if !std::mem::replace(&mut reachable[block.index()], true) {
            stack.extend(body.blocks[block.index()].terminator.successors());
        }
    }
    let renamed: Vec<_> = reachable
        .iter()
        .scan(0, |next, reachable| {
            let block = RBlockId::new(*next);
            *next += usize::from(*reachable);
            Some(block)
        })
        .collect();
    body.blocks = reachable_only(std::mem::take(&mut body.blocks), &reachable);
    for block in &mut body.blocks {
        for successor in block.terminator.successors_mut() {
            *successor = renamed[successor.index()];
        }
    }
    body.stmt_origins = reachable_only(std::mem::take(&mut body.stmt_origins), &reachable);
    body.terminator_origins =
        reachable_only(std::mem::take(&mut body.terminator_origins), &reachable);
}
