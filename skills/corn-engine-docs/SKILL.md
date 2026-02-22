---
name: corn-engine-docs
description: Explain and apply Corn runtime semantics, invariants, benchmark expectations, and pain-point boundaries for this repository. Use when changing `Engine.Data.Program` or `Engine.Data.ECS`, reviewing scheduler/kernel/query behavior, deciding whether a claim is a hard guarantee vs expectation, or rewriting Corn guidance/docs.
---

# Corn Engine Docs

## Workflow

1. Read `references/first-principles.md` for the engine model and non-goals.
2. For game-logic timing questions, read `references/time-function-concepts.md`.
3. Read `references/invariants.md` for hard behavioral/performance contracts.
4. Read `references/expectations-good-bad.md` when clarifying pain points or limits.
5. Classify each claim as `invariant`, `expectation`, or `non-goal`.
6. Anchor claims to concrete code paths before proposing changes.

## Classification Rules

- Treat `invariant` as enforced behavior that should be preserved or asserted.
- Treat `expectation` as guidance with known failure modes; provide one good example and one bad example.
- Treat `non-goal` as intentionally unsupported behavior; name the required external mechanism.
- Distinguish snapshot serialization from replay-based restore.
- Distinguish query expressiveness (`Monad`/`Alternative`) from pruning/perf behavior.

## Output Contract

- Write AI-first guidance: concise, actionable, and optimized for another agent to execute.
- Keep answers concise and code-anchored.
- Include concrete examples for behavior questions.
- Encode new expectation clarifications in `references/expectations-good-bad.md`.
- Keep benchmark claims aligned with repository focus:
  - Primary: `program/10k/eachm`
  - Secondary: `program/10k+1/eachm`

## Reference Map

- `references/first-principles.md`: model, runtime semantics, and non-goals.
- `references/time-function-concepts.md`: mapping from FRP time primitives to common game concepts and pitfalls.
- `references/invariants.md`: explicit runtime/ECS/benchmark invariants.
- `references/expectations-good-bad.md`: expectation boundaries with good/bad examples.
