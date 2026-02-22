# Corn Invariants

This file records domain and runtime invariants that the engine relies on.
The goal is to make simplification and performance work safer: if an invariant
is explicit, we can remove defensive code with confidence and catch regressions
earlier.

## Program Runtime Invariants

1. Program slot and run-flag alignment:
- Scope: `Engine.Data.Program.stepRound` and `updateRunFlags`.
- Invariant: `toRun` provides at least one run flag for each `ProgramSlot`, in the same order (extra trailing flags are allowed).
- Why it matters: missing flags can silently truncate scheduling.

2. Await snapshot semantics per round:
- Scope: `Inbox` construction in `runProgramsPhase`.
- Invariant: awaits are resolved against the round snapshot (`done`, `seen`, `values`) captured at phase entry, not partially updated state from later siblings in the same traversal.
- Why it matters: deterministic and stable await behavior.

3. Batch wait scheduling contract:
- Scope: `ProgAwait (BatchWait b)` handling.
- Invariant: a batch wait enqueues exactly one `PendingBatch` and sets run flag to `False` until batch completion.
- Why it matters: keeps one fused batch boundary per await and avoids double-enqueue.

4. Global step world execution:
- Scope: `runBatchesPhase`.
- Invariant: `stepWorld` is run at most once per round, only when needed (`worldHasSteps`) and before batch kernel execution.
- Why it matters: avoids duplicate work and preserves frame semantics.

5. Batch compile shape:
- Scope: `compilePending`.
- Invariant: for `BatchRun ops n k`, `n` must equal `V.length ops`.
- Why it matters: finalize relies on exact step cardinality.

6. Kernel determinism:
- Scope: `runKernelRowsStateful`, `runKernelRowsStateless`, and chunk merge.
- Invariant: step order and row order are stable and deterministic.
- Why it matters: reproducible behavior and benchmark comparability.

7. Parallel chunk shape:
- Scope: `runPendingStepsPar`.
- Invariant: when parallel mode is chosen (`statefulChunkCount > 1`), chunk vectors are non-empty and aligned (`rowChunks` and `stepChunks` have equal length), and concatenated output row count equals input row count.
- Why it matters: safe parallel execution without shape drift.

8. Finalization exactness:
- Scope: `finalizePendingState`.
- Invariant: all compiled steps are consumed exactly once, in order; program IDs in finalized updates are unique.
- Why it matters: prevents state corruption and duplicate update application.

## ECS Invariants

1. Row/location coherence:
- Scope: world row storage plus entity-location index.
- Invariant: every live entity ID maps to one valid row index and row/entity identity stays coherent through spawn/kill updates.
- Why it matters: all lookups and edits assume this mapping is authoritative.

2. Bag mask/value coherence:
- Scope: `Bag` operations (`bagGetByTyped`, `bagSetByAny`, `bagDelByBit`).
- Invariant: `bagMask` bits exactly describe what exists in `bagValues`; `staticIndex` is valid for every set bit.
- Why it matters: unsafe indexing/coercion correctness and performance.

3. Query signature filter correctness:
- Scope: `matchSig`, `runQuerySig`, `runq`, `foldq`.
- Invariant: `EntityRow.sig` is a correct summary for required/forbidden bits, and `matchSig` must gate query execution.
- Why it matters: query soundness and hot-path performance.

4. Packed edit fast-path safety:
- Scope: `bagApplyEditPacked`.
- Invariant: non-structural fast path is only valid when the edit cannot change mask shape.
- Why it matters: avoids expensive rebuild while preserving bag correctness.

5. Step-store coherence:
- Scope: component add/remove and world stepping.
- Invariant: step-store entries track step-bearing components and are removed when components/entities are removed.
- Why it matters: correct global stepping and no stale state.

## Benchmark Invariants

1. Primary optimization target:
- `program/10k/eachm`.

2. Secondary regression check:
- `program/10k+1/eachm` (informational).

3. Comparison protocol:
- Use repeated medians, fixed benchmark flags, and include allocation plus elapsed.
- Never keep a change based on a single run or elapsed-only improvement with clear allocation regression.

## How To Use Invariants

1. Encode them:
- Prefer representing invariants in structure/types when cheap (for example, paired data instead of independently aligned lists).

2. Assert them:
- Add cheap fail-fast checks around boundaries where invariant violations are otherwise silent (for example, alignment and shape checks).
- Keep checks lightweight in hot paths; use debug-only checks if needed.

3. Test them:
- Add property tests for invariant families (shape preservation, mask/value coherence, deterministic order, exact finalization accounting).

4. Gate performance work with them:
- Treat invariants as acceptance criteria, not just documentation.
- If an optimization violates an invariant or increases allocation materially, revert quickly.

5. Use them to simplify code:
- Once an invariant is encoded or asserted, remove redundant branch fallback code that only handled impossible states.

## Immediate Practical Uses

1. Add a check in `stepRound` that there are no missing run flags before zipping.
2. Assert `BatchRun` declared count matches `ops` length in `compilePending`.
3. Assert chunk-shape invariants in `runPendingStepsPar` before merge.
4. Add property tests for bag mask/value coherence and query signature gating.
