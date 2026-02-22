# Corn Expectations: Good/Bad Examples

Use this file to answer "is this guaranteed?" questions.
Classify each statement as:

- `Invariant`: hard contract enforced by implementation/tests/checks.
- `Expectation`: intended behavior that can fail if user programs violate assumptions.
- `Non-goal`: intentionally unsupported feature.

## 1) Frame Progress and Misbehaving Programs

Type: `Expectation`

Expectation:
- The frame loop should converge when programs stop making progress.

Good:
- A program waits for external input (`await (== Tick)`) and only resumes next frame when new input arrives.

Bad:
- A program emits an event and immediately waits on the same predicate in a self-sustaining loop.

```haskell
bad = let loop = do
            S.send [Ping]
            _ <- S.await (== Ping)
            loop
      in loop
```

Why:
- Non-batch blocked waits stay runnable; if output re-satisfies the wait, fixed-point convergence can fail.

## 2) Replay vs Snapshot Serialization

Type: `Non-goal` for snapshot serialization, `Expectation` for replay.

Expectation:
- Rebuild state by replaying logged inputs and `dt` from a known start graph.

Good:
- Persist initial world/graph seed plus per-frame inputs and replay with `run`.

Bad:
- Try to serialize/deserialize in-flight continuations or `Any`-typed locals directly.

Why:
- Runtime state stores closures and erased locals, not a portable value-only snapshot.

## 3) Query Expressiveness vs Query Pruning

Type: `Expectation`

Expectation:
- Applicative/product queries preserve signature pruning.
- `Monad`/`Alternative` composition can drop pruning and scan more rows.

Good:
- Use `E.query @RecordType` or `E.comp <*> E.comp` for predictable signature-gated scans.
- In hot loops, wrap queries with `E.requireQuerySigPruning "<label>"` to catch accidental non-pruned shapes.

Bad:
- Use nested `>>=` / `<|>` in hot loops and assume pruning is unchanged.

Why:
- Query info becomes `mempty` in monadic/alternative composition paths.

## 4) Sum Queries (`QueryableSum`)

Type: `Expectation`

Expectation:
- Sum queries are ergonomic but can skip signature pruning.

Good:
- Use `QueryableSum` where readability dominates and row count is small/moderate.
- Use `E.queryHasSigPruning` to confirm whether a query shape will prune before using it in hot paths.

Bad:
- Rely on sum queries for high-cardinality hot paths while expecting product-query pruning performance.

Why:
- Sum constructor composition uses `<|>` semantics and does not aggregate required/forbidden bits.

## 5) Event Ordering Semantics

Type: `Invariant` for list-order composition, `Non-goal` for priorities.

Expectation:
- Events are plain lists; ordering follows evaluation order.

Good:
- Treat event order as deterministic by program/batch evaluation order in one run.

Bad:
- Assume timestamps, priorities, or automatic fairness scheduling across producers.

Why:
- Event transport is list concatenation without metadata.

## 6) Step State Lifetime

Type: `Expectation`

Expectation:
- Step state is stored in program/entity locals and persists until explicitly dropped or entity/program state is removed.

Good:
- Design entity/program lifecycles so stale machine state is eventually deleted.

Bad:
- Assume automatic GC of dormant step/event state with no lifecycle boundaries.

Why:
- State persistence is keyed by callsite/type in locals/machines.

## 7) Patch Conflict Resolution

Type: `Invariant`

Expectation:
- Conflicts resolve by composition order only.

Good:
- Make update order explicit when composing patches and document precedence.

Bad:
- Assume CRDT-style merge resolution or conflict arbitration beyond order.

Why:
- Patch combination uses `Semigroup` composition semantics.

## 8) `progress` vs `range` vs `window`

Type: `Expectation`

Expectation:
- `progress (t0,t1)` clamps to `[0,1]` outside the span.
- `range (t0,t1)` returns `Nothing` outside and `Just u` only while inside.
- `window` gates another step and returns `Maybe`.

Good:
- Use `progress` for normalized timelines that should stay stable before/after the interval.
- Use `range`/`window` when outside-range should explicitly be `Nothing`.

Bad:
- Treat `progress` as a window signal and branch on it to detect "inactive" time.
- Treat `range` as clamped output and forget to handle `Nothing`.

Why:
- These APIs encode different inactive-time semantics and can silently change gameplay logic if swapped.

## 9) Event Wait Semantics (`await (msg -> Bool)`)

Type: `Expectation`

Expectation:
- Event waits return all matching events in the current inbox snapshot.

Good:
- Process returned events as a batch (`Events msg`) and write idempotent handlers.

Bad:
- Assume single-event consume semantics and only handle the first element.

Why:
- Matching is `filter` over inbox events, not queue-pop/consume.

## 10) `await Update` Is a Seen-Barrier, Not "All Finished"

Type: `Expectation`

Expectation:
- `await Update` resumes once all other programs have been seen in the round.

Good:
- Use `Update` as a per-round synchronization barrier for observation ordering.

Bad:
- Use `Update` expecting every other program to have fully finished all internal waits/batches.

Why:
- The check is against `seen` membership, not a global "done everything" state.

## 11) Batch Await Scope (`ProgramM` Only)

Type: `Invariant`

Expectation:
- `await <batch>` is valid in `ProgramM` and not available from `EntityM`.

Good:
- Run `await` on a batch at program scope, then apply per-entity edits inside that batch.

Bad:
- Call `await` on a batch from inside `eachM` entity code.

Why:
- Batch waits are exposed as a `ProgramM`-only operation, so invalid `EntityM` usage fails at compile time.

## 12) Step State Keying Is Callsite-Sensitive

Type: `Expectation`

Expectation:
- `step` state is keyed by step type and callsite fingerprint.

Good:
- Bind one `step` call once and reuse the value in the same tick when shared state is intended.

Bad:
- Duplicate `step` calls and assume they share one machine state automatically.

Why:
- Distinct callsites create distinct state slots, which can diverge unexpectedly.

## 13) Sticky Applicative Event Waits

Type: `Expectation`

Expectation:
- Use `await` with `sticky` leaves to accumulate multiple event outcomes across frames.

Good:
- Combine hover/focus extraction applicatively:
  - `(,) <$> sticky hoverMatch <*> sticky focusMatch`
  - `await` resumes once both sides have values.

Bad:
- Chain `await` calls and expect the second one to remember a value seen in an earlier frame.

Why:
- Plain predicate waits are sequential per gate; sticky applicative waits store partial matches until all branches are ready.

## Add New Cases

When adding a new expectation:

1. Add `Type`.
2. Add one `Good` and one `Bad` example.
3. Add a short `Why`.
4. Link function/module anchors in nearby docs or code review notes.
