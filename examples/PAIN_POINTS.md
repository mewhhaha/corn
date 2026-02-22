# Modeling Pain Points Log

Use this log to capture places where a concept was hard to express cleanly in `corn`.

## 2026-02-22

1. ~~`await` + `Batch` type inference in example modules was ambiguous for `msg`/`c` in multiple spots.~~
   - Symptom: GHC errors around `S.await`/`S.eachM` with unresolved `Typeable msg`.
   - Workaround used: explicit type applications (`@C @()`) for `eachM` and explicit `S.Batch C () ...` annotations for `collect`.
   - Status: resolved by `AwaitableM`/`Batch` inference refactor in `Engine.Data.Program`.

2. Tuning concept behavior to stay informative without collapsing into trivial outcomes took iteration.
   - Symptom: initial horde simulation ended with `playerHp=0` and `aliveMobs=0`, which was a poor concept signal.
   - Adjustment used: reduced contact damage pressure, raised mob HP, lowered self-attrition range, and shortened simulation length.

3. Under GHC2024 in this repo/toolchain, `foldl'` from Prelude made `Data.List (foldl')` imports redundant.
   - Symptom: `-Wall` emitted redundant-import warnings in new example modules.
   - Adjustment used: remove explicit `Data.List` imports and rely on Prelude-provided `foldl'`.

4. ~~Generic `await` inference needed stronger linkage between `ProgramM` and `Batch` parameters.~~
   - Symptom: even with functional dependencies, `S.await $ S.eachM ...` and `S.await $ S.collect ...` still left `c/msg` ambiguous in example programs.
   - Adjustment used: in `Engine.Data.Program`, make `AwaitableM` carry `m -> c msg`, then use an equality-constrained batch instance:
     `instance (c' ~ c, msg' ~ msg) => AwaitableM c msg (ProgramM c msg) (Batch c' msg' a) a`.
   - Status: resolved; examples now compile without explicit `Batch C ()` or `@C @()` disambiguation.
