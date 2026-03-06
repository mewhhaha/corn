# Modeling Pain Points Log

Use this log to capture places where a concept was hard to express cleanly in `corn`.

## 2026-02-22

1. `await` + `pass` inference in standalone example modules still needs explicit `@C @()` in a few spots.
   - Symptom: after the kernel-first `Engine.Corn` wrapper rewrite, `S.await $ S.pass $ ...` can leave `msg` floating when the batch itself does not mention messages.
   - Workaround used: explicit `S.pass @C @()` and `S.eachM @Row @C @()` in the example modules.
   - Status: current; the examples compile, but the wrapper can still be tightened further if we want those annotations gone again.

2. Tuning concept behavior to stay informative without collapsing into trivial outcomes took iteration.
   - Symptom: initial horde simulation ended with `playerHp=0` and `aliveMobs=0`, which was a poor concept signal.
   - Adjustment used: reduced contact damage pressure, raised mob HP, lowered self-attrition range, and shortened simulation length.

3. Under GHC2024 in this repo/toolchain, `foldl'` from Prelude made `Data.List (foldl')` imports redundant.
   - Symptom: `-Wall` emitted redundant-import warnings in new example modules.
   - Adjustment used: remove explicit `Data.List` imports and rely on Prelude-provided `foldl'`.

4. Earlier `Engine.Data.Program` inference fixes do not fully transfer through the new `Engine.Corn` wrapper.
   - Symptom: the lower-level package can still infer more than the public wrapper in some example-only call sites.
   - Adjustment used: keep the examples explicit at the `pass` boundary instead of hiding the current public API cost.
   - Status: current; acceptable for examples, but still a real ergonomics gap.
