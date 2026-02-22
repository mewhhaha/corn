# Time Functions -> Game Concepts

Use this map when a user asks "how do I model X without manual `dt` math?"

## Fast Mapping

| Primitive | Game concept | Output | Use when | Common mistake |
| --- | --- | --- | --- | --- |
| `F.time` | Absolute timeline | `Time` | Global clocks, looping animation phase, deterministic scripted motion | Using it when you need "time since event" |
| `F.progress (t0,t1)` | Normalized timeline | `Double` in `[0,1]` (clamped) | Tween progress bars, lerp parameters, fixed-duration blends | Treating it like "inactive outside window" |
| `F.range (t0,t1)` | Active window progress | `Maybe Double` | Logic that should be absent outside the window | Forgetting `Nothing` handling |
| `F.window (t0,t1) step` | Lifetime gate | `Maybe a` | Bullets/effects/buffs with finite lifetime | Expecting clamped behavior instead of nullable behavior |
| `F.after t` | One-shot timer | `Events ()` | Delayed trigger once | Expecting repeated pulses |
| `F.every t` | Repeating timer | `Events ()` | Tick effects, periodic spawn, cooldown ticks | Using non-positive `t` and expecting events |
| `F.since ev` | Time since trigger | `Double` | Cooldowns, invulnerability age, hold duration | Using `F.time` and re-deriving resets manually |
| `F.forFrom ev t` | Active for duration after trigger | `Bool` | "Invulnerable for 1s after hit" | Rebuilding with ad-hoc bool flags |
| `F.progressFrom ev t` | Normalized progress after trigger | `Double` | Event-driven tween starts | Forgetting it resets on each trigger |
| `F.delay x0` | Previous-frame value | previous input | Velocity from position delta, edge smoothing | Using for long-term state instead of `acc`/`hold` |
| `F.hold a0` | Last event value latch | current value | Weapon mode/state from sparse events | Expecting history instead of latest-only |
| `F.tween span ease f` + `F.sample` | Declarative animation curve | sampled value | Eased motion/color/scale with compositional curves | Driving simulation truth from presentation tween |

## Selection Rules

1. If concept is "how far through a fixed duration?" use `progress`.
2. If concept is "only active inside duration" use `range` or `window`.
3. If concept is "starts when event happens" use `since` / `forFrom` / `progressFrom`.
4. If concept is "run every N seconds" use `every`, not `after`.
5. If concept is "animate value by curve" use `tween` + `sample`.

## Program-Side Pattern (No Manual dt)

```haskell
-- Example: event-driven dash blend without explicit dt math.
dashBlend :: F.Step Input Double
dashBlend = F.progressFrom (I.press dashButton) 0.2

dashProg :: S.ProgramM C Input ()
dashProg = do
  u <- S.step dashBlend input
  let x = lerp 0 6 u
  _ <- S.await $ S.each @Pos $ \_ -> S.set (Pos x)
  pure ()
```

## Pitfall Checklist

- `progress` is clamped, not nullable.
- `range`/`window` are nullable, not clamped.
- `since` and `progressFrom` reset whenever the trigger events are non-empty.
- `delay` is one-step memory, not an accumulator.
- Prefer `S.step`/`S.time`/`S.sample` for game logic and leave raw `S.dt` to low-level integration code.
