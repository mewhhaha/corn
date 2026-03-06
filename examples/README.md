# Corn Examples

Small genre concept sketches built with the `corn` engine.

## Run

```sh
cd examples
cabal run corn-examples
```

The default run is a pure survivor-style simulation that shows:

- scripted player input
- deterministic enemy spawning
- different enemy behaviors
- auto-attacks and unlockable aura damage
- XP, levels, and upgrade picks applied from pure inputs

List all available examples:

```sh
cd examples
cabal run corn-examples -- list
```

Run the full set:

```sh
cd examples
cabal run corn-examples -- all
```

Run one example:

```sh
cd examples
cabal run corn-examples -- survivor-sim
cabal run corn-examples -- platformer
cabal run corn-examples -- horde-survival
cabal run corn-examples -- bullet-hell
cabal run corn-examples -- scenes-navigation
```

## Examples

- `survivor-sim`: pure vampire-survivor-style simulation with scripted inputs, upgrades, enemy behaviors, and deterministic output snapshots.
- `platformer`: single-actor gravity + periodic jump impulse loop.
- `horde-survival`: moving player target with mob chase/attrition.
- `bullet-hell`: periodic radial bullet burst spawning + TTL cleanup.
- `scenes-navigation`: `Engine.Corn.App` tree example (`Route.create`, `Route.step`, `Route.navigate`)
  with nested layout stacks and route-local navigation output.

## Modeling Notes

Pain points and modeling tradeoffs are tracked in `examples/PAIN_POINTS.md`.
