# Corn Examples

Small genre concept sketches built with the `corn` engine.

## Run

```sh
cd examples
cabal run corn-examples -- all
```

Run one concept:

```sh
cd examples
cabal run corn-examples -- platformer
cabal run corn-examples -- horde-survival
cabal run corn-examples -- bullet-hell
```

## Concepts

- `platformer`: single-actor gravity + periodic jump impulse loop.
- `horde-survival`: moving player target with mob chase/attrition.
- `bullet-hell`: periodic radial bullet burst spawning + TTL cleanup.

## Modeling Notes

Pain points and modeling tradeoffs are tracked in `examples/PAIN_POINTS.md`.
