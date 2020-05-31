# elm16

A Chip16 emulator written in Elm. Final project for cs223.

## The good
- It works, mostly.

## The bad
- No sound.

## The ugly
- Elm forbids n-tuples where n >= 3. ????
- Immutable data structures make performance a real challenge.
    - Every load, move, store, incurs an O(n) operation on RAM size.
    - Every draw incurs an O(m²) operation on screen size.
    - Alternatives: batch RAM updates (use a Heap?).

## On Elm
- Type classes and real parametric polymorphism would have been useful for the arithmetic library