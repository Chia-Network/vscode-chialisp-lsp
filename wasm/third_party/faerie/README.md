This is faerie 0.16.0 (MIT), with its object-file dependencies moved off
`indexmap` 1 and `scroll` 0.10. Those releases pulled `syn` 1.0.109 and kept
`indexmap` 1.9.3, both of which run build scripts. `scroll` 0.12 uses `syn` 2,
which has no build script, and `indexmap` 2 has no build script. `goblin` 0.8
no longer publishes the `.so` fixtures shipped in 0.3.4. The emitter behavior
is unchanged.
