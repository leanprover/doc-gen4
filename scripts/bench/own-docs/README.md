# The `own-docs` benchmark

This benchmark measures the time taken for `doc-gen4` to generate its
own documentation. This is a convenient stand-in for moderately-sized
Lean projects without too many dependencies. When this benchmark was
created, the time was dominated by building documentation for core
Lean, but this may change over time and it's good to track it.
