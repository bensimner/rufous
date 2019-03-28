Statistical removal of the noise
--------------------------------
https://blog.janestreet.com/core_bench-micro-benchmarking-for-ocaml/


Order of evaluation
-------------------
Should be deterministic however unstable (might change with small changes to source)
and `par` exists.


Choosing non-version args
-------------------------
Allowing filling non-version args randomly then throwing away whole BufferedOperations
who do not satisfy the precondition is wasteful.  Better to fix some Version arguments
then iterate until a set of non-version arguments is found: this drastically reduces
the search space.


Randomly generating profiles
----------------------------
It may seem like a uniform distribution over the profile space is the ideal option,
however this isn't true.  Most usages (^need citation/source) use a roughly similar
profile: relatively low mortality, relatively low persistence on mutators, maybe higher
persistence on observers and a diverse proportion of each operation.

Extraction
----------
I found a strictness bug in the extraction algorithm. It used to try grab the nodeIds for
each of the arguments. This is wrong.  Now we annotate each argument with a wrapper that
records what argument it is for.

This does mean that an extracted DUG is not necessarily a complete DUG, where some 
arguments refer to undefined values/unevaluated thunks. For that reason it is undefined
to try evaluate this DUG, as another implementation may not be lazy in exactly the same way.
