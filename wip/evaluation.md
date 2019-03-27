Evaluation and Comparison with Auburn
=====================================


Profile
-------

Rufous' profile type is slightly different to Auburn's.  It is more fine-grained: capturing exactly which operations were persistently applied
and which were not.

Auburn's profile did not contain any metric that described the size of a DUG.  This is interesting as
the same Profile in Auburn could have very different performance characteristics.  For example two DUGs
that share the same Profile over a Set ADT, where the implementation was a List then the performance 
characteristics would be much worse for the larger DUG.

If you were to use this Profile to compare a List to a Tree implementation of sets, it would mistakenly 
decide that the Tree was always better: even though for small sets lists will perform just as well
if not sometimes better.
