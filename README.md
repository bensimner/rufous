Rufous
======

Rufous is a straightforward data structure selection tool.

Installation
------------

To install and run the Queue example:

    $ git clone https://github.com/bensimner/rufous
    $ cd rufous
    $ stack setup
    $ stack build
    $ stack runhaskell examples/Queue.hs

Usage 
-----

(See https://github.com/bensimner/rufous/tree/master/examples for full examples)

Given an abstract data type as a Haskell typeclass, and a set of implementations, generate and run a set of example usages and compute the on-average "best" structure:

    import Test.Rufous (makeADTSpec, runRufous)

    class Queue q where
        empty :: q a
        snoc :: a -> q a
        head :: q a -> a
        tail :: q a -> q a
    
    newtype ListQueue a = ListQueue [a]
    instance Queue ListQueue where
        empty = ListQueue []
        snoc x (ListQueue xs) = ListQueue (xs ++ [x])
        head (ListQueue (x:_)) = x
        tail (ListQueue (_:xs)) = xs

    -- Generate Queue spec
    makeADTSpec ''Queue

    main = runRufous _Queue

Output
------

The default output is a summary table of the profile of each generated usage, how many operations were called, the persistent mutation/observation factors, the mortality (how many versions were not mutated further) and the actual times for each discovered implementation.

    dug name | "empty" count | "head" count | "snoc" count | mortality | pmf   | pof  | "Main.ListQueue" time
    -------- + ------------- + ------------ + ------------ + --------- + ----- + ---- + ---------------------
    dug7029  | 52            | 153          | 262          | 0.6       | 0.875 | 0.34 | 0.000123s

