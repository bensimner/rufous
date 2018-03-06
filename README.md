Rufous
======

Rufous is a straightforward data structure selection tool.

Installation
------------

To install

    $ <install_instructions>


Usage 
-----

(See https://github.com/bensimner/rufous/tree/master/examples for full examples)

Given an abstract data type as a Haskell typeclass, and a set of implementations, generate and run a set of example usages and compute the on-average "best" structure:

    import Test.Rufous (makeRufousSpec, runRufous)

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
    makeRufousSpec ''Queue

    main = runRufous _Queue

Output
------

Example output. ?
