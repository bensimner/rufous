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
    $ stack exec rufous-tests-example-queue

Usage
-----

(See https://github.com/bensimner/rufous/tree/master/rufous-tests/examples/ for full examples)

Given an abstract data type as a Haskell typeclass, and a set of implementations, generate and run a set of example usages and compute the on-average "best" structure:

    import Test.Rufous

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
    makeADTSignature ''Queue

    main = mainWith args{signature=_Queue}

Output
------

By default Rufous will generate and run many tests, over a wide distribution of different possible usages of a data structure.
For example, tests of different sizes, tests with different proportions of each operation, tests with different amount of "sharing".

Then all these results are aggregated together into a summary table.

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     #tests | #versions | "empty" weight | "head" weight | "snoc" weight | "tail" weight | mortality |  pmf |  pof |    Main.RQueue |    Main.BQueue | Main.ListQueue
    ~~~~~~~~+~~~~~~~~~~~+~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~+~~~~~~~~~~~+~~~~~~+~~~~~~+~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~
          1 |        62 |           0.93 |          0.00 |          0.03 |          0.03 |      0.06 | 0.33 | 0.00 |             0s |             0s |             0s
          2 |       163 |           0.42 |          0.12 |          0.29 |          0.17 |      0.45 | 0.17 | 0.03 |  0.0000151105s |  0.0000134035s |  0.0000127935s
          3 |       564 |           0.36 |          0.03 |          0.33 |          0.28 |      0.60 | 0.08 | 0.01 | 0.00002189525s | 0.00002418875s |   0.000017407s
          2 |        17 |           0.81 |          0.03 |          0.08 |          0.08 |      0.24 | 0.12 | 0.00 |   0.000001196s |   0.000000812s |   0.000000692s
          2 |      1465 |           0.37 |          0.12 |          0.34 |          0.17 |      0.56 | 0.04 | 0.17 |   0.000119765s |  0.0001071205s |   0.000103229s

