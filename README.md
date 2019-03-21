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

(See https://github.com/bensimner/rufous/tree/master/rufous-tests/exe/ for full examples)

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

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     Ntests | "empty" count | "head" count | "snoc" count | "tail" count | mortality |  pmf |  pof |   Main.RQueue  |   Main.BQueue   |  Main.ListQueue 
    ~~~~~~~~+~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~+~~~~~~~~~~~~~~+~~~~~~~~~~~~~~+~~~~~~~~~~~+~~~~~~+~~~~~~+~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~~
        1   |      0.88     |     0.00     |     0.08     |     0.04     |    0.16   | 0.00 | 0.00 |       0s       |        0s       |        0s       
        1   |      0.36     |     0.17     |     0.28     |     0.19     |    0.51   | 0.21 | 0.00 |  0.000008954s  |   0.000007222s  |   0.000006423s  
        1   |      0.20     |     0.06     |     0.26     |     0.48     |    0.48   | 0.31 | 0.00 |  0.000013482s  |   0.000010966s  |   0.00000874s   
        3   |      0.67     |     0.03     |     0.25     |     0.05     |    0.35   | 0.02 | 0.00 |  0.0000014675s |  0.0000009685s  |  0.00000084125s 
        4   |      0.38     |     0.23     |     0.34     |     0.05     |    0.58   | 0.04 | 0.01 | 0.00001171675s | 0.000009486625s | 0.000008869875s 

