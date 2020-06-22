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

(See [the examples directory](./examples/) for more examples)

Given an abstract data type as a Haskell typeclass, and a set of implementations, generate and run a set of example usages and compute the on-average "best" structure:

    {-# LANGUAGE TemplateHaskell #-}
    import Test.Rufous

    import qualified Data.List as L
    import qualified Data.Set as S

    class Set q where
        empty :: Ord a => q a
        insert :: Ord a => a -> q a -> q a
        delete :: Ord a => a -> q a -> q a
        lookupMin :: Ord a => q a -> Maybe a

    -- the actual implementation we want to benchmark
    instance Set S.Set where
        empty = S.empty
        insert = S.insert
        delete = S.delete
        lookupMin = S.lookupMin

    -- and another
    data SortedListSet a = SortedListSet [a]
        deriving (Show)

    instance Set SortedListSet where
        empty = SortedListSet []
        insert x (SortedListSet xs) = SortedListSet (L.sort (L.nub (x:xs)))
        delete x (SortedListSet xs) = SortedListSet (L.delete x xs)
        lookupMin (SortedListSet []) = Nothing
        lookupMin (SortedListSet (x:_)) = Just x

    -- Generate Set spec
    makeADTSignature ''Set

    main :: IO ()
    main = mainWith args{signature=_Set}

Output
------

By default Rufous will generate and run many tests, over a wide distribution of different possible usages of a data structure.
For example, tests of different sizes, tests with different proportions of each operation, tests with different amount of "sharing".

Then all these results are aggregated together into a summary table.

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     #dugs |   size | "delete" weight | "empty" weight | "insert" weight | "lookupMin" weight | mortality |  pmf |  pof | Main.SortedListSet | Data.Set.Internal.Set
    ~~~~~~~+~~~~~~~~+~~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~~~~~+~~~~~~~~~~~+~~~~~~+~~~~~~+~~~~~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~~~~~~~~
        13 |  10471 |            0.29 |           0.15 |            0.39 |               0.18 |      0.36 | 0.58 | 1.00 |              0.03s |                 0.02s
        11 |    613 |            0.16 |           0.38 |            0.30 |               0.16 |      0.32 | 0.66 | 1.00 |           6.30e-4s |              7.17e-4s
        17 |   1866 |            0.20 |           0.13 |            0.37 |               0.30 |      0.43 | 0.42 | 1.00 |           4.51e-3s |              5.09e-3s
        53 |    117 |            0.17 |           0.26 |            0.42 |               0.15 |      0.45 | 0.49 | 0.96 |           9.16e-5s |              8.23e-5s
        6  |   3794 |            0.29 |           0.18 |            0.35 |               0.17 |      0.38 | 0.56 | 1.00 |           7.71e-3s |              7.39e-3s
