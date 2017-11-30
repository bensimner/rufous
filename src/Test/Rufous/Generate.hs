module Test.Rufous.Generate where

import System.Random

import qualified Test.Rufous.Profile   as Prof
import qualified Test.Rufous.Signature as Sig
import qualified Test.Rufous.DUG       as DUG

data GenerationOptions =
    GenerationOptions {
        frontierSize :: Int
    }

defaultGenerationOptions =
    GenerationOptions {
        frontierSize = 15
    }

-- Intermediate DUG during generation
--  there's an easier representation of a DUG and nodes here than the final one
-- namely as a list of versions
data GenDUG =
    GenDUG {
        operationsBuffer :: [VersionNode]  -- these are operations we're building up
    }

data DUGOperation =
    Operation {
        operation :: Sig.OperationId
        , argsBuffer :: [VersionNode]
    }

data VersionNode =
    Node {
        operation :: Sig.OperationId     -- the name of the operation, i.e. "enqueue"
        , args :: [DUG.DUGArg]           -- the applied arguments to this, i.e. "NonVersionArg 2"
        , todoList :: [Sig.OperationId]  -- the list of operations to apply
    }

chooseGenerator :: Sig.Signature st -> Prof.Profile -> IO Sig.OperationId
chooseGenerator s p = do
    let gens = Sig.generators s
    let n = length gens
    i <- randomRIO (0, n - 1)
    return $ Sig.opName (gens !! i)


-- Seeding creates a new DUG with a small number of generated versions
-- with no operations.
seedDUG :: Sig.Signature st -> Prof.Profile -> IO GenDUG
seedDUG s p = do
    generator <- chooseGenerator s p
    let node = Node generator [] []
    return $ GenDUG [node]

generateDUG :: Sig.Signature st -> Prof.Profile -> IO DUG.DUG1
generateDUG s p = undefined

-- add a new node to the DUG
addNewNode :: Sig.Signature st -> Prof.Profile -> DUG.DUG1 -> IO DUG.DUG1
addNewNode s p = undefined


generateExample p = do
    let sumOfWeights = sum $ map snd (Prof.operationWeights p)
    a <- randomRIO (1, sumOfWeights)
    return (DUG.DUG1 [a] (const []))