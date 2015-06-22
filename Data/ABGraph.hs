module Data.ABGraph where

import Data.Tuple
import Debug.Trace
import Data.Set (Set)
import qualified Data.Set as S
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

-- TODO
-- do we really need the integer for Ord...?
-- if we don't have Ord, then we don't have Set, then we can't build
-- ancestor Sets if there are loops -> badness.
data ABNode a b =
    ANode Int [ABNode a b] [ABNode a b] a
    | BNode Int [ABNode a b] [ABNode a b] b


instance Eq (ABNode a b) where
    n == n' = nodeIdx n == nodeIdx n'

instance Ord (ABNode a b) where
    n `compare` n' = nodeIdx n `compare` nodeIdx n'


-- instance Bifunctor ABNode where
    -- bimap = 

parents :: ABNode a b -> [ABNode a b]
parents (ANode _ ps _ _) = ps
parents (BNode _ ps _ _) = ps

children :: ABNode a b -> [ABNode a b]
children (ANode _ _ cs _) = cs
children (BNode _ _ cs _) = cs

nodeIdx :: ABNode a b -> Int
nodeIdx (ANode i _ _ _) = i
nodeIdx (BNode i _ _ _) = i

isANode :: ABNode a b -> Bool
isANode ANode{} = True
isANode BNode{} = False

isBNode :: ABNode a b -> Bool
isBNode ANode{} = False
isBNode BNode{} = True

descendents :: ABNode a b -> [ABNode a b]
descendents n = S.toList $ decendents' S.empty n
    where
        -- ignore nodes that are already in the set---avoid loops.
        decendents' s n' = if n' `S.member` s
                            then s
                            else foldl decendents' (n' `S.insert` s) (children n')

ancestors :: ABNode a b -> [ABNode a b]
ancestors n = S.toList $ decendents' S.empty n
    where
        -- ignore nodes that are already in the set---avoid loops.
        decendents' s n' = if n' `S.member` s
                            then s
                            else foldl decendents' (n' `S.insert` s) (parents n')


aValue :: ABNode a b -> a
aValue (ANode _ _ _ x) = x
aValue BNode{} = undefined

bValue :: ABNode a b -> b
bValue ANode{} = undefined
bValue (BNode _ _ _ x) = x

data ABGraph a b = ABGraph {
    aNodes :: [ABNode a b],
    bNodes :: [ABNode a b],
    aNodesMap :: IntMap (ABNode a b),
    bNodesMap :: IntMap (ABNode a b)
}


buildGraph :: IntMap a -> IntMap b -> [(Int, Int)] -> [(Int, Int)] -> ABGraph a b
buildGraph aMap bMap aChildLinks bChildLinks =
        ABGraph (IM.elems aNodesMap') (IM.elems bNodesMap') aNodesMap' bNodesMap'
    where
        -- aArray = listArray (0, length as - 1) as
        -- bArray = listArray (0, length bs - 1) bs

        listSnd (x,y) = (x, [y])

        -- TODO
        -- slow?
        aChildLinksMap = IM.fromListWith (++) $ map listSnd aChildLinks
        bChildLinksMap = IM.fromListWith (++) $ map listSnd bChildLinks

        aParentLinksMap = IM.fromListWith (++) $ map (listSnd . swap) bChildLinks
        bParentLinksMap = IM.fromListWith (++) $ map (listSnd . swap) aChildLinks

        buildA i = ANode i
                    (map (bNodesMap' IM.!) (IM.findWithDefault [] i aParentLinksMap) )
                    (map (bNodesMap' IM.!) (IM.findWithDefault [] i aChildLinksMap) )

        buildB i = BNode i
                    (map (aNodesMap' IM.!) (IM.findWithDefault [] i bParentLinksMap) )
                    (map (aNodesMap' IM.!) (IM.findWithDefault [] i bChildLinksMap) )

        aNodesMap' = IM.mapWithKey buildA aMap
        bNodesMap' = IM.mapWithKey buildB bMap
