module Data.ABGraph where

import Data.Tuple
import Debug.Trace (traceShow)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

data ABNode a b =
    ANode [ABNode a b] [ABNode a b] a
    | BNode [ABNode a b] [ABNode a b] b


-- instance Bifunctor ABNode where
    -- bimap = 

parents :: ABNode a b -> [ABNode a b]
parents (ANode ps _ _) = ps
parents (BNode ps _ _) = ps

children :: ABNode a b -> [ABNode a b]
children (ANode _ cs _) = cs
children (BNode _ cs _) = cs

aValue :: ABNode a b -> a
aValue (ANode _ _ x) = x
aValue BNode{} = undefined

bValue :: ABNode a b -> b
bValue ANode{} = undefined
bValue (BNode _ _ x) = x

data ABGraph a b = ABGraph {
    aNodes :: [ABNode a b],
    bNodes :: [ABNode a b],
    aNodesMap :: IntMap (ABNode a b),
    bNodesMap :: IntMap (ABNode a b)
}


access :: IntMap e -> Int -> e
access a i = traceShow i $ a IM.! i

-- TODO
-- these links are *not* pointing to the correct numbers when coming from
-- Event.hs...
buildGraph :: IntMap a -> IntMap b -> [(Int, Int)] -> [(Int, Int)] -> ABGraph a b
buildGraph aMap bMap aChildLinks bChildLinks =
        ABGraph (IM.elems aNodesMap') (IM.elems bNodesMap') aNodesMap' bNodesMap'
    where
        -- aArray = listArray (0, length as - 1) as
        -- bArray = listArray (0, length bs - 1) bs

        -- TODO
        -- slow
        aChildLinksMap = IM.fromListWith (++) $ map (\(x, y) -> (x, [y]) ) aChildLinks
        bChildLinksMap = IM.fromListWith (++) $ map (\(x, y) -> (x, [y]) ) bChildLinks

        aParentLinksMap = IM.fromListWith (++) $ map ( (\(x, y) -> (x, [y]) ) . swap) aChildLinks
        bParentLinksMap = IM.fromListWith (++) $ map ( (\(x, y) -> (x, [y]) ) . swap) bChildLinks

        buildA i = ANode
                    (map (bNodesMap' `access`) (aParentLinksMap `access` i) )
                    (map (bNodesMap' `access`) (aChildLinksMap `access` i) )

        buildB i = BNode
                    (map (aNodesMap' `access`) (IM.findWithDefault [] i bParentLinksMap) )
                    (map (aNodesMap' `access`) (IM.findWithDefault [] i bChildLinksMap) )

        aNodesMap' = IM.mapWithKey buildA aMap
        bNodesMap' = IM.mapWithKey buildB bMap
