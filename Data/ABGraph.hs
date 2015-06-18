module Data.ABGraph where

import Data.Array
import Data.Tuple

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
    aNodesArray :: Array Int (ABNode a b),
    bNodesArray :: Array Int (ABNode a b)
}

buildGraph :: [a] -> [b] -> [(Int, Int)] -> [(Int, Int)] -> ABGraph a b
buildGraph as bs aChildLinks bChildLinks = ABGraph (elems aNodesArray') (elems bNodesArray') aNodesArray' bNodesArray'
    where
        aArray = listArray (0, length as - 1) as
        bArray = listArray (0, length bs - 1) bs

        aChildLinksArr = accumArray (flip (:)) [] (bounds aArray) aChildLinks
        bChildLinksArr = accumArray (flip (:)) [] (bounds bArray) bChildLinks

        aParentLinksArr = accumArray (flip (:)) [] (bounds aArray) $ map swap bChildLinks
        bParentLinksArr = accumArray (flip (:)) [] (bounds bArray) $ map swap aChildLinks

        buildA (i, a) = ANode
                    (map (bNodesArray' !) (aParentLinksArr ! i) )
                    (map (bNodesArray' !) (aChildLinksArr ! i) )
                    a

        buildB (i, b) = BNode
                    (map (aNodesArray' !) (bParentLinksArr ! i) )
                    (map (aNodesArray' !) (bChildLinksArr ! i) )
                    b

        aNodesArray' = listArray (bounds aArray) $ map buildA (assocs aArray)
        bNodesArray' = listArray (bounds bArray) $ map buildB (assocs bArray)
