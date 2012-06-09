

module RTree (Rectangle, RTree, insert, delete, search) where

data Rectangle = R { ul , ll , lr , ur :: (Int ,Int) }

data RTree = NonLeaf (Int, RTree, Rectangle) | Leaf (Rectangle)

insert :: RTree -> Rectangle -> Either e RTree
insert t _ = Right t

delete :: RTree -> Rectangle -> Either e RTree
delete t _ = Right t

search :: RTree -> Rectangle -> Maybe [ Rectangle ]
search _ r = Just [r]