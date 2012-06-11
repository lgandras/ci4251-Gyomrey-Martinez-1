

module RTree (Rectangle, RTree, insert, delete, search) where

import Data.Bits
import Data.Monoid

data Rectangle = R { ul , ll , lr , ur :: (Int ,Int) }

data RTree = NonLeaf Int [RTree] Rectangle | Leaf Rectangle

n :: Int
n = 65535

delete :: RTree -> Rectangle -> Either e RTree
delete t _ = Right t

insert :: RTree -> Rectangle -> Either e RTree
insert rts rect = Right rts
{-
handleOverflow (NonLeaf _ rts _) rect h = 
chooseLeaf (Leaf r) rect h = 
-}
search :: RTree -> Rectangle -> Maybe [ Rectangle ]
search (NonLeaf _ rts rect) w = foldr ((mappend) . flip search rect) Nothing rts
search (Leaf rect) w = if insersect rect w then Just [rect] else Nothing

mbr :: [Rectangle] -> Rectangle
mbr r = foldl1 mbr_aux r
    where
        mbr_aux rect1 rect2 = (R (l, b) (l, u) (r, b) (r, u))
            where (l1, b1) = ll rect1
                  (u1, r1) = ur rect1
                  (l2, b2) = ll rect2
                  (u2, r2) = ur rect2
                  l = l1 `min` l2
                  b = b1 `min` b2
                  u = u1 `max` u2
                  r = r1 `max` r2


insersect :: Rectangle -> Rectangle -> Bool
insersect rect1 rect2 = ix && iy
    where (l1, b1) = ll rect1
          (u1, r1) = ur rect1
          (l2, b2) = ll rect2
          (u2, r2) = ur rect2
          ix = b1 <= b2 && (b2 < u1 || u2 <= u1)
            || b2 <= b1 && (b1 < u2 || u1 <= u2)
          iy = l1 <= l2 && (l2 < r1 || r2 <= r1)
            || l2 <= l1 && (l1 < r2 || r1 <= r2)

-- Adaptado de http://en.wikipedia.org/wiki/Hilbert_curve
hv rect = hv_aux (n `div` 2) x y 0
    where (l, b) = ll rect
          (u, r) = ur rect
          x = (l + r) `div` 2
          y = (b + u) `div` 2
hv_aux 0 x y d = d
hv_aux s x y d = hv_aux (s `div` 2) x y (d + s * s * m)
    where m = if (y .&. s) > 0 then
              (if (x .&. s) > 0 then 3 else 0) else 1
          (x, y) = hc_rot m x y

hc_rot :: Int -> Int -> Int -> (Int, Int)
hc_rot 1 x y = (x, y)
hc_rot 3 x y = hc_rot 0 (n - 1 - x) (n - 1 - y)
hc_rot 0 x y = (y, x)
