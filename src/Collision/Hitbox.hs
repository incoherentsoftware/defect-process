module Collision.Hitbox
    ( module Collision.Hitbox.Types
    , rectHitbox
    , rectHitboxEx
    , polyHitbox
    , lineHitbox
    , dummyHitbox
    , hitboxTopLeft
    , hitboxTopRight
    , setHitboxTopLeft
    , setHitboxTopLeftEx
    , moveHitbox
    , hitboxWidth
    , hitboxHeight
    , hitboxVertices
    , hitboxStartVertex
    , hitboxEndVertex
    , hitboxLeft
    , hitboxTop
    , hitboxRight
    , hitboxBot
    , hitboxX
    , hitboxY
    , hitboxCenter
    , hitboxBotCenter
    , hitboxBotLeft
    , hitboxBotRight
    , hitboxTopCenter
    , intersectsHitbox
    , intersectsLineHitbox
    , containsPointHitbox
    , hitboxAvgIntersectPos
    , hitboxPointCoarseDistance
    , isDummyHitbox
    , drawHitbox
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (catMaybes, fromMaybe, isJust, listToMaybe)
import qualified Data.List as L

import Util
import Window.Graphics
import Collision.Hitbox.Types

rectHitbox :: Pos2 -> Float -> Float -> Hitbox
rectHitbox pos width height = RectHitbox pos width height zeroPos2

rectHitboxEx :: Pos2 -> Float -> Float -> Pos2 -> Hitbox
rectHitboxEx pos width height offset = RectHitbox pos width height offset

polyHitbox :: Pos2 -> [Pos2] -> Hitbox
polyHitbox pos offsets = PolyHitbox vertices offsets
    where vertices = map (vecAdd pos) offsets

lineHitbox :: Pos2 -> Pos2 -> Hitbox
lineHitbox pos1 pos2 = polyHitbox zeroPos2 [pos1, pos2]

dummyHitbox :: Pos2 -> Hitbox
dummyHitbox pos = DummyHitbox pos

hitboxTopLeft :: Hitbox -> Pos2
hitboxTopLeft (RectHitbox pos _ _ _) = pos
hitboxTopLeft hbx@(PolyHitbox _ _)   = Pos2 (hitboxLeft hbx) (hitboxTop hbx)
hitboxTopLeft (DummyHitbox pos)      = pos

hitboxTopRight :: Hitbox -> Pos2
hitboxTopRight (RectHitbox (Pos2 x y) w _ _) = Pos2 (x + w) y
hitboxTopRight hbx@(PolyHitbox _ _)          = Pos2 (hitboxRight hbx) (hitboxTop hbx)
hitboxTopRight (DummyHitbox pos)             = pos

setHitboxTopLeft :: Pos2 -> Hitbox -> Hitbox
setHitboxTopLeft pos (RectHitbox _ w h offset) = RectHitbox (offset `vecAdd` pos) w h offset
setHitboxTopLeft pos (PolyHitbox _ offsets)    = PolyHitbox vertices offsets
    where vertices = map (vecAdd pos) offsets
setHitboxTopLeft pos (DummyHitbox _)           = DummyHitbox pos

setHitboxTopLeftEx :: Pos2 -> Bool -> Hitbox -> Hitbox
setHitboxTopLeftEx pos False hbx                                             = setHitboxTopLeft pos hbx
setHitboxTopLeftEx pos True (RectHitbox _ w h offset@(Pos2 offsetX offsetY)) = RectHitbox pos' w h offset
    where pos' = pos `vecAdd` Pos2 (-offsetX - w) offsetY
setHitboxTopLeftEx pos True (PolyHitbox vertices offsets)                    = PolyHitbox vertices' offsets
    where vertices' = [pos `vecAdd` Pos2 (-x) y | Pos2 x y <- vertices]
setHitboxTopLeftEx pos True (DummyHitbox _)                                  = DummyHitbox pos

moveHitbox :: Pos2 -> Hitbox -> Hitbox
moveHitbox offset (RectHitbox pos w h rectOffset)   = RectHitbox (pos `vecAdd` offset) w h rectOffset
moveHitbox offset (PolyHitbox vertices polyOffsets) = PolyHitbox vertices' polyOffsets
    where vertices' = map (`vecAdd` offset) vertices
moveHitbox offset (DummyHitbox pos)                 = DummyHitbox (pos `vecAdd` offset)

hitboxWidth :: Hitbox -> Float
hitboxWidth (RectHitbox _ w _ _)    = w
hitboxWidth (PolyHitbox vertices _) = abs $ maxX - minX
    where
        xs   = map (fst . vecToTuple) vertices
        maxX = maximum xs
        minX = minimum xs
hitboxWidth (DummyHitbox _)         = 0.0

hitboxHeight :: Hitbox -> Float
hitboxHeight (RectHitbox _ _ h _)    = h
hitboxHeight (PolyHitbox vertices _) = abs $ maximum ys - minimum ys
    where ys = verticesYs vertices
hitboxHeight (DummyHitbox _)         = 0.0

hitboxVertices :: Hitbox -> [Pos2]
hitboxVertices (RectHitbox pos@(Pos2 x y) w h _) =
    [ pos
    , Pos2 (x + w) y
    , Pos2 (x + w) (y + h)
    , Pos2 x (y + h)
    ]
hitboxVertices (PolyHitbox vertices _)           = vertices
hitboxVertices (DummyHitbox _)                   = []

hitboxStartVertex :: Hitbox -> Pos2
hitboxStartVertex hbx = fromMaybe (hitboxTopLeft hbx) (listToMaybe vertices)
    where vertices = hitboxVertices hbx

hitboxEndVertex :: Hitbox -> Pos2
hitboxEndVertex hbx = fromMaybe (hitboxTopLeft hbx) (listToMaybe vertices)
    where vertices = reverse $ hitboxVertices hbx

hitboxLines :: Hitbox -> [(Pos2, Pos2)]
hitboxLines hbx = zip hbxVertices (safeTail hbxVertices ++ take 1 hbxVertices)
    where hbxVertices = hitboxVertices hbx

verticesXs :: [Pos2] -> [Float]
verticesXs vs = map (fst . vecToTuple) vs

verticesYs :: [Pos2] -> [Float]
verticesYs vs = map (snd . vecToTuple) vs

hitboxLeft :: Hitbox -> Float
hitboxLeft (RectHitbox (Pos2 x _) _ _ _) = x
hitboxLeft (PolyHitbox vertices _)       = minimum $ verticesXs vertices
hitboxLeft (DummyHitbox (Pos2 x _))      = x

hitboxTop :: Hitbox -> Float
hitboxTop (RectHitbox (Pos2 _ y) _ _ _) = y
hitboxTop (PolyHitbox vertices _)       = minimum $ verticesYs vertices
hitboxTop (DummyHitbox (Pos2 _ y))      = y

hitboxRight :: Hitbox -> Float
hitboxRight (RectHitbox (Pos2 x _) w _ _) = x + w
hitboxRight (PolyHitbox vertices _)       = maximum $ verticesXs vertices
hitboxRight (DummyHitbox (Pos2 x _))      = x

hitboxBot :: Hitbox -> Float
hitboxBot (RectHitbox (Pos2 _ y) _ h _) = y + h
hitboxBot (PolyHitbox vertices _)       = maximum $ verticesYs vertices
hitboxBot (DummyHitbox (Pos2 _ y))      = y

hitboxX :: Hitbox -> Float
hitboxX = hitboxLeft

hitboxY :: Hitbox -> Float
hitboxY = hitboxTop

hitboxCenter :: Hitbox -> Pos2
hitboxCenter (RectHitbox (Pos2 x y) w h _) = Pos2 centerX centerY
    where
        centerX = x + w / 2.0
        centerY = y + h / 2.0
hitboxCenter poly@(PolyHitbox _ _)         = Pos2 centerX centerY
    where
        centerX = hitboxLeft poly + hitboxWidth poly / 2.0
        centerY = hitboxTop poly + hitboxHeight poly / 2.0
hitboxCenter (DummyHitbox pos)             = pos

hitboxBotCenter :: Hitbox -> Pos2
hitboxBotCenter hbx = Pos2 x (hitboxBot hbx)
    where x = vecX $ hitboxCenter hbx

hitboxBotLeft :: Hitbox -> Pos2
hitboxBotLeft hbx = Pos2 (hitboxLeft hbx) (hitboxBot hbx)

hitboxBotRight :: Hitbox -> Pos2
hitboxBotRight hbx = Pos2 (hitboxRight hbx) (hitboxBot hbx)

hitboxTopCenter :: Hitbox -> Pos2
hitboxTopCenter hbx = Pos2 x (hitboxTop hbx)
    where x = vecX $ hitboxCenter hbx

intersectsLine :: Pos2 -> Pos2 -> Pos2 -> Pos2 -> Maybe Pos2
intersectsLine start1 end1 start2 end2
    | t >= 0 && t <= 1 && u >= 0 && u <= 1 = Just $ r `vecMul` t `vecAdd` p
    | otherwise                            = Nothing
        where
            p = start1
            r = end1 `vecSub` start1
            q = start2
            s = end2 `vecSub` start2
            t = q `vecSub` p `vecCross` s / (r `vecCross` s)
            u = q `vecSub` p `vecCross` r / (r `vecCross` s)

intersectsLineHitbox :: Pos2 -> Pos2 -> Hitbox -> Maybe Pos2
intersectsLineHitbox start1 end1 hbx = case catMaybes points of
    [] -> Nothing
    ps -> Just $ L.minimumBy distCmp ps
    where
        distCmp :: Pos2 -> Pos2 -> Ordering
        distCmp p1 p2
            | distSq1 == distSq2 = EQ
            | distSq1 < distSq2  = LT
            | otherwise          = GT
                where
                    distSq1 = vecDistSq p1 start1
                    distSq2 = vecDistSq p2 start1

        hbxLines = hitboxLines hbx
        points   = [intersectsLine start1 end1 start2 end2 | (start2, end2) <- hbxLines]

containsPointHitbox :: Pos2 -> Hitbox -> Bool
containsPointHitbox (Pos2 x y) hitbox = x >= hX && x <= hX + hW && y >= hY && y <= hY + hH
    where
        Pos2 hX hY = hitboxTopLeft hitbox
        hW         = hitboxWidth hitbox
        hH         = hitboxHeight hitbox

intersectsRect :: (Pos2, Float, Float) -> (Pos2, Float, Float) -> Bool
intersectsRect (Pos2 x1 y1, w1, h1) (Pos2 x2 y2, w2, h2) =
    not $ x1 > (x2 + w2) || x2 > (x1 + w1) || y1 > (y2 + h2) || y2 > (y1 + h1)

intersectsPoly :: [Pos2] -> [Pos2] -> Bool
intersectsPoly vertices1 vertices2 = all overlap vertices1Pairs && all overlap vertices2Pairs
    where
        normalizeAxis :: (Pos2, Pos2) -> Vec2
        normalizeAxis (Pos2 x1 y1, Pos2 x2 y2) = vecNormalize axis
            where axis = Vec2 (y1 - y2) (x2 - x1)

        projectionRange :: [Pos2] -> Vec2 -> Vec2
        projectionRange vertices axis = Vec2 minLength maxLength
            where
                projLengths = map ((`vecDot` axis) . toVec2) vertices
                minLength   = minimum projLengths
                maxLength   = maximum projLengths

        overlap :: (Pos2, Pos2) -> Bool
        overlap pairVerts = not $ maxA < minB || minA > maxB
            where
                axis           = normalizeAxis pairVerts
                Vec2 minA maxA = projectionRange vertices1 axis
                Vec2 minB maxB = projectionRange vertices2 axis

        vertices1Pairs = zip vertices1 (safeTail vertices1 ++ take 1 vertices1)
        vertices2Pairs = zip vertices2 (safeTail vertices2 ++ take 1 vertices2)

intersectsHitbox :: Hitbox -> Hitbox -> Bool
intersectsHitbox (DummyHitbox _) _                                             = False
intersectsHitbox _ (DummyHitbox _)                                             = False
intersectsHitbox (RectHitbox p1 w1 h1 _) (RectHitbox p2 w2 h2 _)               =
    intersectsRect (p1, w1, h1) (p2, w2, h2)
intersectsHitbox (PolyHitbox [v1, v2] _) hbx                                   =
    isJust $ intersectsLineHitbox v1 v2 hbx
intersectsHitbox hbx (PolyHitbox [v1, v2] _)                                   =
    isJust $ intersectsLineHitbox v1 v2 hbx
intersectsHitbox poly1@(PolyHitbox vertices1 _) poly2@(PolyHitbox vertices2 _) =
    intersectsCoarse && intersectsPoly vertices1 vertices2
        where
            (p1, p2)         = (hitboxTopLeft poly1, hitboxTopLeft poly2)
            (w1, w2)         = (hitboxWidth poly1, hitboxWidth poly2)
            (h1, h2)         = (hitboxHeight poly1, hitboxHeight poly2)
            intersectsCoarse = intersectsRect (p1, w1, h1) (p2, w2, h2)
intersectsHitbox (RectHitbox p1 w1 h1 _) poly@(PolyHitbox vertices2 _)         =
    intersectsCoarse && intersectsPoly vertices1 vertices2
        where
            p2               = hitboxTopLeft poly
            w2               = hitboxWidth poly
            h2               = hitboxHeight poly
            intersectsCoarse = intersectsRect (p1, w1, h1) (p2, w2, h2)
            vertices1        =
                [ p1
                , p1 `vecAdd` Pos2 w1 0.0
                , p1 `vecAdd` Pos2 w1 h1
                , p1 `vecAdd` Pos2 0.0 h1
                ]
intersectsHitbox poly@(PolyHitbox _ _) rect@(RectHitbox _ _ _ _)               = intersectsHitbox rect poly

-- this is a very hazy interpretation of average intersect position
hitboxAvgIntersectPos :: Hitbox -> Hitbox -> Pos2
hitboxAvgIntersectPos (PolyHitbox [v1, v2] _) hbx2 = fromMaybe (hitboxCenter hbx2) (intersectsLineHitbox v1 v2 hbx2)
hitboxAvgIntersectPos hbx1 (PolyHitbox [v1, v2] _) = fromMaybe (hitboxCenter hbx1) (intersectsLineHitbox v1 v2 hbx1)
hitboxAvgIntersectPos hbx1 hbx2
    | null intersectPoints = hitboxCenter hbx1
    | otherwise            = averagePos intersectPoints
    where
        averagePos :: [Pos2] -> Pos2
        averagePos ps = sumPs `vecDiv` (fromIntegral $ length ps)
            where sumPs = foldr vecAdd zeroPos2 ps

        intersectPoints = catMaybes $ do
            (start1, end1) <- hitboxLines hbx1
            (start2, end2) <- hitboxLines hbx2

            let
                start1Contained = containsPointHitbox start1 hbx2
                end1Contained   = containsPointHitbox end1 hbx2
                start2Contained = containsPointHitbox start2 hbx1
                end2Contained   = containsPointHitbox end2 hbx1

            return $ case intersectsLine start1 end1 start2 end2 of
                pos@(Just _) -> pos
                Nothing      -> if
                    | start1Contained && end1Contained -> Just $ averagePos [start1, end1]
                    | start1Contained                  -> Just start1
                    | end1Contained                    -> Just end1
                    | start2Contained && end2Contained -> Just $ averagePos [start2, end2]
                    | start2Contained                  -> Just start2
                    | end2Contained                    -> Just end2
                    | otherwise                        -> Nothing

hitboxPointCoarseDistance :: Pos2 -> Hitbox -> Float
hitboxPointCoarseDistance (Pos2 x y) hbx = sqrt $ dx ** 2.0 + dy ** 2.0
    where
        left  = hitboxLeft hbx
        right = hitboxRight hbx
        top   = hitboxTop hbx
        bot   = hitboxBot hbx

        dx
            | x >= left && x <= right = 0.0
            | otherwise               = min (abs (x - left)) (abs (x - right))
        dy
            | y >= top && y <= bot    = 0.0
            | otherwise               = min (abs (y - top)) (abs (y - bot))

isDummyHitbox :: Hitbox -> Bool
isDummyHitbox = \case
    DummyHitbox _ -> True
    _             -> False

drawHitbox :: (GraphicsReadWrite m, MonadIO m) => Color -> ZIndex -> Hitbox -> m ()
drawHitbox color zIndex (RectHitbox pos w h _)  = drawRect pos w h color zIndex
drawHitbox color zIndex (PolyHitbox vertices _) = drawPolygon vertices color zIndex
drawHitbox  _ _ (DummyHitbox _)                 = return ()
