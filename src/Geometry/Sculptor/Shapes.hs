-- |
-- Module      : Geometry.Sculptor.Shapes
-- Description :
-- Copyright   : (c) Jonatan Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan Sundqvist
-- Stability   :
-- Portability : Portable
--

-- Based on Graphics.Michelangelo.Shapes

-- TODO | - Generic container types (eg. Monoids or OverloadedLists?)
--          -- Rely on polymorphism (typeclasses) rather than hard-coded types
--        - Generic vertex types (?)
--        - Anchored shapes (?)
--        - Bounding boxes
--        - Indexed shapes
--        - Triangulation (or - more generally - tiling) of polygons
--        - QuickCheck
--        - Performance
--        - Types to represent surfaces, edges, etc.
--        - Different coordinate systems and orientations (eg. clockwise vs counter-clockwise)
--        - Generating colours (eg. monochrome) and creating data that is easily uploaded to the GPU
--
--        - Use types to ensure correctness
--          -- Color type (and a module to go with it)
--          -- Vector type (cf. Cartesian)
--          -- Vertex winding

-- API -------------------------------------------------------------------------

module Geometry.Sculptor.Shapes where

-- We'll need these ------------------------------------------------------------

import Leibniz.Constants (π)

-- Types -----------------------------------------------------------------------

-- | A 'Face' is a list of vertices
-- TODO | - Polymorphic container
--        - Make sure all vertices lie in the same plane (using Triangles?)
data Face v = Face [v]


-- |
data Edge v = Edge v v

-- Data ------------------------------------------------------------------------

-- Functions -------------------------------------------------------------------

-- Vertex constructors ---------------------------------------------------------

-- | A simple list of vertices
vlist :: a -> a -> a -> [a]
vlist x y z = [x, y, z]

-- Tessellation ----------------------------------------------------------------

-- TODO | - Move tessellation to separate module

-- | Tessellate a polygon with triangles. Presently, only concave shapes will tessellate properly.
-- TODO | - Make it work for any shape (convex and concave)
--        - Opposite rather than adjacent sides (don't include centre)
triangles :: [a] -> [[a]]
triangles (a:rest) = pairwise (\b c -> [a, b, c]) rest
  where
    -- | Combine every adjacent pair in the list with the given function
    pairwise :: (a -> a -> b) -> [a] -> [b]
    pairwise f xs = zipWith f xs (drop 1 xs)

-- Two-dimensional shapes ------------------------------------------------------------------------------------------------------------------

-- TODO | - Normals and texture coordinates

-- | Create a point that lies on the XY plane
xy :: Fractional f => (f -> f -> f -> a) -> f -> f -> a
xy f x y = f x y 0


-- | Create a point that lies on the XZ plane
xz :: Fractional f => (f -> f -> f -> a) -> f -> f -> a
xz f x z = f x 0 z


-- | Create a point that lies on the YZ plane
yz :: Fractional f => (f -> f -> f -> a) -> f -> f -> a
yz f z y = f 0 y z


-- | Generate the vertices for a rectangular plane, centred at (0,0,0)
--   TODO | - Rename (eg. 'rectangle') (?)
--
--   Assuming the plane is parallel to the X and Y axes, the vertices are given in a clockwise order,
--   starting from the top left corner (eg. top left, top right, bottom right, bottom left).
plane :: Fractional f => (f -> f -> a) -> f -> f -> [a]
plane f da db = [f (-da/2) (db/2), f (da/2) (db/2), f (da/2) (-db/2), f (-da/2) (-db/2)]

-- TODO | - Deal with clock-wise/counter-clockwise issues (if there are any)

-- | Generate the vertices for a rectangular plane, parallel with the X and Y axes and centred at (0,0,0)
planeXY :: Fractional f => (f -> f -> f -> a) -> f -> f -> [a]
planeXY = plane . xy


-- | Generate the vertices for a rectangular plane, parallel with the X and Z axes and centred at (0,0,0)
planeXZ :: Fractional f => (f -> f -> f -> a) -> f -> f -> [a]
planeXZ = plane . xz


-- | Generate the vertices for a rectangular plane, parallel with the Y and Z axes and centred at (0,0,0)
planeYZ :: Fractional f => (f -> f -> f -> a) -> f -> f -> [a]
planeYZ = plane . yz


-- | Generate the vertices for a regular polygon, centred at (0,0,0)
polygon :: (Floating f, Integral i) => (f -> f -> a) -> i -> f -> [a]
polygon f sides radius = [ let θ = angle n in f (radius*cos θ) (radius*sin θ) | n <- [0..hi] ]
  where
    hi = sides - 1
    angle n = fromIntegral n * 2*π/fromIntegral sides


-- |
-- lattice :: 

-- Three-dimensional shapes ----------------------------------------------------------------------------------------------------------------

-- | Generate the vertices of an axis-aligned cuboid centred at (0,0,0)
-- TODO | - Use combinatorics to generate vertices (?)
--        - Specify order and winding somehow
cuboid :: Fractional f => (f -> f -> f -> a) -> f -> f -> f -> [a]
cuboid f dx dy dz = [f (-hdx)   hdy  hdz, f hdx   hdy  hdz, f hdx   hdy  (-hdz), f (-hdx)   hdy  (-hdz), --
                     f (-hdx) (-hdy) hdz, f hdx (-hdy) hdz, f hdx (-hdy) (-hdz), f (-hdx) (-hdy) (-hdz)] --
  where
    (hdx, hdy, hdz) = (dx/2, dy/2, dz/2)


-- | Generate the vertices of an axis-aligned cube centred at (0,0,0)
cube :: Fractional f => (f -> f -> f -> a) -> f -> [a]
cube f side = cuboid f side side side

-- Indices ---------------------------------------------------------------------------------------------------------------------------------

-- |
-- left, right, bottom, top, back, front
-- V3 top left bottom


-- | Indices for a solid cuboid (with triangulated sides, cf. `cuboid`)
-- TODO | - Use more sensible container types (three levels of lists seems a bit excessive...)
--        - Allow any kind of tessellation (?)
cuboidIndices :: Integral i => [[[i]]]
cuboidIndices = fmap triangles [[0,1,2,3], [4,5,6,7], [3,2,6,7], [0,1,5,4], [0,3,7,4], [1,2,6,5]] -- Top, bottom, Front, Back, Left, Right


-- |
cuboidLineIndices :: Integral i => [(i, i)]
cuboidLineIndices = [(0,1), (2,3), (4,5), (6,7), -- X-axis
                     (0,4), (5,1), (3,7), (6,2), -- Y-axis
                     (0,3), (2,1), (4,7), (6,5)] -- Z-axis

--------------------------------------------------------------------------------------------------------------------------------------------

-- | Indices for a shape extruded from a 2D polygon
-- TODO | - Figure out which types to use
--        - Generic 'stitching' of lid and bottom, counter-clockwise or clockwise (?)
--        - Generate vertices and indices separately
-- extrude :: (Vector v, Fractional f) => (v -> v') -> v -> [v] -> [v']
-- extrude :: (Num v, Fractional f) => (v -> v') -> v -> [v] -> [v']
-- extrude f direction shape = undefined
--   where
--     -- |
--     -- side ::
--     side a b = concat [[f a, f $ a + direction, f $ b + direction], [f a, f $ b + direction, f b]]
--
--     -- | Stitches together two polygons by successively connecting one edge from each shape with two adjacent triangles.
--     --   The end result is a rim joining the shapes, made out of a triangle strip.
--     -- TODO: Factor out 'looping' (ie. closing the loop)
--     -- Found hole `_pairwise' with type: (v -> v -> [v']) -> [(a1, b)] -> t0 [a]
--     stitch side' bottom lid = concat . pairwise (uncurry side) $ zip (close bottom) (close lid)
--
--     -- |
--     -- TODO: I wonder how many times I've written this function (factor out)
--     pairwise f xs = zipWith f xs (tail xs)
--
--     -- |
--     close shape' = shape' ++ [head shape']
--
--     -- |
--     rim = stitch undefined
