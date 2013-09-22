-- | Haskell bindings for the Proj.4 map projection C library.
-- 
-- You need to have Proj.4 already installed as a shared library.
-- 
-- Example transformations (from <http://www.mysqludf.org/lib_mysqludf_fPROJ4/>):
-- @
-- import Geo.Proj4
-- 
-- lon0 = 22.350 * pi / 180 -- Test longitude in radians.
-- lat0 = 40.084 * pi / 180 -- Test latitude in radians.
-- alt0 = 2843 -- Test altitude in meters.
-- 
-- -- | Test projection:
-- pj = newProjection "+proj=utm +zone=34"
-- 
-- -- | Another test (null, e.g. cylindrical) projction:
-- pj0 = newProjection "+proj=latlong +ellps=clrk66"
-- 
-- -- | A projection of our test longitude & latitude, using @pj:
-- (x, y) = pjFwd pj (lon0, lat0)
-- 
-- -- | An inverse projection from (x, y) to (longitude, latitude):
-- (lon, lat) = pjInv pj (x, y)
-- 
-- -- | Convert our test position from one projection to another (pj0 -> pj):
-- (x2, y2, z2) = pjTransformPt pj0 pj (lon0, lat0, alt0)
-- 
-- main :: IO ()
-- main = do
--     putStrLn $ "(x, y): " ++ (show (x, y))
--     putStrLn $ "(lon, lat): " ++ (show (lon * 180 / pi, lat * 180 / pi))
--     putStrLn $ "(x, y, z): " ++ (show $ (x2, y2, z2))
--     return ()
-- @
--
-- You should see the following when you compile and run the above:
-- 
-- >>> main
-- (x, y): (615096.1096381239,4437953.6592040695)
-- (lon, lat): (22.350000000000662,40.083999999999584)
-- (x, y, z): (615096.1096381239,4437953.6592040695,2843.0)
module Geo.Proj4
	( newProjection
	, pjFwd
	, pjInv
	, pjTransformPt
	) where

import Foreign		(ForeignPtr (..), newForeignPtr, withForeignPtr)
import System.IO.Unsafe	(unsafePerformIO)

import Geo.Proj4.Internal


-- | Create a new projection specified by @spec, which should be a string in
--   the format recognized by the 'pj_init_plus' C function.
newProjection spec = unsafePerformIO $ do
	res <- pj_init_plus spec
	newForeignPtr p_c_pj_free res

pjFwd pj coord = unsafePerformIO $ withForeignPtr pj $ \pjPtr ->
	pj_fwd_io coord pjPtr

pjInv pj coord = unsafePerformIO $ withForeignPtr pj $ \pjPtr ->
	pj_inv_io coord pjPtr

pjTransformPt :: ForeignPtr OCIprojPJ -> ForeignPtr OCIprojPJ ->
	(Double, Double, Double) -> (Double, Double, Double)
pjTransformPt srcPj dstPj coord = unsafePerformIO $
	withForeignPtr srcPj $ \srcPjPtr ->
	withForeignPtr dstPj $ \dstPjPtr ->
	  pj_transform_pt_io srcPjPtr dstPjPtr coord
