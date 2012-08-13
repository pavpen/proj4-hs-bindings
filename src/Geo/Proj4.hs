module Geo.Proj4
	( newProjection
	, pjFwd
	, pjInv
	, pjTransformPt
	) where

import Foreign		(newForeignPtr)
import System.IO.Unsafe	(unsafePerformIO)

import Geo.Proj4.Internal


-- | Create a new projection specified by @spec, which should be a string in
--   the format recognized by the 'pj_init_plus' C function.
newProjection spec = unsafePerformIO $ do
	res <- pj_init_plus spec
	newForeignPtr p_c_pj_free res
	return res

pjFwd = pj_fwd

pjInv = pj_inv

pjTransformPt :: OCIprojPJHdl -> OCIprojPJHdl -> (Double, Double, Double) ->
	(Double, Double, Double)
pjTransformPt = pj_transform_pt
