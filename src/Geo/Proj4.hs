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
