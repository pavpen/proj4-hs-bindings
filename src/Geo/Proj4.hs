{-# LANGUAGE ForeignFunctionInterface, GHCForeignImportPrim, MagicHash,
	UnboxedTuples, UnliftedFFITypes, BangPatterns #-}

module Geo.Proj4 where

import Foreign		(Storable (..), Ptr (..), alloca, castPtr, plusPtr,
			 nullPtr)
import Foreign.C
--import GHC.Float	(Double (..))
--import GHC.Prim
--import GHC.Ptr		(Ptr (..))
import System.IO.Unsafe	(unsafePerformIO)

-- #let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data OCIprojPJ = OCIprojPJ
type OCIprojPJHdl = Ptr OCIprojPJ


data OCIprojUV = OCIprojUV { u :: Double, v :: Double }
	deriving (Eq, Show)

instance Storable OCIprojUV where
  -- alignment _ = #{alignment projUV}
  alignment _ = 4
  sizeOf _ = 2 * (sizeOf $ CDouble 0) -- #{size projUV}
  peek ptr = do --IOExts.fixIO $ \result -> do
  	uVal <- peek ( (castPtr ptr)::(Ptr CDouble) )
	vVal <- peek ( ((castPtr ptr) `plusPtr` (sizeOf uVal))::(Ptr CDouble) )
	return $ OCIprojUV { u=(realToFrac uVal), v=(realToFrac vVal) }
  poke ptr (OCIprojUV { u=u, v=v }) = do
  	poke ((castPtr ptr)::(Ptr CDouble)) (realToFrac u)
	poke (((castPtr ptr) `plusPtr` (sizeOf u))::(Ptr CDouble)) (realToFrac v)


data OCIxyz = OCIxyz { x :: Double, y :: Double, z :: Double, errCode :: Int }
	deriving (Eq, Show)

instance Storable OCIxyz where
  -- alignment _ = #{alignment projUV}
  alignment _ = 4
  sizeOf _ = 3 * (sizeOf $ CDouble 0) -- #{size projUV}
  peek ptr = do --IOExts.fixIO $ \result -> do
  	xVal <- peek ( (castPtr ptr)::(Ptr CDouble) )
	yVal <- peek ( ((castPtr ptr) `plusPtr` (sizeOf xVal))::(Ptr CDouble) )
	zVal <- peek ( ((castPtr ptr) `plusPtr`
				(2 * (sizeOf xVal)))::(Ptr CDouble) )
	errCode <- peek ( ((castPtr ptr) `plusPtr`
				(3 * (sizeOf xVal)))::(Ptr CInt) )
	return $ OCIxyz { x=(realToFrac xVal),
			  y=(realToFrac yVal),
			  z=(realToFrac zVal),
			  errCode=(fromIntegral errCode) }
  poke ptr (OCIxyz { x=x, y=y, z=z, errCode=errCode }) = do
  	poke ( (castPtr ptr)::(Ptr CDouble) ) (realToFrac x)
	poke ( ((castPtr ptr) `plusPtr` (sizeOf x))::(Ptr CDouble) )
	     (realToFrac y)
	poke ( ((castPtr ptr) `plusPtr` (2 * (sizeOf x)))::(Ptr CDouble) )
	     (realToFrac z)
	poke ( ((castPtr ptr) `plusPtr` (3 * (sizeOf x)))::(Ptr CInt) )
	     (fromIntegral errCode)


foreign import ccall "proj_api.h pj_init" c_pj_init ::
	CInt -> Ptr CString -> OCIprojPJHdl


-- pj_init_plus:
foreign import ccall "proj_api.h pj_init_plus" c_pj_init_plus ::
	CString -> OCIprojPJHdl

pj_init_plus defn = withCString defn $ \c_defn -> 
	let ptr = c_pj_init_plus c_defn
	in getRes ptr
  where getRes ptr | ptr == nullPtr = error "pj_init_plus() returned NULL!"
		   | otherwise      = return ptr


-- pj_free:
foreign import ccall "proj_api.h pj_free" c_pj_free :: OCIprojPJHdl -> ()

pj_free :: OCIprojPJHdl -> IO ()
pj_free pj = return $ c_pj_free pj


-- pj_fwd:
foreign import ccall "c_proj4.h c_pj_fwd_ptr" c_pj_fwd ::
	Ptr OCIprojUV -> OCIprojPJHdl -> Ptr OCIprojUV

pj_fwd_io (u, v) pj = alloca $ \uvPtr -> do
	poke uvPtr (OCIprojUV {u=u, v=v})
	let newUVPtr = c_pj_fwd uvPtr pj
	(OCIprojUV { u=resU, v=resV }) <- peek newUVPtr
	return (resU, resV)

pj_fwd uv pj = unsafePerformIO $ pj_fwd_io uv pj

--  The primop wrapper of the C function is not implemented, yet!:
--foreign import prim "c_pj_fwd" c_pj_fwd# ::
--	Double# -> Double# -> Addr# -> (# Double#, Double# #)

--pj_fwd1 :: (Double, Double) -> OCIprojPJHdl -> (Double, Double)
--pj_fwd1 (D# u, D# v) (Ptr pj) =
--	let (# x, y #) = c_pj_fwd# u v pj
--	in (D# x, D# y)


-- pj_inv:
foreign import ccall "c_proj4.h c_pj_inv_ptr" c_pj_inv ::
	Ptr OCIprojUV -> OCIprojPJHdl -> Ptr OCIprojUV

pj_inv_io (u, v) pj = alloca $ \uvPtr -> do
	poke uvPtr (OCIprojUV {u=u, v=v})
	let newUVPtr = c_pj_inv uvPtr pj
	(OCIprojUV { u=resU, v=resV }) <- peek newUVPtr
	return (resU, resV)

pj_inv uv pj = unsafePerformIO $ pj_inv_io uv pj


-- pj_transform:
foreign import ccall "c_proj4.h c_pj_transform_pt" c_pj_transform_pt ::
	OCIprojPJHdl -> OCIprojPJHdl -> CDouble -> CDouble -> CDouble ->
		Ptr OCIxyz -> Ptr OCIxyz

pj_transform_pt_io srcPj dstPj (x, y, z) = alloca $ \resPtr -> do
	let newResPtr = c_pj_transform_pt srcPj dstPj
		(realToFrac x) (realToFrac y) (realToFrac z) resPtr
	(OCIxyz {x=resX, y=resY, z=resZ, errCode=errC}) <- peek newResPtr
	return (resX, resY, resZ)

pj_transform_pt srcPj dstPj xyz =
	unsafePerformIO $ pj_transform_pt_io srcPj dstPj xyz
