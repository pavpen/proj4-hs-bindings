proj4-hs-bindings
=================

Haskell bindings for the Proj.4 map projection C library.

You need to have Proj.4 already installed as a shared library.

Example transformations (from
[http://www.mysqludf.org/lib_mysqludf_fPROJ4/](http://www.mysqludf.org/lib_mysqludf_fPROJ4/)):

```haskell
	import Geo.Proj4
	
	main :: IO ()
	main = do
		let lon0 = 22.350 * pi / 180 -- Test longitude in radians.
		let lat0 = 40.084 * pi / 180 -- Test latitude in radians.
		let alt0 = 2843 -- Test altitude in meters.
		-- Allocate a new projection:
		pj <- pj_init_plus "+proj=utm +zone=34"
		-- Project our test longitude & latitude, using pj:
		let (x, y) = pj_fwd (lon0, lat0) pj
		putStrLn $ "(x, y): " ++ (show (x, y))
		-- Inverse-project from (x, y) to (longitude, latitude):
		let (lon, lat) = pj_inv (x, y) pj
		putStrLn $ "(lon, lat): " ++ (show (lon, lat))
		-- Convert from one projection to another:
		pj0 <- pj_init_plus "+proj=latlong +ellps=clrk66"
		let (x2, y2, z2) = pj_transform_pt pj0 pj (lon0, lat0, alt0)
		putStrLn $ "(x, y, z): " ++ (show $ (x2, y2, z2))
		-- Free the allocated projections (I know, I should use foreign
		-- pointers with garbage collection, but for now . . .):
		pj_free pj0
		pj_free pj
		return ()
```

You should the following when you compile and run the above:

	(x, y): (615096.1096381239,4437953.6592040695)
	(lon, lat): (0.3900810878207442,0.6995977773693998)
	(x, y, z): (615096.1096381239,4437953.6592040695,2843.0)


GPL license,
(C) Pavel M. Penev, 2012
