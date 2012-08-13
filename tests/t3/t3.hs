import Geo.Proj4

lon0 = 22.350 * pi / 180 -- Test longitude in radians.
lat0 = 40.084 * pi / 180 -- Test latitude in radians.
alt0 = 2843 -- Test altitude in meters.

-- | Test projection:
pj = newProjection "+proj=utm +zone=34"

-- | Another test (null, e.g. cylindrical) projction:
pj0 = newProjection "+proj=latlong +ellps=clrk66"

-- | A projection of our test longitude & latitude, using @pj:
(x, y) = pjFwd (lon0, lat0) pj

-- | An inverse projection from (x, y) to (longitude, latitude):
(lon, lat) = pjInv (x, y) pj

-- | Convert our test position from one projection to another (pj0 -> pj):
(x2, y2, z2) = pjTransformPt pj0 pj (lon0, lat0, alt0)

main :: IO ()
main = do
	putStrLn $ "(x, y): " ++ (show (x, y))
	putStrLn $ "(lon, lat): " ++ (show (lon * 180 / pi, lat * 180 / pi))
	putStrLn $ "(x, y, z): " ++ (show $ (x2, y2, z2))
	return ()
