import Geo.Proj4

main :: IO ()
main = do
	let lon0 = 22.350 * pi / 180
	let lat0 = 40.084 * pi / 180
	let alt0 = 2843
	pj <- pj_init_plus "+proj=utm +zone=34"
	--xy <- pj_fwd (OCIprojUV { u=22.350 * pi / 180, v=40.084 * pi / 180 }) pj
	let (x, y) = pj_fwd (lon0, lat0) pj
	putStrLn $ "xy: " ++ (show $ (x, y))
	let (u, v) = pj_inv (x, y) pj
	putStrLn $ "lambda,phi: " ++ (show $ (u * 180 / pi, v * 180 / pi))
	--let (x1, y1) = pj_fwd1 (lon0, lat0) pj
	--putStrLn $ "x1y1: " ++ (show $ (x1, y1))
	pj0 <- pj_init_plus "+proj=latlong +ellps=clrk66"
	let (x2, y2, z2) = pj_transform_pt pj0 pj (lon0, lat0, alt0)
	putStrLn $ "xyz: " ++ (show $ (x2, y2, z2))
	pj_free pj0
	pj_free pj
	return ()
