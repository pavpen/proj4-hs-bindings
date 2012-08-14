{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Geo			(bearing, wgs84, (|..|), (!.!))
import qualified Data.Geo as Geo
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Geo.Proj4

-- | Test projection:
pj = newProjection "+proj=vandg +datum=WGS84"

-- | Another test (null, e.g. cylindrical) projction:
pj0 = newProjection "+proj=latlong +ellps=clrk66"


scalePair factor (x,y) = (x * factor, y * factor)

degrees' = scalePair (180 / pi)


datum = wgs84
cScaleX = 0.00001
cScaleY = cScaleX
lonEps = 10 * pi / 180
lonStep = 10 * pi / 180
latEps = 10 * pi / 180
latStep = 10 * pi / 180

projLonLat (lon, lat) = let (x, y) = pjFwd pj (lon, lat)
  			in (x * cScaleX, y * cScaleY)

lons =	[-pi + lonEps] ++
	[-pi + lonStep, -pi + (2*lonStep) .. pi - lonStep] ++
	[ pi - lonEps]

lats =	[-pi/2 + latEps] ++
	[-pi/2 + latStep, -pi/2 + (2*latStep) .. pi/2 - latStep] ++
	[ pi/2 - latEps]

lonLine pj latStep lon =
	cubicSpline False $ map procsLat lats
  where procsLat lat = p2 $ (let (x, y) = pjFwd pj (lon, lat)
  			     in (x * cScaleX, y * cScaleY))

latLine pj lonStep lat =
	cubicSpline False $ map procsLon lons
  where procsLon lon = p2 $ projLonLat (lon, lat)

lonLines pj = mconcat $ map procsLon lons 
  where procsLon lon = lonLine pj latStep lon

latLines pj = mconcat $ map procsLat lats 
  where procsLat lat = latLine pj lonStep lat

graticules =
	lonLines pj # lw 1 # lc gray `atop`
	latLines pj # lw 1 # lc green

lonLatCirc ctr radius bearingStep = map procsBrg [0, bearingStep .. 360]
  where procsBrg b =
	  let (lat, lon) = Geo.radians' $ fst $ Geo.direct datum ctr b radius
	  in (lon, lat)

mapCirc ctr radius bearingStep = cubicSpline True $
		map (p2 . projLonLat) $ lonLatCirc ctr radius bearingStep

lonLatSphCirc ctr rho rotStep = map coords [0, rotStep .. 2*pi]
  where (cLat, cLon) = Geo.radians' ctr
	sin_dLat rotAng = (sin rho) * (sin rotAng)
	sin_dLon rotAng lat = (sin rho) * (cos rotAng) / (cos lat)
	coords rotAng = let lat = cLat + (asin $ sin_dLat rotAng)
			    lon = cLon + (asin $ sin_dLon rotAng lat)
			in (lon, lat)

sphCirc ctr rho rotStep =
	cubicSpline True $ map (p2 . projLonLat) $ lonLatSphCirc ctr rho rotStep


dgm = 	mapCirc (70 !.! 90) 1000000 45 # lw 1 # lc blue `atop`
	sphCirc (70 !.! 90) (10 * pi / 180) (pi / 4) # lw 1 # lc red `atop`
	graticules

main = do
	putStrLn $ show $ map (scalePair (180/pi)) $
		lonLatCirc (80 !.! 40) 1000000 45
	putStrLn $ show $ map (scalePair (180/pi)) $
		lonLatSphCirc (0 !.! 0) (10 * pi / 180) (pi / 3)
	defaultMain (pad 1.1 dgm)
