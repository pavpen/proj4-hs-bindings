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


datum = wgs84
cScaleX = 0.00001
cScaleY = cScaleX
lonEps = 10 * pi / 180
lonStep = 10 * pi / 180
latEps = 10 * pi / 180
latStep = 10 * pi / 180

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
  where procsLon lon = p2 $ (let (x, y) = pjFwd pj (lon, lat)
  			     in (x * cScaleX, y * cScaleY))

lonLines pj = mconcat $ map procsLon lons 
  where procsLon lon = lonLine pj latStep lon

latLines pj = mconcat $ map procsLat lats 
  where procsLat lat = latLine pj lonStep lat

graticules =
	lonLines pj # lw 1 # lc red `atop`
	latLines pj # lw 1 # lc green

mapCirc ctr radius bearingStep =
	cubicSpline True $ map procsBrg [0, bearingStep .. 360]
  where procsBrg b = let (lonLat, _) = Geo.direct datum ctr b radius
  			 (x, y) = pjFwd pj $ Geo.radians' lonLat
  		     in p2 $ (x * cScaleX, y * cScaleY)

dgm = 	mapCirc (40 !.! 40) 1000000 (pi/5) # lw 1 # lc blue `atop`
	graticules

main = defaultMain (pad 1.1 dgm)
