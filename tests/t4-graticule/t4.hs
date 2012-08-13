{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Geo.Proj4

-- | Test projection:
pj = newProjection "+proj=vandg +towsg84=0,0,0"

-- | Another test (null, e.g. cylindrical) projction:
pj0 = newProjection "+proj=latlong +ellps=clrk66"


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

dgm =	lonLines pj # lw 1 # lc red `atop`
	latLines pj # lw 1 # lc green

main = defaultMain (pad 1.1 dgm)
