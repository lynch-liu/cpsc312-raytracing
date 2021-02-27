module Main where

import qualified Data.Maybe
-- import Graphics.Gloss
-- import Graphics.Gloss.Juicy
import Codec.Picture
import qualified Data.Vector

main :: IO ()
main = print "test"

type Vec = (Double,Double,Double)

vmap :: (Double -> Double) -> Vec -> Vec
vmap f (x,y,z) = (f x, f y, f z)

--vzip :: (Double -> Double -> Double) -> Vec -> Vec -> Vec
vzip :: (a -> b -> c) -> (a, a, a) -> (b, b, b) -> (c, c, c)
vzip f (x1,y1,z1) (x2,y2,z2) = (f x1 x2, f y1 y2, f z1 z2)

(+.) :: Vec -> Vec -> Vec
(+.) = vzip (+)

(-.) :: Vec -> Vec -> Vec
(-.) = vzip (-)

(*.) :: Vec -> Double -> Vec 
(*.) v d = vmap (*d) v

(/.) :: Vec -> Double -> Vec
(/.) v d = vmap (/d) v

epsilon :: Double
epsilon = 1e-12

--approximately equals (within epsilon)
(~=) :: Double -> Double -> Bool 
(~=) x y = abs(x-y) < epsilon

--vector approximately equals
(=.) :: Vec -> Vec -> Bool 
(=.) p1 p2 = case vzip (~=) p1 p2 of
   (True,True,True) -> True 
   _ -> False

-- >>> (1,1,1) =. (0.999999999999999,0.999999999999999,1.00000000000001)
-- True

--dot product
dot :: Vec -> Vec -> Double
dot (x1,y1,z1) (x2,y2,z2) = x1*x2 + y1*y2 + z1*z2

--cross product
cross :: Vec -> Vec -> Vec
cross (x1,y1,z1) (x2,y2,z2) = (y1*z2 - z1*y2, z1*x2-x1*z2,x1*y2-y1*x2)

quadrance :: Vec -> Double 
quadrance v = v `dot` v

mag :: Vec -> Double
mag v = sqrt (quadrance v)

normalize :: Vec -> Vec 
normalize v = v /. mag v

--the Vecs are the rows
type Mat3 = (Vec,Vec,Vec) 

--matrix multiplying vector
(*..) :: Mat3 -> Vec -> Vec
(*..) ((a11,a12,a13),
       (a21,a22,a23),
       (a31,a32,a33)) (x,y,z) = 
    (x*a11 + y*a12 + z*a13,
     x*a21 + y*a22 + z*a23,
     x*a31 + y*a32 + z*a33)

transp :: Mat3 -> Mat3 
transp ((a11,a12,a13),
        (a21,a22,a23),
        (a31,a32,a33)) = 
       ((a11,a21,a31),
        (a12,a22,a32),
        (a13,a23,a33))

--matrix multiplied by matrix
(*...) :: Mat3 -> Mat3 -> Mat3 
(*...) m1 m2 = let (c1,c2,c3) = transp m2 in 
   transp (m1 *.. c1, m1 *.. c2, m1 *.. c3)
--because transpose lets you get and set the columns instead of the rows

rotxy :: Double -> Mat3 
rotxy th = ((cos th, -sin th, 0),
            (sin th,  cos th, 0),
            (0,       0,      1))

rotxz :: Double -> Mat3
rotxz th = ((cos th, 0, -sin th),
            (0,      1,       0),
            (sin th, 0,  cos th))

rotyz :: Double -> Mat3
rotyz th = ((1, 0,       0),
            (0, cos th, -sin th),
            (0, sin th,  cos th))

-- 3 in 1
rot_xy_xz_yz :: Double -> Double -> Double -> Mat3
rot_xy_xz_yz xy xz yz = rotxy xy *... rotxz xz *... rotyz yz

--point and normal
data Plane = Plane {pnt, nrm :: Vec} 

threePointsToPlane :: Vec -> Vec -> Vec -> Plane 
threePointsToPlane a b c = Plane {pnt = (a +. b +. c) /. 3, nrm = norm}
   where norm = normalize ((b -. a) `cross` (c -. a))

data Ray = Ray {pos, look :: Vec}

parallelTo :: Ray -> Plane -> Bool 
parallelTo ry pl = nrm pl `dot` look ry ~= 0

--https://www.scratchapixel.com/lessons/3d-basic-rendering/minimal-ray-tracer-rendering-simple-shapes/ray-plane-and-ray-disk-intersection
--returns maybe the r such that the point pos + r*look is on the plane
planeIntersectDist :: Plane -> Ray -> Maybe (Double,Vec) 
planeIntersectDist pl ry = if ry `parallelTo` pl then Nothing 
   else let dist =  (((pnt pl -. pos ry) `dot` nrm pl) / (look ry `dot` nrm pl)) in 
      Just (raydist2distpoint ry dist)

--reflects the direction of a given vector off a given plane
reflectDirOff :: Plane -> Vec -> Vec 
reflectDirOff pl dir = 
   let componentOfDirInDirectionOfNormal = nrm pl *. (nrm pl `dot` dir) in  
   dir -. (componentOfDirInDirectionOfNormal *. 2)

data Sphere = Sphere {radius :: Double, center :: Vec}


raydist2distpoint :: Ray -> Double -> (Double, Vec)
raydist2distpoint ry dst = (dst, look ry *. dst +. pos ry)

sphereIntersectDist :: Sphere -> Ray -> Maybe (Double,Vec)
sphereIntersectDist sph ry = 
   let r2 = radius sph * radius sph in 
   let center2pos = center sph -. pos ry in 
   let distanceFromCenterToPosSquared = quadrance center2pos in 
   let loc = center2pos `dot` look ry in 
   let loc2 = loc * loc in 
   let back2frontDistSquared = loc2 - distanceFromCenterToPosSquared + r2 in 
   if back2frontDistSquared < 0 then Nothing else
      let back2frontDist = sqrt back2frontDistSquared in 
      let ans1 = -loc - back2frontDist in 
      --ans1 is the distance to the front of the sphere and ans2 the distance to the back of the sphere
      --if you're not inside the sphere, you want ans1
      if ans1 > 0 then Just (raydist2distpoint ry ans1) else 
         let ans2 = -loc + back2frontDist in 
         Just (raydist2distpoint ry ans2)

transformSpherePointToPlane :: Sphere -> Vec -> Plane
transformSpherePointToPlane sph pnt = 
   let ctr = center sph in                -- center of the sphere
   let nrm = normalize (pnt -. ctr) in    -- normal is the vector from the center of the sphere to the intersection point, normalized
   Plane pnt nrm

data RayResult = NoHit |
   Hit {dist :: Double, clr :: Vec, newRay :: Maybe Ray}

rrHits NoHit = False 
rrHits _ = True

type Traceable = Ray -> RayResult
-- ADD OBJECT IN THE SCENE HERE
-- object 1: a reflective sphere
trcbl1 :: Traceable
trcbl1 ry =
   let rad = 1 in
   let centerx = 1 in
   let centery = 1 in
   let centerz = 1 in 
   let sph = Sphere rad (centerx,centery,centerz) in       -- declare the sphere
   if Data.Maybe.isJust (sphereIntersectDist sph ry) then                                             -- Ray hit the sphere
      let Just (dist,pnt) = sphereIntersectDist sph ry in         -- calculate distance and hitPoint
      let pl = transformSpherePointToPlane sph pnt in             -- get plane generated from the hit point on sphere
      let newRay = Ray pnt (reflectDirOff pl (look ry)) in 
               let colorr = 200 in
      let colorg = 200 in
      let colorb = 200 in       -- get the reflected ray
      let hitResult = Hit dist (colorr,colorg,colorb) (Just newRay) in   -- return the hitresult with distance, color and new ray
      hitResult
   else NoHit                                          -- Ray miss the sphere, return NoHit

-- object 2: a non-reflective plane
trcbl2 :: Traceable
trcbl2 ry = 
   let pntx = 1 in
   let pnty = 1 in
   let pntz = 1 in
   let nrmx = 1 in
   let nrmy = 1 in
   let nrmz = 1 in
   let pl = Plane (pntx,pnty,pntz) (nrmx,nrmy,nrmz) in   -- declare the plane
   if Data.Maybe.isJust (planeIntersectDist pl ry) then                                          -- Ray hit the sphere
      let Just (dist,pnt) = planeIntersectDist pl ry in                -- calculate distance and hitpoint
      let colorr = 200 in
      let colorg = 200 in
      let colorb = 200 in
      let newRay = Nothing in                                     -- set to Nothing for non-reflective item
      let hitResult = Hit dist (colorr,colorg,colorb) newRay in   -- return the hitresult with distance, color and Nothing
      hitResult
   else NoHit                                       -- Ray miss the sphere, return NoHit

--Add all traceable to a list so that can be used later
globalTraceablelist :: [Traceable]
globalTraceablelist = [trcbl1,trcbl2]

data Camera = Camera {position :: Vec, fov, yaw, pitch :: Double}

--consider the image to be a square (you can still ask for pixel indices outside the square though.)
--the main image is mapped onto x,y in [0, sidelength) 
getRayAtPixel :: Camera -> Int -> Int -> Int -> Ray
getRayAtPixel cam sidelength x y = 
   let xf  = fromIntegral x in
   let yf  = fromIntegral y in 
   let slf = fromIntegral sidelength in 
   --say that the fov is the angle from the center of the image to the middle of the right of the image.
   let xf2 = xf - slf / 2 in 
   let yf2 = yf - slf / 2 in 
   --the image is a grid (plane) of points perpendicular to the x axis.
   --we want some distance to initialize the grid from the origin so that:
   --then the angle from (dist,0,0) to (0,0,0) to (dist,sidelength/2,0) = fov
   --    see fig1.png (shows the xy plane)
   --    therefore tan(fov) = (sidelength/2) / dist.
   --    dist = sidelength / (2*tan(fov))
   let dist = slf / 2 / tan (fov cam) in 
   --rotate this point accordint to the yaw and pitch of the camera 
   let camrot = rotxy (pitch cam) *... rotxz (yaw cam) in 
   let lk = camrot *.. (dist,xf2,yf2) in 
      Ray {pos = position cam, look = lk}
   
getHitTraceableOfRay :: Ray -> [Traceable] -> Maybe Traceable
getHitTraceableOfRay _ [] = Nothing
getHitTraceableOfRay ry (trcbl:trcbls) =
   let hitResult1 = trcbl ry in
   let trcbl2 = getHitTraceableOfRay ry trcbls in 
      case (hitResult1, trcbl2) of 
         (NoHit,Nothing) -> Nothing 
         (NoHit,other) -> other 
         (Hit {}, Nothing) -> Just trcbl 
         (Hit {dist = dist1},Just trcbl2) -> let hitResult2 = trcbl2 ry in 
   -- if (hitResult1 == NoHit) && (trcbl2 == Nothing)    -- if current traceble does not hit and there are no hit either in the rest of the list
   --    then Nothing else                                  --return Nothing
   -- if hitResult1 == NoHit                             -- if current traceble does not hit but there are hit in the rest of the list
   --    then Just trcbl2 else                                   --return result from the rest of the list
   -- let dist1 = dist (hitResult1) in                   -- if both current traceble hit and there are hit in the rest of the list 
   -- let hitResult2 = trcbl2 ry in        
            let dist2 = dist (hitResult2) in                               -- compare the distance of current object 
               if (dist1 <= dist2)                                           -- to the closest object in the rest of the list
            then Just trcbl else Just trcbl2                            -- return the one has closer distance
      
pickResult :: RayResult -> RayResult -> RayResult
pickResult NoHit r = r
pickResult r NoHit = r
pickResult Hit {dist=d1, clr = c1, newRay=nr1} Hit {dist=d2, clr=c2, newRay=nr2} = 
   if d1 > d2 then Hit {dist=d2, clr=c2, newRay=nr2} else
      Hit {dist=d1, clr = c1, newRay=nr1}

getHitTraceable :: Ray -> [Traceable] -> RayResult
getHitTraceable ry traceables = 
   let applied = map (\f -> f ry) traceables in
   let hits = filter rrHits applied in 
   let notBehindCamera = filter (\Hit {dist = dst} -> dst > 0) hits in 
   case notBehindCamera of 
      [] -> NoHit
      (t:ts) -> foldl pickResult t ts

-- getColorOfRay :: Ray -> Int -> Double -> Vec
-- getColorOfRay ry limit totalDist =
--    if length(globalTraceablelist) == 0                         -- if list is empty 
--       then (0,0,0) else                                      -- return BLACK
--    let  trcbl = (getHitTraceableOfRay ry (globalTraceablelist)) in 
--       case trcbl of 
--          Nothing -> (0,0,0)
--          Just something -> 
--    -- if Just trcbl == Nothing                                         -- if this ray does not hit any object
--    --    then (0,0,0) else                                      -- return BLACK
--             let hitResult = something ry in --this is wrong, trcbl is a Maybe Traceable, not a Traceable. //TODO                          
--             let dimCoef = 0.999 * (totalDist + dist (hitResult)) in   
--                case (newRay hitResult, limit) of 
--                   (_,0) ->  vmap (dimCoef *) (clr hitResult)
--                   (Nothing,_) ->  vmap (dimCoef *) (clr hitResult)       
--                   (Just newRayobj,limit) ->                     
--             -- if (newRay (hitResult) == Nothing) || (limit == 0)          -- if it hit, and newRay is nothing or the bounce limit is reached
--             --    then (vmap (dimCoef *) (color hitResult)) else          -- then return the color of current object (*modified by dimCoef)
--                      -- let Just newRayobj = newRay (hitResult)in
--                      let newRayColor = (getColorOfRay (newRayobj)) (limit - 1) (totalDist + dist (hitResult)) in -- get color of the newRay, if it hit, and newRay is not nothing 
--                      --this is wrong, it tries get the color of a Maybe Ray instead of a Ray //TODO
--                      (vmap ((clr hitResult) *. dimCoef) +. newRayColor)        --then mix current color with the color of the newRay (*modified by dimCoef)

dm :: Vec -> Double -> Vec
dm v d = v *. (0.9 ** d)

getColorOfRay_v2 :: Ray -> Int -> Double -> Vec
getColorOfRay_v2 ry limit totalDist = 
   case (limit,getHitTraceable ry globalTraceablelist) of 
      (_,NoHit) -> (0,0,0)
      (_,Hit {dist=d, clr = c, newRay=Nothing}) -> dm c (d + totalDist)
      (0,Hit {dist=d, clr = c, newRay=Just nr}) -> dm c (d + totalDist)
      (_,Hit {dist=d, clr = c, newRay=Just nr}) -> getColorOfRay_v2 nr (limit - 1) (d + totalDist)


--consider the image to be a square (you can still ask for pixel indices outside the square though.)
--the main image is mapped onto x,y in [0, sidelength) 
getColorAtPixel :: Camera -> Int -> Int -> Int -> Vec 
getColorAtPixel cam sidelength x y = 
   let ry = getRayAtPixel cam sidelength x y in 
      getColorOfRay_v2 ry 100 0

getColorAtPixelW8 :: Camera -> Int -> Int -> Int -> (Pixel8,Pixel8,Pixel8)
getColorAtPixelW8 cam sidelength x y =let (r,g,b) =  (getColorAtPixel cam sidelength x y) in
   (round r, round g, round b)

defaultCamera = Camera {position = (2,1,1), fov=pi/4, yaw=0,pitch=0}


mainImage :: Int -> [[Vec]]
mainImage sidelength = map (\y -> map (getColorAtPixel defaultCamera sidelength y) [0..sidelength-1]) [0..sidelength-1]

mainImageW8 :: Int -> [[(Pixel8,Pixel8,Pixel8)]]
mainImageW8 sidelength = map (\y -> map (getColorAtPixelW8 defaultCamera sidelength y) [0..sidelength-1]) [0..sidelength-1]

getPixelRGB8 :: Camera -> Int -> Int -> Int -> PixelRGB8 
getPixelRGB8 cam sidelength x y = let (r,g,b) = getColorAtPixelW8 cam sidelength x y in 
   PixelRGB8 r g b

mainImagePixelRGB8 :: Int -> [[PixelRGB8]]
mainImageW8 sidelength = map (\y -> map (getPixelRGB8 defaultCamera sidelength y) [0..sidelength-1]) [0..sidelength-1]


-- >>>mainImage 2
-- [[(180.0,180.0,180.0),(141.263602302561,141.263602302561,141.263602302561)],[(141.263602302561,141.263602302561,141.263602302561),(118.09800000000001,118.09800000000001,118.09800000000001)]]
 
mainImageVector int = Data.Vector.fromList (concat mainImagePixelRGB8 int)

mainImageImage sidelength = Image {imageHeight = sidelength,imageWidth=sidelength,imageData=mainImageVector sidelength}

-- gifobj = 
--    let geWidth = sidelength in
--    let geHeight = sidelength in
--    let geLooping = LoopingForever in
--    let geFrames = getFrameFromImage img in
--    GifEncode geWidth geHeight Nothing Nothing geLooping geFrames

-- getFrameFromImage :: Image -> GifFrame
-- getFrameFromImage img = 
--    let pxls = Nothing in --it is Image Pixel8 here //TODO
--    GifFrame sidelength sidelength Nothing Nothing 1 DisposalAny pxls
