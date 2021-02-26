module Main where

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
      Just (raydist2distpoint dist ry)

--reflects the direction of a given vector off a given plane
reflectDirOff :: Plane -> Vec -> Vec 
reflectDirOff pl dir = 
   let componentOfDirInDirectionOfNormal = nrm pl *. (nrm pl `dot` dir) in  
   dir -. (componentOfDirInDirectionOfNormal *. 2)

data Sphere = Sphere {radius :: Double, center :: Vec}


raydist2distpoint ry dst = (dist, look ry *. dist +. pos ry)

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
      if ans1 > 0 then Just (raydist2distpoint ans1 ry) else 
         let ans2 = -loc + back2frontDist in 
         Just (raydist2distpoint ans2 ry)

transformSpherePointToPlane :: Sphere -> Vec -> Plane
transformSpherePointToPlane sph pnt = 
   let ctr = center sph in                -- center of the sphere
   let nrm = normalize (pnt -. ctr) in    -- normal is the vector from the center of the sphere to the intersection point, normalized
   Plane pnt nrm

data RayResult = NoHit |
   Hit {dist :: Float, color :: Vec, newRay :: Maybe Ray}

type Traceable = Ray -> RayResult
--TODO: ADD OBJECT IN THE SCENE HERE
-- object 1: a reflective sphere
trcbl1 :: Traceable
trcbl1 ry = 
   let sph = Sphere rad (centerx,centery,centerz)) in       -- declare the sphere
   let (dist,pnt) = sphereIntersectDist sph ry in          -- calculate distance and hitPoint
   if dist then                                             -- Ray hit the sphere
      let pl = transformSpherePointToPlane sph pnt in             -- get plane generated from the hit point on sphere
      let newRay = Ray pnt (reflectDirOff pl (look ry)) in        -- get the reflected ray
      let hitResult = Hit dist (colorr,colorg,colorb) newRay in   -- return the hitresult with distance, color and new ray
      Just hitResult
   else Just NoHit                                          -- Ray miss the sphere, return NoHit

-- object 2: a non-reflective plane
trcbl2 :: Traceable
trcbl2 ry = 
   let pl = Plane (pntx,pnty,pntz) (nrmx,nrmy,nrmz) in   -- declare the plane
   let (dist,pnt) = planeIntersectDist pl ry in                -- calculate distance and hitpoint
   if dist then                                          -- Ray hit the sphere
      let newRay = Nothing in                                     -- set to Nothing for non-reflective item
      let hitResult = Hit dist (colorr,colorg,colorb) newRay in   -- return the hitresult with distance, color and Nothing
      Just hitResult
   else Just NoHit                                       -- Ray miss the sphere, return NoHit

--Add all traceable to a list so that can be used later
globalTraceablelist :: [Traceable]
globalTraceablelist = (trcbl1,trcbl2)

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
   getHitTraceableRay _ [] = Nothing
   getHitTraceableOfRay ry (trcbl:trcbls) =
      let hitResult1 = trcbl ry
      let trcbl2 = getHitTraceableOfRay ry trcbls
      if (hitresult1 == NoHit) && (trcbl2 == Nothing)    -- if current traceble does not hit and there are no hit either in the rest of the list
         then Just Nothing else                             --return Nothing
      if (hitresult1 == NoHit)                           -- if current traceble does not hit but there are hit in the rest of the list
         then Just trcbl2 else                              --return result from the rest of the list
      let dist1 = dist (hitResult1) in                   -- if both current traceble hit and there are hit in the rest of the list 
      let trcbl2 = getHitTraceableOfRay ry trcbls
      let hitResult2 = trcbl2 ry
      let dist2 = dist (hitResult2) in                               -- compare the distance of current object 
      if (dist1 <= dist 2)                                           -- to the closest object in the rest of the list
         then Just trcbl else Just trcbl2                            -- return the one has closer distance
      
   getColorOfRay :: Ray -> Int -> Double -> Vec
   getColorOfRay ry limit totalDist =
      if length(globalTraceablelist) == 0                         -- if list is empty 
         then Just (0,0,0) else                                      -- return BLACK
      let trcbl = (getHitTraceableOfRay ry (globalTraceablelist)) in 
      if trcbl == Nothing                                         -- if this ray does not hit any object
         then Just (0,0,0) else                                      -- return BLACK
      let hitResult = trcbl ry in                           
      let dimCoef = 0.999 * (totalDist + dist (hitResult)) in                                        
      if (newRay (hitResult) == Nothing) || (limit == 0)          -- if it hit, and newRay is nothing or the bounce limit is reached
         then Just (map (dimCoef *) (color hitResult)) else          -- then return the color of current object (*modified by dimCoef)
      let newRayColor = (getColorOfRay (newRay (hitResult)) (limit - 1) (totalDist + dist (hitResult)) in -- get color of the newRay, if it hit, and newRay is not nothing 
      Just (map (dimCoef *) (color hitResult)) +. newRayColor        --then mix current color with the color of the newRay (*modified by dimCoef)
