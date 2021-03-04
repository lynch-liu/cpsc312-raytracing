module Main where

import qualified Data.Maybe
import System.Process 
import GHC.IO.Handle
-- import Graphics.Gloss
-- import Graphics.Gloss.Juicy
-- import Codec.Picture
-- import Codec.Picture.Gif
-- import Codec.Picture.Types
-- import qualified Data.Vector
-- import qualified Data.Vector.Storable

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
   let center2pos = pos ry -. center sph in 
   let distanceFromCenterToPosSquared = quadrance center2pos in 
   let loc = center2pos `dot` look ry in 
   let loc2 = loc * loc in 
   let back2frontDistSquared = loc2 - distanceFromCenterToPosSquared + r2 in 
   if back2frontDistSquared < 0 then Nothing else
      let back2frontDist = sqrt back2frontDistSquared in 
      let ans1 = (-loc) - back2frontDist in 
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
   Hit {dist :: Double, clr :: Vec, newRay :: Maybe (Ray,Double)}

rrHits NoHit = False 
rrHits _ = True

type Traceable = Ray -> RayResult

makeCheckerboardFloorPlane :: Double -> Vec -> Vec -> Double -> Double -> Traceable
makeCheckerboardFloorPlane h c1 c2 xoffset zoffset ray = 
   let thePlane = Plane{pnt=(0,h,0),nrm=(0,1,0)} in 
   case planeIntersectDist thePlane ray of 
      Nothing -> NoHit 
      Just (d,(ptx,_,ptz)) -> 
         let chosenColor = if even (round (ptx + xoffset)) == even (round (ptz + zoffset)) then c1 else c2 in
         Hit {dist=d,clr=chosenColor,newRay=Nothing}

makeReflectiveSphere :: Vec -> Double -> Vec -> Double -> Traceable
makeReflectiveSphere cntr r clr tint ray = 
   let theSphere = Sphere {radius=r,center=cntr} in 
      case sphereIntersectDist theSphere ray of 
         Nothing -> NoHit 
         Just (d,pnt) -> let tangentPlane = transformSpherePointToPlane theSphere pnt in 
            let incidentDir = reflectDirOff tangentPlane (look ray) in 
            let pnt2 = pnt +. (incidentDir *. epsilon) in --make sure it doesn't count as already intersecting the sphere
            let nwry = Ray {pos = pnt2, look = incidentDir} in 
            Hit {dist=d,clr=clr,newRay = Just (nwry,tint)}

makeColoredSphere :: Vec -> Double -> Vec -> Traceable
makeColoredSphere cntr r clr ray = 
   let theSphere = Sphere {radius=r,center=cntr} in 
      case sphereIntersectDist theSphere ray of 
         Nothing -> NoHit 
         Just (d,pnt) -> Hit {dist=d,clr=clr,newRay = Nothing}

type Scene = [Traceable]

data Camera = Camera {position :: Vec, fov, yaw, pitch :: Double}

defaultCamera = Camera {position = (0,0,0), fov=pi/4, yaw=0,pitch=0}

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
   let lk = normalize (camrot *.. (dist,-yf2,xf2)) in 
      Ray {pos = position cam, look = lk}
      
pickResult :: RayResult -> RayResult -> RayResult
pickResult NoHit r = r
pickResult r NoHit = r
pickResult Hit {dist=d1, clr = c1, newRay=nr1} Hit {dist=d2, clr=c2, newRay=nr2} = 
   if d1 > d2 then Hit {dist=d2, clr=c2, newRay=nr2} else
      Hit {dist=d1, clr = c1, newRay=nr1}

scene2TraceableAndCullBehind :: Scene -> Traceable
scene2TraceableAndCullBehind scene ry = 
   let applied = map (\f -> f ry) scene in
   let hits = filter rrHits applied in 
   let notBehindCamera = filter (\Hit {dist = dst} -> dst > 0) hits in 
   case notBehindCamera of 
      [] -> NoHit
      (t:ts) -> foldl pickResult t ts

dm :: Vec -> Double -> Vec
dm v d = v *. (0.9 ** d)

mix :: Vec -> Vec -> Double -> Vec 
mix v1 v2 proportionOfFirst = (v1 *. proportionOfFirst) +. (v2 *. (1 - proportionOfFirst))

getColorOfRay_v2 :: Scene -> Ray -> Int -> Double -> Vec
getColorOfRay_v2 scene ry limit totalDist = 
   case scene2TraceableAndCullBehind scene ry of 
      NoHit -> skyblue
      Hit {dist=d, clr = c, newRay=Nothing} -> c --dm c (d + totalDist)
      Hit {dist=d, clr = c, newRay=Just (nr,tint)} | limit <= 0 -> c --dm c (d + totalDist)
      Hit {dist=d, clr = c, newRay=Just (nr,tint)} ->  mix c (getColorOfRay_v2 scene nr (limit - 1) (d + totalDist)) tint --getColorOfRay_v2 scene nr (limit - 1) (d + totalDist)


--consider the image to be a square (you can still ask for pixel indices outside the square though.)
--the main image is mapped onto x,y in [0, sidelength) 
getColorAtPixel :: Camera -> Scene -> Int -> Int -> Int -> Vec 
getColorAtPixel cam scene sidelength x y = 
   let ry = getRayAtPixel cam sidelength x y in 
      getColorOfRay_v2 scene ry 100 0


getColorAtPixelInt ::  Camera -> Scene -> Int -> Int -> Int -> (Int, Int, Int)
getColorAtPixelInt cam scene sl x y = let (r,g,b) = getColorAtPixel cam scene sl x y in    
   (round r, round g, round b)

mainImage :: Scene -> Int -> [[Vec]]
mainImage scene sl = map (\y -> map (getColorAtPixel defaultCamera scene sl y) [0..sl-1]) [0..sl-1]

render :: Camera -> Scene -> Int -> [[(Int, Int, Int)]]
render camera scene sl = map (\y -> map (getColorAtPixelInt camera scene sl y) [0..sl-1]) [0..sl-1]

hPutIntPlusSpace :: Handle -> Int -> IO ()
hPutIntPlusSpace handle i = do
   hPutStr handle (show i)
   hPutChar handle ' '

hPutPixelSpaced :: Handle -> (Int, Int, Int) -> IO ()
hPutPixelSpaced handle (r,g,b) = do
   hPutIntPlusSpace handle r 
   hPutIntPlusSpace handle g 
   hPutIntPlusSpace handle b

hPutPixelListSpaced :: Handle -> [(Int, Int, Int)] -> IO ()
hPutPixelListSpaced _ [] = return ()
hPutPixelListSpaced handle (px:pxs) = do
   hPutPixelSpaced handle px 
   hPutPixelListSpaced handle pxs

hPutPixelListListSpaced :: Handle -> [[(Int, Int, Int)]] -> IO ()
hPutPixelListListSpaced _ [] = return ()
hPutPixelListListSpaced handle (pl:pls) = do 
   hPutPixelListSpaced handle pl 
   hPutPixelListListSpaced handle pls 

hPutPixelListListListSpaced :: Handle -> [[[(Int, Int, Int)]]] -> Int -> IO ()
hPutPixelListListListSpaced _ [] _ = return () 
hPutPixelListListListSpaced handle (pll:plls) n = do 
   hPutPixelListListSpaced handle pll 
   putStrLn ("finished rendering frame #" ++ (show n))
   hPutPixelListListListSpaced handle plls (n+1)

saveImagesToGif :: String -> Int -> Int -> [[[(Int, Int, Int)]]] -> IO ()
saveImagesToGif filename w h imgs = do
   --putStrLn ("writing GIF: " ++ show imgs ++ "\n")
   (Just ins,_, _,_) <- createProcess (proc "java" ["-jar","GifWriterCommandLine.jar"]) {std_in = CreatePipe }
   hPutStr ins filename 
   hPutChar ins '\n'
   hPutIntPlusSpace ins w
   hPutIntPlusSpace ins h
   hPutIntPlusSpace ins (length imgs)
   hPutPixelListListListSpaced ins imgs 0 
   hFlush ins
   putStrLn "Done writing GIF."
   hClose ins 

testgif = saveImagesToGif "test.gif" 1 1 [ [[(0,0,0)]],[[(255,255,255)]]]

skyblue = (0x87,0xce,0xeb)
black = (0,0,0)
white = (0xff,0xff,0xff)
gray = (0x80,0x80,0x80)
red = (0xff,0x40,0)

scene1 :: Scene
scene1 = [
   makeCheckerboardFloorPlane (-3) white gray 0 0,
   makeReflectiveSphere (2,0,0) 1 red 0.1]

saveSceneAsSingleImage :: Camera -> Scene -> Int -> IO ()
saveSceneAsSingleImage camera scene sl = saveImagesToGif "main.gif" sl sl [render camera scene sl]

type Tableau = (Camera,Scene)

--the double is time, and it is between 0 and 1
type Animation = Double -> Tableau 

saveAnimation :: String -> Animation -> Int -> Int -> IO () 
saveAnimation name anim numframes sidelength = 
   let nfd = fromIntegral numframes in 
   let tableaus = [anim ((fromIntegral f) / nfd) | f <- [0..numframes - 1]] in 
   let images = [render cam scene sidelength | (cam,scene) <- tableaus] in 
   saveImagesToGif name sidelength sidelength images 

floormoving :: Animation 
floormoving r = 
   let flr = makeCheckerboardFloorPlane (-3) white gray (r*2) 0 in 
   let sph = makeReflectiveSphere (2,0,0) 1 black 0.2 in 
   let cam = Camera {position = (0,0.5,0), fov=pi/4, yaw=0,pitch=0} in 
      (cam, [flr,sph])

bobbingSphere :: Animation 
bobbingSphere r = 
   let flr = makeCheckerboardFloorPlane (-3) white gray 0 0 in 
   let sph = makeReflectiveSphere (2,(sin (2*pi*r)),0) 1 black 0.2 in 
   let cam = Camera {position = (0,0.5,0), fov=pi/4, yaw=0,pitch=0} in 
      (cam, [flr,sph])

bobbingFloorMoving :: Animation 
bobbingFloorMoving r = 
   let flr = makeCheckerboardFloorPlane (-3) white gray (r*2) 0 in 
   let sph = makeReflectiveSphere (2.7,(sin (2*pi*r)),0) 1 black 0.2 in 
   let cam = Camera {position = (0,0.5,0), fov=pi/4, yaw=0,pitch=0} in 
      (cam, [flr,sph])

orbiting :: Int -> Double -> Animation 
orbiting n rad r = 
   let degs = r * pi * 2 / (fromIntegral n) in 
   let offset = 2 * pi / (fromIntegral n) in 
   let indices = [0..n-1] in 
   let rads = [degs + (fromIntegral i) * offset | i <- indices] in 
   let ps = [(cos(t),0,sin(t)) | t <- rads] in 
   let spheres = [makeReflectiveSphere pos rad black 0.2 | pos <- ps] in 
   let flr = makeCheckerboardFloorPlane (-5) white gray (r * 2) 0 in 
   let cam = Camera {position = ((-2),0.75,0), fov=pi/4, yaw=0,pitch=(-pi)/12} in 
      (cam,flr:spheres)

main :: IO () 
--main = saveAnimation "floormoving.gif" floormoving 150 700
--main = saveAnimation "bobbing.gif" bobbingSphere 150 700
--main = saveAnimation "bobbingFloorMoving.gif" bobbingFloorMoving 150 700
main = saveAnimation "orbiting.gif" (orbiting 5 0.5) 100 700