import Codec.Picture
import Data.Colour
import Data.Colour.RGBSpace.HSV
import Data.Colour.SRGB

main = do
  createZoom "rangerotatezoom" ((-2, 2), (-2, 2)) range rotate 0.01 1000
  -- imageCreator "rangerotate.png" range rotate

minimumWind x delt iter 
                       | iter == 0 = x
                       | otherwise = minimumWind (x - delt*x) delt (iter-1)

height :: Int
height = 720

width :: Int
width = 1280

xInt :: Double -> Double -> Double
xInt max min = (max-min)/(fromIntegral width)

yInt :: Double -> Double -> Double
yInt max min = (max-min)/(fromIntegral height)

maxIter :: Integer
maxIter = 20

absolute :: Double -> Double -> Double
absolute a b = sqrt (a*a + b*b)

getX :: Int -> Double -> Double -> Double
getX x max min = (fromIntegral x)*(xInt max min) + min
getY :: Int -> Double -> Double -> Double
getY y max min = -(fromIntegral y)*(yInt max min) + max

type State = ((Double, Double), (Double, Double))
type ComplexFunction = (Double -> Double -> [Double] -> Integer -> [Double])
type Measurement = ([Double] -> Double)

createZoom :: String -> State -> Measurement -> ComplexFunction -> Double -> Int -> IO ()
createZoom name st@((minx, maxx), (miny, maxy)) m f delt iter 
                             | iter == 0 = do imageCreator (name ++ show (-iter + 1000) ++ ".png") st m f
                             | otherwise = do imageCreator (name ++ show (-iter + 1000) ++ ".png") st m f
                                              putStrLn ("Done with " ++ show (-iter + 1000) ++ ".")
                                              createZoom name state m f delt (iter-1)
                             where state = ((minx + xdelt, maxx - xdelt), (miny + ydelt, maxy + ydelt))
                                   xdelt = delt*maxx
                                   ydelt = delt*maxy
-- 2 1.99 1.98 1.97
imageCreator :: String -> State -> Measurement -> ComplexFunction -> IO ()
imageCreator path ((minx, maxx), (miny, maxy)) m f = writePng path $ generateImage pixelRenderer width height
  where pixelRenderer x y = PixelRGB8 (channelRed color) (channelGreen color) (channelBlue color)
          where hue = m (f (getX x maxx minx) (getY y maxy miny) [0] 1)
                color = toSRGB24 $ (sRGB (channelRed srgb) (channelGreen srgb) (channelBlue srgb))
                  where srgb = hsv (180*hue) 0.6 1

-- MEASUREMENT DEFINITIONS
average :: Measurement
average (d:ds) = d/(fromIntegral maxIter)

range :: Measurement
range ds = (maximum ds) - (minimum ds) 

contraharmonicmean :: Measurement
contraharmonicmean ds = (sum (map (^2) ds)) / (sum ds)

-- FUNCTION DEFINITIONS
zsquared :: ComplexFunction
zsquared x y xs@(smooth:_) iter
                        | iter >= maxIter = (zsmooth + smooth):xs --results in maxIter items in the list excluding starting zero
                        | otherwise      = zsquared (x*x-y*y - 0.8) (2*x*y + 0.156) ((zsmooth + smooth):xs) (iter+1)
                          where zsmooth = exp (-(absolute x y))
sinz :: ComplexFunction 
sinz x y xs@(smooth:_) iter
                    | iter > maxIter = (zsmooth + smooth):xs
                    | otherwise = sinz nx ny ((zsmooth + smooth):xs) (iter+1)
                    where zsmooth = exp (-(absolute x y))
                          nx = 0.5*(exp(-y)*sin(x)-exp(y)*sin(-x));
                          ny = 0.5*(exp(y)*cos(-x) - exp(-y)*cos(x));

rotate :: ComplexFunction
rotate x y xs@(smooth:_) iter
                     | iter > maxIter = (zsmooth + smooth):xs
                     | otherwise = rotate nx ny ((zsmooth + smooth) : xs) (iter+1)
                     where theta = atan(y/x)
                           ratio = tan(theta + y)
                           zsmooth = exp (-theta)
                           nx = sqrt((x*x + y*y)/(ratio*ratio+1))
                           ny = ratio*nx



