{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- :set -fobject-code in GHCi
module Picture where

import Foreign hiding (rotate)
import Foreign.C.Types

import Control.Applicative

import Data.Complex

default (Int, Float, Integer)

-- Strict
data Color =
  Color
    { r, g, b, a :: !Float
    }
  deriving (Show)

data CColor =
  CColor
    { rC, gC, bC :: !CUChar
    }
  deriving (Show)

instance Storable CColor where
  alignment _ = 1
  sizeOf _ = 3
  peek ptr =
    CColor <$> peekByteOff ptr 0 <*> peekByteOff ptr 1 <*> peekByteOff ptr 2
  poke ptr (CColor d c i) = do
    pokeByteOff ptr 0 d
    pokeByteOff ptr 1 c
    pokeByteOff ptr 2 i

type Point = (Float, Float)

type Image a = Point -> a

type Region = Image Bool

type Filter c = Image c -> Image c

type FilterC = Filter Color

type ImageC = Image Color

renderRegion :: Region -> Image Color
renderRegion r p =
  if r p
    then black
    else white

cOver :: Color -> Color -> Color
cOver (Color r1 g1 b1 a1) (Color r2 g2 b2 a2) =
  Color (h r1 r2) (h g1 g2) (h b1 b2) (h a1 a2)
  where
    h x1 x2 = x1 + (1 - a1) * x2

bilerpBRBW = bilerpC black red blue white

bilerpC ll lr ul ur (wx, wy) = lerpC wy (lerpC wx ll lr) (lerpC wx ul ur)

over :: ImageC -> ImageC -> ImageC
top `over` bot = \p -> top p `cOver` bot p

instance Semigroup Color where
  (<>) = cOver

instance Monoid Color where
  mempty = invisible

invisible = Color 0 0 0 0

black = Color 0 0 0 1

white = Color 1 1 1 1

red = Color 1 0 0 1

green = Color 0 1 0 1

blue = Color 0 0 1 1

yellow = Color 1 1 0 1

cond :: Image Bool -> Image c -> Image c -> Image c
cond =
  liftA3
    (\a b c ->
       if a
         then b
         else c)

lerpI :: Image Frac -> ImageC -> ImageC -> ImageC
lerpI = liftA3 lerpC

empty = const invisible

whiteI = const white

blackI = const black

redI = const red

blueI = const blue

greenI = const green

yellowI = const yellow

blackWhiteIm reg = cond reg blackI whiteI

blueYellowIm reg = cond reg blueI yellowI

crop :: Region -> FilterC
crop reg im = cond reg im mempty

type Warp = Point -> Point

type Vector = (Float, Float)

translateP :: Vector -> Warp
translateP (dx, dy) (x, y) = (x + dx, y + dy)

scaleP :: Vector -> Warp
scaleP (sx, sy) (x, y) = (sx * x, sy * y)

uscaleP :: Float -> Warp
uscaleP s = scaleP (s, s)

rotateP :: Float -> Warp
rotateP θ (x, y) = (x * cos θ - y * sin θ, y * cos θ + x * sin θ)

udisk :: Region
udisk p = distO p < 1

invWarp warp im = im . warp

translate :: Vector -> Filter c
translate (dx, dy) = invWarp (translateP (-dx, -dy))

scale :: Vector -> Filter c
scale (sx, sy) = invWarp (scaleP (1 / sx, 1 / sy))

-- Laws
-- uscale x . uscale y = uscale (x * y)
uscale :: Float -> Filter c
uscale s = invWarp (uscaleP (1 / s))

rotate :: Float -> Filter c
rotate θ = invWarp (rotateP (-θ))

swirlP :: Float -> Warp
swirlP r p = rotateP (distO p * (2 * pi / r)) p

swirl :: Float -> Filter c
swirl r = invWarp (swirlP (-r))

square n (x, y) = abs x <= n / 2 && abs y <= n / 2

-- Figure to visually check orientation
disks =
  renderRegion $
  f <$> udisk <*> translate (2, 2) udisk <*> translate (3, 2) udisk <*> vstrip
  where
    f a b c d = a || b || c || d

-- Figures from "Functional Images" by Conal Elliot.
fig1 = renderRegion $ vstrip

fig2 = renderRegion $ checker

fig3 = renderRegion $ altRings

fig4 = renderRegion $ polarChecker 10

fig5 = renderRegion $ uscale 0.05 gasket

fig6 = renderFrac $ wavDist

fig7 = bilerpBRBW

fig8 = lerpI wavDist (blackWhiteIm (polarChecker 10)) (blueYellowIm checker)

fig9 = ybRings

fig10 = renderRegion udisk

fig11 = renderRegion $ swirl 1 vstrip

fig13 t = renderRegion (swirlingXPos t)

fig14 = renderRegion $ annulus 0.5

fig17 = renderRegion $ shiftXor 2.6 altRings

fig18 = renderRegion $ xorgon 8 (7 / 4) altRings

-- Typo in original paper, 0.2 5 should be 0.25
fig19 = crop (wedgeAnnulus 0.25 10) ybRings

fig20 = crop (swirl 2 (wedgeAnnulus 0.25 10)) ybRings

fig22 = translate (-20, -20) tiledBilerp

fig23 = renderRegion $ uscale 3.5 (radInvert checker)

fig24 = rippleRad 8 0.3 ybRings

fig25 = \t -> rippleRad 8 (cos t / 2) ybRings

fig26 = rippleRad 8 0.3 $ cropRad 1 $ ybRings

fig27 = cropRad 1 $ rippleRad 8 0.3 $ ybRings

fig28 = swirl 8 $ rippleRad 5 0.3 $ cropRad 5 $ ybRings

-- Typo in original paper, washer (1/2) (pi/2) 1 tiledBilerp
-- TODO: Fix visual mismatch with paper.
fig32 = washer (1 / 2) (pi / 2) tiledBilerp

-- Convert an image to a static animation.
static :: Image c -> Anim c
static = const

-- The main animation to run.
-- mainAnim = static . renderFrac $ mandel
mainAnim :: Anim Color
mainAnim = fig32

calculate (x, y, t) = adjust (mainAnim t') (x', y')
  where
    duration = 5000 -- Duration of the animation in ms
    range = 2 * pi -- Time range of the animation
    -- Time normalized from 0 to 1
    normalizedTime = fromIntegral (t `mod` duration) / fromIntegral duration
    scaledTime = range * normalizedTime
    -- scaledTime = fromIntegral t / 10000
    (x', y', t') = (fromIntegral x, fromIntegral y, scaledTime)
    adjust = adjustToWindow
    -- Adjust an image to a window, by translating, scaling and flipping
    adjustToWindow :: Filter c
    adjustToWindow =
      translate (screenWidth / 2, screenHeight / 2) .
      uscale 60 . flipY
    flipY p (x, y) = p (x, -y)

(screenWidth, screenHeight) = (640, 480)

foreign export ccall fillPixelBuffer :: Ptr CColor -> CInt -> IO ()
fillPixelBuffer arr t = pokeArray arr (map calc l1)
  where
    l1 = [(x, y) | y <- [0 .. screenHeight - 1], x <- [0 .. screenWidth - 1]]
    calc (x, y) =
      CColor (truncate (255 * r)) (truncate (255 * g)) (truncate (255 * b))
      where
        Color r g b _ = calculate (x, y, t)

checker :: Region
checker (x, y) = even (floor x + floor y)

vstrip :: Region
vstrip (x, y) = abs x <= 1 / 2

overlay :: ImageC -> ImageC -> ImageC
overlay = liftA2 (<>)

-- Sierpinski triangle
gasket :: Region
gasket (x, y) = floor x .|. floor y == (floor x :: Integer)

altRings p = even (floor (distO p))

distO (x, y) = sqrt (x * x + y * y)

type Frac = Float -- in [0, 1]

wavDist :: Image Frac
wavDist p = (1 + cos (pi * distO p)) / 2

ybRings = lerpI wavDist blueI yellowI

renderFrac :: Image Frac -> ImageC
renderFrac f (x, y) = lerpC (f (x, y)) black white

lerpC :: Frac -> Color -> Color -> Color
lerpC w (Color r1 g1 b1 a1) (Color r2 g2 b2 a2) =
  Color (h r1 r2) (h g1 g2) (h b1 b2) (h a1 a2)
  where
    h x1 x2 = w * x1 + (1 - w) * x2

lighten x c = lerpC x c white

darken x c = lerpC x c black

type PolarPoint = (Float, Float)

fromPolar :: Point -> PolarPoint
fromPolar (ρ, θ) = (ρ * cos θ, ρ * sin θ)

toPolar :: PolarPoint -> Point
toPolar (x, y) = (distO (x, y), atan2 y x)

polarChecker :: Int -> Region
polarChecker = polarize checker

polarize :: Image c -> Int -> PolarPoint -> c
polarize pat n = pat . sc . toPolar
  where
    sc (ρ, θ) = (ρ, θ * n' / π)
    π = pi
    n' = fromIntegral n

-- Animations
type Time = Float

type Anim c = Time -> Image c

swirlingXPos :: Anim Bool
swirlingXPos t = swirl (t * t) xPos

xPos :: Region
xPos (x, y) = x > 0

-- Region algebra
intersect :: Region -> Region -> Region
intersect = liftA2 (&&)

union :: Region -> Region -> Region
union = liftA2 (||)

xorR :: Region -> Region -> Region
xorR = liftA2 xor
  where
    xor True b = not b
    xor _ b = b

compR :: Region -> Region
compR = fmap not

universeR :: Region
universeR = const True

emptyR :: Region
emptyR = const False

r \\ r' = r `intersect` compR r'

annulus :: Frac -> Region
annulus inner = udisk \\ uscale inner udisk

radReg :: Int -> Region
radReg n = test . toPolar
  where
    test (r, a) = even (floor (a * fromIntegral n / pi))

wedgeAnnulus :: Frac -> Int -> Region
wedgeAnnulus inner n = annulus inner `intersect` radReg n

shiftXor :: Float -> Region -> Region
shiftXor r reg = reg' r `xorR` reg' (-r)
  where
    reg' d = translate (d, 0) reg

-- Typo in the original paper, missing argument g
xorgon :: Int -> Float -> Region -> Region
xorgon n r g = xorRs (rf <$> [0 .. n - 1])
  where
    rf :: Int -> Region
    rf i = translate (fromPolar (r, a)) g
      where
        a = fromIntegral i * 2 * pi / fromIntegral n

xorRs :: [Region] -> Region
xorRs = foldr xorR emptyR

tileP :: Vector -> Warp
tileP (w, h) (x, y) = (wrap' w x, wrap' h y)

tiledBilerp = about (1 / 2, 1 / 2) (tile (1, 1)) bilerpBRBW

type HyperFilter c = Filter c -> Filter c

about :: Point -> HyperFilter c
about (x, y) filt = translate (x, y) . filt . translate (-x, -y)

wrap :: Float -> Float -> Float
wrap w x = w * fracPart (x / w)
  where
    fracPart t = t - fromIntegral (truncate t)

wrap' :: Float -> Float -> Float
wrap' w x = wrap w (x + w / 2) - w / 2

tile :: Vector -> Filter c
tile size = invWarp (tileP size)

swirlP' r = polarWarp (\(ρ, θ) -> (ρ, θ + ρ * (2 * pi / r)))

polarWarp warp = fromPolar . warp . toPolar

radInvertP :: Warp
radInvertP = polarWarp (\(ρ, θ) -> (1 / ρ, θ))

radInvert :: Filter c
radInvert = invWarp radInvertP

rippleRadP :: Int -> Float -> Warp
rippleRadP n s =
  polarWarp $ (\(ρ, θ) -> (ρ * (1 + s * sin (fromIntegral n * θ)), θ))

rippleRad :: Int -> Float -> Filter c
rippleRad n s = invWarp (rippleRadP n (-s))

cropRad :: Float -> FilterC
cropRad r = crop (uscale r udisk)

wiggleRotateP :: Float -> Float -> Time -> Warp
wiggleRotateP cycles θmax t = polarWarp warp
  where
    warp (r, a) = (r, a + θmax * sin (t + dt))
      where
        dt = 2 * pi * cycles * (r - 1 / 2)

wiggleRotate :: Float -> Float -> Time -> Filter c
wiggleRotate cycles θmax t = invWarp (wiggleRotateP cycles θmax t)

washer :: Float -> Float -> ImageC -> Time -> ImageC
washer cycles θmax im t = cropRad 1 $ wiggleRotate cycles θmax t $ im

-- Constants
maxIter :: Int -- Max iterations
maxIter = 750

width :: Int -- Image width
height :: Int -- Image height
width = 400

height = 400

-- Note: aspect ratio of (minX, minY), (maxX, maxY) must
-- match aspect ratio of (width, height)
minX :: Float -- Min x-coordinate of graph
maxX :: Float -- Max x-coordinate of graph
minY :: Float -- Min y-coordinate of graph
maxY :: Float -- Max y-coordinate of graph
-- For the zoomed in part of the Mandelbrot:
--minX = -0.826341244461360116
--maxX = -0.8026423086165848822
--minY = -0.2167936114403439588
--maxY = -0.193094675595568725
--For a full view of the mandelbrot
minX = -2.5

maxX = 1.5

minY = -2

maxY = 2

-- The actual fractal part. It basically works on a matrix, which we
-- will call M, that represents a grid of points on the
-- graph. Essentially, M[i, j] is (xList[j], yList[i])
xList :: [Float]
yList :: [Float]
xList = [minX,(minX + ((maxX - minX) / (fromIntegral width - 1))) .. maxX]

yList =
  reverse [minY,(minY + ((maxY - minY) / (fromIntegral height - 1))) .. maxY]

etaFraction :: Complex Float -> Float
etaFraction z = (log (log (magnitude z))) / log 2

smoothEta :: Int -> Complex Float -> Float -- Smooth escape time algorithm value
smoothEta iter z = (fromIntegral iter - etaFraction z) / fromIntegral maxIter

-- Gets the color for the point, in range [0, 1]
color :: Int -> Complex Float -> Float
color iter z = smoothEta iter z -- Smooth escape time algorithm (and invert)

-- color iter z = fromIntegral iter / fromIntegral maxIter
interpolate :: Float -> Int -- Adds an interpolation curve for interpolating color
interpolate v = truncate ((v ^ 12) * 255) -- Polynomial curve

-- interpolate v = chr (truncate(v * 255)) -- Linear
-- The actual fractal algorithm!
frac :: Complex Float -> Complex Float -> Int -> Int
frac c@(x :+ y) z iter
  | iter >= maxIter = 255 -- never escaped, return color value of 255
  | let p = sqrt ((x - 0.25) ^ 2 + y ^ 2)
     in x <= p - 2 * p ^ 2 + 0.25 && (x + 1) ^ 2 + y ^ 2 <= 0.625 = 255
  | otherwise =
    let z' = z * z + c
     in if ((realPart z') * (realPart z') + (imagPart z') * (imagPart z')) > 4
          then interpolate (color iter z')
          else frac c z' (iter + 1)

mandel :: Image Frac
mandel (x, y) = fromIntegral (frac (x :+ y) (0 :+ 0) 0) / 255

-- dotGridGradient :: Int -> Int -> Float -> Float -> IO Float
-- dotGridGradient ix iy x y = do
--   (v1, v2) <- do p' <- randomIO :: IO Float
--                  let (_, p) = properFraction p'
--                  pure (p, sqrt (1 - p ^ 2))
--   let c1 = dx * v1
--   let c2 = dy * v2
--   let res = (c1 + c2)
--   -- print res
--   pure res
--   where
--     dx = x - fromIntegral ix
--     dy = y - fromIntegral iy

-- perlin :: Image (IO Frac)
-- perlin (x, y) = do
--   let (x0, sx) = properFraction x
--   let (y0, sy) = properFraction y
--   let x1 = x0 + 1
--   let y1 = y0 + 1
--   n0 <- dotGridGradient x0 y0 x y
--   n1 <- dotGridGradient x1 y0 x y
--   let ix0 = lerp n0 n1 sx
--   n0' <- dotGridGradient x0 y1 x y
--   n1' <- dotGridGradient x1 y1 x y
--   let ix1 = lerp n0' n1' sx
--   let res' = lerp ix0 ix1 sy
--   let res = abs res'
--   pure res
--   where
--     lerp a0 a1 w = (1.0 - w) * a0 + w * a1
