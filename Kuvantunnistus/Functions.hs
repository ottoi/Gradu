{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Functions where

import Data.Word
import Data.Int (Int32)
import Data.Vector as V hiding (convert, (++))

import Vision.Image as I
import Vision.Image.Class 
import Vision.Image.Conversion
import Vision.Primitive (Z (..), (:.) (..), Rect (..), ix2, Point)
import Vision.Detector.Edge (canny)


--------------------------------------------------------------------------------

-- Method one --

-- Finds the birghtest pixel in each colum.
-- This is the simplest way do the interface detection.
brightestPixels :: Word8 -> Grey -> Vector (Maybe Int)
brightestPixels cutoffValue grey = V.generate w fun
   where
        Z :. h :. w = shape grey
        fun i = rec i 0 Nothing
        rec :: Int -> Int -> Maybe Int -> Maybe Int
        rec x y maxY
            | y >= h-1 = maxY
            | (maxP maxY) < p && cutoff <= p = rec x (y+1) (Just y)
            | (maxP maxY) < p = rec x (y+1) maxY 
            | (maxP maxY) >= p = rec x (y+1) maxY
            where
                maxP Nothing = GreyPixel (0 :: Word8)
                maxP (Just yy) = pixel' grey yy x
                p = pixel' grey y x
        cutoff =  GreyPixel (cutoffValue)

--------------------------------------------------------------------------------    

-- Method two --

-- Finds the front of the interface.
-- Assumes that the edgeDetection function finds correctly 
-- the position of the interface front and, don't leave any disruptions
-- under the detected edge.
-- TODO: Fix the second assumption
frontOfTheInterface :: Int -> Int32 -> Int32 -> Grey -> Vector (Maybe Int)
frontOfTheInterface rad lT hT src = 
    if(crop) == (0,0)
    then V.generate w (\a -> Nothing) 
    else firstBrightPixel y0 cutoff edges
        where
            Z:.h:.w = shape src
            edges = edgeDetection rad lT hT cropped :: Grey
            (cropped,y0) = cropToInterface crop highContrast
            crop = cropFun cutoff highContrast
            highContrast = contrastCutoff (150 :: Word8) src
            cutoff = 200 :: Word8

-- Finds the first bright pixel, which have one bright pixel behind.
-- Starts from the lower left corner (h-1)
-- Both pixels value have to be more than 200. 
-- TODO: Ota huomioon mahdolliset rintaman alapuolella olevat häiriöt.
-- Esim. jonkinäköinen maski jolla etsitään pisimmät yhtenäiset viivat.
firstBrightPixel :: Int -> Word8 -> Grey -> Vector (Maybe Int)
firstBrightPixel y0 cf src = V.generate w fun
        where
            Z :. h :. w = shape src
            -- Starts from lower left corner (h-1)
            fun i = rec i (h-1) True Nothing
            rec :: Int -> Int -> Bool -> Maybe Int -> Maybe Int
            rec x y isFirst firstY
                | y == 1 = Nothing --firstY
                | p >= cutoff &&  not isFirst = firstY
                | p >= cutoff && isFirst = 
                        rec x (y-1) False (Just $ (h-y)+y0)
                | otherwise = rec x (y-1) isFirst firstY
                where
                     p     =  pixel' src y x
            cutoff = GreyPixel cf

-- Detect edces by using Canny's algrithm
-- For more info check: 
-- https://hackage.haskell.org/package/friday-0.2.2.0/docs/Vision-Detector-Edge.html
edgeDetection :: Int -> Int32 -> Int32 -> Grey -> Grey
edgeDetection rad lTreshold hTreshold src = 
        -- Applies the Canny's algorithm with a 5x5 Sobel kernel (radius
        -- = rad).
        canny rad lTreshold hTreshold blurred
            where
                -- Applies a Gaussian filter with a 3x3 Double kernel to remove
                -- small noises.
                blurred = gaussianBlur 3 (Nothing:: Maybe Double) src :: Grey

cropToInterface :: (Int,Int) -> Grey -> (Grey,Int)
cropToInterface (starY, endY) src = (crop rect src, h-endY)
    where
        -- Creates a Rect object which will be used to define how we
        -- will crop our image. The rectangle is centered on the largest
        -- side of the image.
        Z :. h :. w = shape src
        rect  = Rect 0 (starY-10) w (deltaY+20)
        deltaY = endY - starY


contrastCutof :: Word8 -> Grey -> Grey
contrastCutof cutoff src = I.fromFunction size fun
    where
        fun :: Point -> GreyPixel
        fun = cutPixel cutoff' . I.index src 
        size = shape src
        cutoff' = GreyPixel cutoff
  
cutPixel :: GreyPixel -> GreyPixel -> GreyPixel      
cutPixel cutoff pixl  
   | pixl >= cutoff = pixl
   | otherwise = GreyPixel 0

--------------------------------------------------------------------------------
-- Helper functions --

pixel' :: Grey -> Int -> Int -> GreyPixel 
pixel' image y1 x1 = image I.! (ix2 y1 x1)


-- Finds the interface and gives minumum and maximum value for 
-- the y-coordinant
cropFun :: Word8 -> Grey -> (Int,Int) 
cropFun cutoff grey =  minMax (0,0) 0 0
    where
        Z :. h :. w = shape grey
        minMax yM@(yMin,yMax) x y
            | x >= w = yM  
            | y >= h = minMax yM (x+1) 0
            | p >= co && yM == (0,0) = minMax (y,y) x (y+1)
            | p >= co && y > yMax = minMax (yMin,y) x (y+1)
            | p >= co && y < yMin = minMax (y, yMax) x (y+1)
            | otherwise = minMax yM x (y+1)
            where
                p = pixel' grey y x
        co = GreyPixel (cutoff)

-- Unused
-- Ordering to help finding real minimum pixel value
minOrd :: Maybe Int -> Maybe Int -> Ordering
minOrd Nothing Nothing = EQ
minOrd Nothing _       = GT
minOrd _ Nothing       = LT
minOrd a b 
    | a < b = LT
    | a == b = EQ
    | a > b = GT

