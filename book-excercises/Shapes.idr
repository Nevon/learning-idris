module Shapes

data Shape = ||| Triangle with base and height
             Triangle Double Double
           | ||| Rectangle with width and height
             Rectangle Double Double
           | ||| Circle with radius
             Circle Double
%name Shape shape, shape1, shape2

total area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle width height) = width * height
area (Circle radius) = pi * radius * radius

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture
%name Picture pic, pic1, pic2

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

data BiggestTriangle = NoTriangle | Size Double

biggestTriangle : Picture -> BiggestTriangle
biggestTriangle (Primitive shape) = if (isTriangle shape) then Size (area shape)
                                                          else NoTriangle
    where
        isTriangle : Shape -> Bool
        isTriangle (Triangle x y) = True
        isTriangle (Rectangle x y) = False
        isTriangle (Circle x) = False
biggestTriangle (Combine pic pic1) = pickLargest (biggestTriangle pic) (biggestTriangle pic1)
    where
        pickLargest : BiggestTriangle -> BiggestTriangle -> BiggestTriangle
        pickLargest NoTriangle NoTriangle = NoTriangle
        pickLargest NoTriangle (Size t) = Size t
        pickLargest (Size t) NoTriangle = Size t
        pickLargest (Size t) (Size t1) = Size (max t t1)
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 2 15)

triangle2 : Picture
triangle2 = Primitive (Triangle 5 23)

testPicture : Picture
testPicture = Combine (Translate 5 5 rectangle)
              (Combine (Translate 35 5 triangle2)
              (Translate 15 4 triangle))