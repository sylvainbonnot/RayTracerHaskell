import RTCTuple
import Color
import Canvas
import Matrix
import Transform

clr::Color 
clr = Color (1.0, 1.0, 1.0)

hour_inc::Float
hour_inc = 2.0 * pi / 12.0 


draw_clock:: Int -> Int -> Canvas 
draw_clock width height = loop (Canvas.createCanvas width height) 0
    
center_clock:: Int-> Int->(Int, Int, RTCTuple Float)
center_clock width height = (cx, cy, twelve)
        where cx = div width  2
              cy = div height 2
              twelve = (RTCTuple.point 0.0 ((fromIntegral cy) * 0.8) 0.0)

coord_point::Int ->Int -> RTCTuple Float -> (Int, Int)
coord_point cx cy point= (x1,x2)
    where x1 = cx + floor((x point))
          x2 = cy - floor(y point)

pointFromHr::Int -> RTCTuple Float -> RTCTuple Float
pointFromHr hr twelve = Matrix.tmult tx twelve
    where tx = Transform.rotation_z (hour_inc * fromIntegral hr)

loop::Canvas -> Int -> Canvas
loop canvas hr = if (hr==12) then canvas else loop (Canvas.writePixelAt canvas x y clr) (hr+1)
    where (x, y) =  coord_point cx cy point
          (cx, cy, twelve) = center_clock (width canvas) (height canvas)
          point = pointFromHr hr twelve

main = do
    
    let ppm = Canvas.ppmFromCanvas (draw_clock 100 100)  
    let filename = "04-clock.ppm" 
    writeFile filename ppm
    putStrLn "successfully written"