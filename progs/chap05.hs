import RTCTuple
import Color
import Canvas
import Matrix
import Transform
import Sphere
import Ray


color = Color ( 1.0, 0.0, 0.0)

drawHit:: Canvas -> Int-> Int -> Maybe Intersection -> Canvas
drawHit canvas x y hit = if (hit == Nothing) then canvas else newCanvas
                       where newCanvas  = writePixelAt canvas x y color
 
canvasToWorld:: Float->Float->Int-> Int->(Float, Float)
canvasToWorld half pixel_size x y = (xw, yw)
                             where xw = -half + pixel_size*(fromIntegral x)
                                   yw = half - pixel_size*(fromIntegral y)

drawSphere::Sphere-> Float-> Float -> Int-> Canvas 
drawSphere sphere ray_z wall_z canvas_size =
  let canvas = createCanvas canvas_size canvas_size in
  let (ray_origin, wall_size, pixel_size, half)= paramsPlot ray_z wall_z (width canvas) in
  let  renderRow canvas y = if  (y>= (height canvas)) then canvas else renderPixel canvas y 0 
       renderPixel canvas y x = if (x >= width canvas) then renderRow canvas (y+1)
                              else renderPixel newCanvas y (x+1)
                               where (world_x, world_y) = canvasToWorld half pixel_size x y
                               	     position = RTCTuple.point world_x world_y wall_z 
                                     direction = RTCTuple.norm (RTCTuple.subtract position ray_origin)
                                     r = Ray.build ray_origin direction
                                     xs = Sphere.intersect sphere r
                                     newCanvas = drawHit canvas x y (Sphere.hit xs) 
   in

  renderRow canvas 0

paramsPlot::Float-> Float -> Int->(RTCTuple Float, Float, Float, Float)
paramsPlot ray_z wall_z canvas_size = (ray_origin, wall_size, pixel_size, half)
                                    where ray_origin = RTCTuple.point 0.0 0.0 ray_z
                                          wall_size = 2.0 * (wall_z - ray_z) / (abs ray_z) + 1.0
                                          pixel_size = wall_size / (fromIntegral canvas_size)
                                          half = wall_size / 2.0


main:: IO () 
main = do
  let sphere = Sphere {sphereId = 0, Sphere.transform = Matrix.identity}
  let canvas = drawSphere sphere (-5.0) 10.0 100
  let ppm = Canvas.ppmFromCanvas canvas 
  let filename = "05-sphere.ppm" 
  writeFile filename ppm
  putStrLn "successfully written"