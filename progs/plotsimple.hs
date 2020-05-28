
import RTCTuple
import Color
import Canvas
import Projectile


        	
     


main = do
    --let c = createCanvas 50 100
    let colorRed = Color (1, 0, 0)
    let clr  = Color (1.0, 0.7, 0.7)

    let grav = RTCTuple.vector 0.0 (0.0) 0.0 
    let wind = RTCTuple.vector (0.0) 0.0 0.0 
    let pos  = RTCTuple.point 0.0 0.0 0.0 
    let vel  = RTCTuple.mults (RTCTuple.norm (RTCTuple.vector 1.0 0.0 0.0)) 1.0 
    let c    = Canvas.createCanvas 20 10
    let proj = Projectile { position = pos, velocity=vel}

    let pos = position proj 
    let yy = (-1 +height c) - floor ((y pos) + 0.5)
    print ("y=" ++ show (floor ((y pos) + 0.5)))
    let xx = floor ((x pos) + 0.5) 
    print ("x=" ++ show (floor ((x pos) + 0.5)))

    let c' = writePixelAt c xx yy clr
    let env = Environment { gravity= grav, wind = wind}
    


    --let d = writePixelAt c xx yy clr
    let proj2 = tick env proj
    let pos2 = position proj2 
    let yy2 = (-1 + height c) - floor ((y pos2) + 0.5) 
    print ("y2=" ++ show (floor ((y pos2) + 0.5)))
    let xx2 = floor ((x pos2) + 0.5) 
    print ("x2=" ++ show (floor ((x pos2) + 0.5)))
    let e = writePixelAt c' xx2 yy2 clr


    let ppm  = ppmFromCanvas e
    print ppm

    let filename = "02-plotsimple.ppm" 
    writeFile filename ppm
    putStrLn "successfully written"