import RTCTuple
import Color
import Canvas
import Projectile

trajectory::Canvas->Color -> Environment-> Projectile -> Canvas
trajectory c clr env proj = if y (position proj) <= 0.0 then c else 
        let pos = position proj in
    	let yy = (height c) - floor ((y pos) + 0.5) in
        let xx = floor ((x pos) + 0.5) in
        let cond = yy >= 0 && yy < (height c) && xx >= 0 && xx < (width c ) in
        if cond then trajectory (Canvas.writePixelAt c xx yy clr) clr env (tick env proj)
        	    else c        

trajectory2::Int->Canvas->Color -> Environment-> Projectile -> Canvas
trajectory2 counter c clr env proj = 
    if counter>1 then c else 
 
        let pos = position proj in
    	let yy = (height c) - floor ((y pos) + 0.5) in
        let xx = floor ((x pos) + 0.5) in
        let cond = yy >= 0 && yy < (height c) && xx >= 0 && xx < (width c ) in
        if cond then trajectory2 (counter+1) (Canvas.writePixelAt c xx yy clr) clr env (tick env proj)
        	    else c        
        	
     


main = do
	
	let grav = RTCTuple.vector 0.0 (0.0) 0.0 
	let wind = RTCTuple.vector (0.0) 0.0 0.0 
	let pos  = RTCTuple.point 0.0 1.0 0.0 
	let vel  = RTCTuple.mults (RTCTuple.norm (RTCTuple.vector 1.0 0.0 0.0)) 5.0 
	let c    = Canvas.createCanvas 20 10 
	let clr  = Color (1.0, 0.7, 0.7)
	let env = Environment { gravity= grav, wind = wind}
	let proj = Projectile { position = pos, velocity=vel}
	let ppm  = ppmFromCanvas (trajectory2 1 c clr env proj) 
	let filename = "02-trajectory.ppm" 
	writeFile filename ppm
	putStrLn "successfully written"

--let y = c.height - (int_of_float (pos.y +. 0.5)) in
--       let x = int_of_float (pos.x +. 0.5) in
--  begin
--    Printf.printf "(x,y) = (%d,%d)\n" x y;
 --   if y >= 0 && y < c.height && x >= 0 && x < c.width then RTCCanvas.write_pixel c x y clr;
 --   loop c clr env (tick env proj)
 -- end