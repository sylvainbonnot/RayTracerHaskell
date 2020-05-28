module Transform where


import Matrix


translation:: Float -> Float ->Float -> Matrix Float
translation x y z = Matrix.createMatrixFromList 4 4 [[1.0, 0.0, 0.0, x::Float],
                                 [0.0, 1.0, 0.0, y::Float],
                                 [0.0, 0.0, 1.0, z::Float],
                                 [0.0, 0.0, 0.0, 1.0::Float]]


scaling:: Float -> Float ->Float -> Matrix Float
scaling x y z = Matrix.createMatrixFromList 4 4 [[x, 0.0, 0.0, 0.0::Float],
                                 [0.0, y, 0.0, 0.0::Float],
                                 [0.0, 0.0, z, 0.0::Float],
                                 [0.0, 0.0, 0.0, 1.0::Float]]


rotation_x:: Float -> Matrix Float
rotation_x r = Matrix.createMatrixFromList 4 4 [[1.0, 0.0, 0.0, 0.0::Float],
                                 [0.0, cosr, (-sinr), 0.0::Float],
                                 [0.0, sinr, cosr, 0.0::Float],
                                 [0.0, 0.0, 0.0, 1.0::Float]]
                        where cosr = cos r
                              sinr = sin r

rotation_y:: Float -> Matrix Float
rotation_y r = Matrix.createMatrixFromList 4 4 [[cosr, 0.0, sinr, 0.0::Float],
                                 [0.0, 1.0, 0.0, 0.0::Float],
                                 [-sinr, 0.0, cosr, 0.0::Float],
                                 [0.0, 0.0, 0.0, 1.0::Float]]
                        where cosr = cos r
                              sinr = sin r

rotation_z:: Float -> Matrix Float
rotation_z r = Matrix.createMatrixFromList 4 4 [[cosr, -sinr, 0.0, 0.0::Float],
                                 [sinr, cosr, 0.0, 0.0::Float],
                                 [0.0, 0.0, 1.0, 0.0::Float],
                                 [0.0, 0.0, 0.0, 1.0::Float]]
                        where cosr = cos r
                              sinr = sin r

shearing :: Float -> Float -> Float -> Float -> Float -> Float -> Matrix Float
shearing xy xz yx yz zx zy = Matrix.createMatrixFromList 4 4 [[1.0, xy, xz, 0.0::Float],
                                 [yx, 1.0, yz, 0.0::Float],
                                 [zx, zy, 1.0, 0.0::Float],
                                 [0.0, 0.0, 0.0, 1.0::Float]]



