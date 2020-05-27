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