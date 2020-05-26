module Matrix where



import Data.Array


type Matrix a =  Array (Int, Int) a

createMatrixFromList ::  (Num a)=> Int -> Int -> [[a]]-> Matrix  a
createMatrixFromList r c l= listArray ((0,0),(r-1,c-1)) [(l !! i) !!j| i<-[0..r-1], j<-[0..c-1]]


matMult :: (Ix a, Ix b, Ix c, Num d) =>
                   Array (a,b) d -> Array (b,c) d -> Array (a,c) d
matMult x y     =  array resultBounds
                         [((i,j), sum [x!(i,k) * y!(k,j) | k <- range (lj,uj)])
                                       | i <- range (li,ui),
                                         j <- range (lj',uj') ]
        where ((li,lj),(ui,uj))         =  bounds x
              ((li',lj'),(ui',uj'))     =  bounds y
              resultBounds
                | (lj,uj)==(li',ui')    =  ((li,lj'),(ui,uj'))
                | otherwise             = error "matMult: incompatible bounds"