module Matrix where



import Data.Array
import RTCTuple

type Matrix a =  Array (Int, Int) a

epsilon::Float
epsilon = 0.000001

identity::Matrix Float
identity = createMatrixFromList 4 4 [[1.0, 0.0, 0.0, 0.0::Float],
                                 [0.0, 1.0, 0.0, 0.0::Float],
                                 [0.0, 0.0, 1.0, 0.0::Float],
                                 [0.0, 0.0, 0.0, 1.0::Float]]


--createMatrixFromList ::  (Num a)=> Int -> Int -> [[a]]-> Matrix  a
createMatrixFromList ::  Int -> Int -> [[Float]]-> Matrix  Float
createMatrixFromList r c l= listArray ((0,0),(r-1,c-1)) [(l !! i) !!j| i<-[0..r-1], j<-[0..c-1]]

createMatrixFromTuple ::  RTCTuple Float-> Matrix  Float
createMatrixFromTuple t= listArray ((0,0),(3,0)) [x t, y t, z t, w t]

equal:: Matrix Float -> Matrix Float -> Bool
equal c d = (bounds c ==bounds d) && and (zipWith RTCTuple.approxEq (elems c) (elems d))


        

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

tmult :: Matrix Float -> RTCTuple Float-> RTCTuple Float
tmult m t = RTCTuple {x = prodList !! 0, y=prodList !! 1, z=prodList !! 2, w=prodList !! 3}
            where  tmatrix = createMatrixFromTuple t
                   prodList = elems (matMult m tmatrix)


swap:: (a,b)->(b,a)
swap (x,y) = (y,x)
 
transpArray :: (Ix a, Ix b) => Array (a,b) e -> Array (b,a) e
transpArray a = ixmap (swap l, swap u) swap a where 
  (l,u) = bounds a

removeRC :: Int -> Int -> ((Int, Int), Float)-> Bool
removeRC r c ((a,b), f) = not(a==r || c==b)

decreaseBounds :: ((Int, Int),(Int, Int))-> ((Int, Int),(Int, Int))
decreaseBounds ((a, b), (c, d)) = ((a,b),(c-1, d-1))

submatrix:: Matrix Float -> Int -> Int -> Matrix Float
submatrix m r c = listArray (decreaseBounds ( bounds m)) items
                          where as = assocs m
                                sublist = filter (removeRC r c) as
                                items = map snd sublist
                            	    	

determinant :: Matrix Float -> Float
determinant m | (length m) == 4 = (m ! (0,0))*(m ! (1,1)) -(m ! (1,0))*(m ! (0,1))
              | otherwise = sum [(m !(0,c))*cofactor m 0 c| c <- [0..col]] 
                           where ((a,b),(c,d)) = bounds m
                                 col = d-b

minor :: Matrix Float-> Int-> Int-> Float
minor m r c= determinant (submatrix m r c)

cofactor :: Matrix Float-> Int-> Int-> Float
cofactor m r c= ((-1)^r)*((-1)^c)*minor m r c

isInvertible :: Matrix Float -> Bool
isInvertible m = not (RTCTuple.approxEq (determinant m) 0.0) 

inverse :: Matrix Float -> Matrix Float
inverse m | not (isInvertible m) = error "matrix is not invertible"
          | otherwise = array (bounds m) [((j,i),(cofactor m i j)/det)| i<-[a..c], j<-[b..d]]
                      where ((a,b),(c,d)) = bounds m
                            det = determinant m
