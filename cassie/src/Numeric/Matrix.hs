module Numeric.Matrix 
    ( (!)
    , (!?)
    , augment
    , column
    , column'
    , cross
    , dot
    , dot'
    , fromList
    , ident
    , lupDecompose
    , matrixInv
    , matrixProd
    , matrixSum
    , matrixTranspose
    , row
    , row'
    , size
    , toMatrix
    , toMatrix'
    , Matrix(rows, cols, flatten)
    ) where

import Numeric.Matrix.Internal
import Numeric.Matrix.LU
