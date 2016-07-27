module BasisSets(
  FunctionBasisSet,
  FunctionBasisIndex, FunctionBasisIndexPair, FunctionBasisIndexQuad
) where

import Nuclei(Nuclei)

import Data.Eigen.Matrix(MatrixXd)

-- An interface for the basis set of functions used to generate orbitals.
--Used to hide the backend for the integral components.

-- A typedef around an Int to represent the index of a single electron basis function in some set
-- This isnt made more rigorous as it may make the code to create matrices unwieldy
type FunctionBasisIndex = Int 

-- A typedef to represent a pair of single electron basis functions in a set
-- Often used for exchange / coulomb integrals
type FunctionBasisIndexPair = (FunctionBasisIndex, FunctionBasisIndex)

-- A typedef to represent 4 single electron basis functions in a set
-- Often used to index the generalInteraction integral
type FunctionBasisIndexQuad = (FunctionBasisIndexPair, FunctionBasisIndexPair)

class FunctionBasisSet basis where
    -- The number of basis functions in the set.
    -- Indices will only be valid if 0 <= index < size.
    size :: basis -> Int
    -- Get the core hamaltonion matrix,
    -- H(i,j) = intergral of f_i*(x) h_R f_i(x) dx
    -- For the single electron hamiltonian for the nuclei in R.
    coreHamaltonion :: basis -> Nuclei -> MatrixXd
    -- Get the exchange integral of the 2 basis functions (i,i) that is
    -- the integral f_i*(x1)f_j(x1) r(x1,x2) f_j*(x2)f_i(x2) dx1 dx2
    -- Where r(x1,x2) is the reciprocal of the distance squared between x1 and x2. 
    -- Should be equivalent to generic_integral ((i,j),(i,j))
    exchange :: basis -> FunctionBasisIndexPair -> Double
    exchange basis (i,j) = generalInteraction basis ((i,j),(i,j))
    -- Get the coulomb integral of the 2 basis functions (i,j) that is
    -- the integral of |f_i(x1)|^2 r(x1,x2) |f_j(x2)|^2 dx1 dx2
    -- Where r(x1,x2) is the reciprocal of the distance squared between x1 and x2. 
    -- Should be equivalent to generalInteraction basis ((i,i),(j,j))
    coulomb :: basis -> FunctionBasisIndexPair -> Double
    coulomb basis (i,j) = generalInteraction basis ((i,i),(j,j))
    -- Get the generic integral of the 4 basis functions ((a,b),(i,j)) that is
    -- the integral f_a*(x1)f_b(x1) r(x1,x2) f_j*(x2)f_i(x2) dx1 dx2
    generalInteraction :: basis -> FunctionBasisIndexQuad -> Double
    -- Get the overlap integral this is a size * size - matrix such that
    -- S(i,j) = integral f_i(x) f_j*(x) dx
    overlap_matrix :: basis -> MatrixXd
    
    
    
    
