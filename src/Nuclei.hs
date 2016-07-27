module Nuclei(
  AtomicNumber, Nucleus, Nuclei 
) where


-- A typedef around the atomic number - ie the number of protons of a nucleus
-- Should always be positive.
type AtomicNumber = Int

-- A nucleus is specified as a position and atomic number
data Nucleus = Nucleus Double Double Double AtomicNumber


-- A typedef for an arrangment of nuclie given as a simple list of 
-- several Nuclei.
type Nuclei = [Nucleus] 
