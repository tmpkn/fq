{-# LANGUAGE ViewPatterns #-}

module Fq where

import Data.Map (Map)
import Data.List
import qualified Data.Map as Map


-- TYPE DEFINITIONS --
----------------------

-- Pure Qbits: |0> or |1>
type QBit = Integer

-- To represent probability amplitude, rather than using complex numbers, we are going to store
-- phase (deg. 0-360) and magnitude separately.
type Phase = Integer
type Value = Float

-- Phase and values combined. Using phase as a key makes it easier to calculate a vector sum and
-- the actual probability.
type PhaseValues = Map Phase Value

-- State of a 1-Qbit system, represented as a 2 item list (vector).
type QBitPhases = (QBit, PhaseValues)
type BinaryState = [QBitPhases]

-- Probabilities of a 1-Qbit system. The values should add to 1.00 (100%).
type StateProbs = [(QBit, Float)]

-- Circuit elements: gates, input and output (measurement).
data CirElem = GateH CirElem
    | GateX CirElem
    | GateY CirElem
    | GateZ CirElem
    | GateS CirElem
    | GateT CirElem
    | CInput QBit
    | COutput CirElem deriving (Show, Eq)

-- 1-Qbit circuit comprised of multiple elements chained together.
type Circuit = [CirElem]


-- HELPER FUNCTIONS --
----------------------

-- Add new Phase/Value pair to an existing map. If a given phase already exists, combine the magnitudes.
addPhaseValue :: Phase -> Value -> PhaseValues -> PhaseValues
addPhaseValue ph val pv =
    case Map.lookup ph pv of
        Just x -> Map.adjust (+ x) ph pv
        _ -> Map.insert ph val pv

-- Shift phases of a Phase/Value map.
shiftPhases :: PhaseValues -> Phase -> PhaseValues
shiftPhases pvs s = Map.fromList [(mod (phase + s) 360, value) | (phase, value) <- Map.toList $ pvs]

-- Merge 2 possible Phase/Value maps. Combine matching phases together.
mergeQBitPhases :: Maybe PhaseValues -> Maybe PhaseValues -> PhaseValues
mergeQBitPhases (Just qpa) (Just qpb) = Map.unionWith (+) qpa qpb
mergeQBitPhases Nothing (Just qpb) = qpb
mergeQBitPhases (Just qpa) Nothing = qpa
mergeQBitPhases _ _ = Map.fromList []

-- Merge 2 complete binary states. Phase/Value maps are merged separately for Qbits |0> and |1>.
mergeBinaryState :: BinaryState -> BinaryState -> BinaryState
mergeBinaryState bsa bsb =
    let qpa_0 = lookup 0 bsa
        qpa_1 = lookup 1 bsa
        qpb_0 = lookup 0 bsb
        qpb_1 = lookup 1 bsb
    in [(0, mergeQBitPhases qpa_0 qpb_0), (1, mergeQBitPhases qpa_1 qpb_1)]

-- Return an opposite Qbit
oppQBit :: QBit -> QBit
oppQBit 0 = 1
oppQBit 1 = 0

-- Calculate probability amplitude (i.e. produce a state vector) for a given circuit.
emitState :: CirElem -> BinaryState
emitState (CInput qb) = [(qb, Map.fromList [(0, 1)])]
emitState (COutput ce) = emitState ce
emitState (GateH ce) =
    let es = emitState ce
    in foldr (\x -> mergeBinaryState(emitGateH x)) [] es
emitState (GateX ce) =
    let es = emitState ce
    in foldr (\x -> mergeBinaryState(emitGateX x)) [] es
emitState (GateY ce) = emitGateY $ emitState ce
emitState (GateZ ce) =
    let es = emitState ce
    in foldr (\x -> mergeBinaryState(emitGateZ x)) [] es
emitState (GateS ce) =
    let es = emitState ce
    in foldr (\x -> mergeBinaryState(emitGateS x)) [] es
emitState (GateT ce) =
    let es = emitState ce
    in foldr (\x -> mergeBinaryState(emitGateT x)) [] es

-- Convert degrees to radians
degToRad :: Phase -> Float
degToRad d = df * (pi / 180)
    where df = fromIntegral d :: Float

-- Convert radians to degrees
radToDeg :: Float -> Phase
radToDeg r = round $ r * (180 / pi)

-- Add two vectors (Phase/Value pairs)
mergeVectors :: (Phase, Value) -> (Phase, Value) -> (Phase, Value)
mergeVectors (0, 0) b = b
mergeVectors a (0, 0) = a
mergeVectors (ap, av) (bp, bv) =
    let sx = (av * cos (degToRad ap)) + (bv * cos (degToRad bp))
        sy = (av * sin (degToRad ap)) + (bv * sin (degToRad bp))
        adjDeg = (case (sx < 0) of True -> 180
                                   False -> 0)
    in (mod ((radToDeg (atan (sy / sx))) + (adjDeg)) 360, sqrt (sx ** 2 + sy ** 2))

-- Merge a state by adding multiple vectors together (if they exist) for each Qbit.
mergeState :: BinaryState -> BinaryState
mergeState [] = []
mergeState bs = [(qb, reducePVS pv) | (qb, pv) <- bs]
    where reducePVS :: PhaseValues -> PhaseValues
          reducePVS pvs = Map.fromList $ [foldr mergeVectors (0, 0) (Map.toList pvs)]

-- Calculate Qbit probabilities for a given state.
calcProb :: BinaryState -> StateProbs
calcProb bs = [(qb, amplitudePV pv) | (qb, pv) <- (mergeState bs)]
    where amplitudePV :: PhaseValues -> Float
          amplitudePV pvs = foldr (\x -> (+) ((snd x) ** 2)) 0 (Map.toList pvs)

-- Circuit connection (read left to right)
(-:) :: a -> (a -> b) -> b
x -: f = f x


-- QASM DE/COMPILER --
----------------------

-- Represent a Circuit as a QASM code, to be deployed on physical quantum CPUs.
-- Usage: putStrLn $ parseElements $ circuit
parseElements :: CirElem -> String
parseElements (COutput ce) = 
    "qreg q[1];\n" ++ "qreg c[1];\n" ++ parseAux ce ""
    where parseAux (CInput qb) xs = "reset q[0];\n" ++ xs
          parseAux (GateH o) xs = parseAux o ("h q[0];\n" ++ xs)
          parseAux (GateX o) xs = parseAux o ("x q[0];\n" ++ xs)
          parseAux (GateY o) xs = parseAux o ("y q[0];\n" ++ xs)
          parseAux (GateZ o) xs = parseAux o ("z q[0];\n" ++ xs)
          parseAux (GateS o) xs = parseAux o ("s q[0];\n" ++ xs)
          parseAux (GateT o) xs = parseAux o ("t q[0];\n" ++ xs)

-- Parse a QASM code into a Circuit
readElements :: String -> CirElem
readElements (stripPrefix "qreg q[1];\n" -> Just xs) = readElements xs
readElements (stripPrefix "qreg c[1];\n" -> Just xs) = (COutput $ readAux xs)
    where readAux xs | "\nh q[0];\n" `isSuffixOf` xs = GateH $ readAux $ reverse $ drop 8 $ reverse xs
          readAux xs | "\nx q[0];\n" `isSuffixOf` xs = GateX $ readAux $ reverse $ drop 8 $ reverse xs
          readAux xs | "\ny q[0];\n" `isSuffixOf` xs = GateY $ readAux $ reverse $ drop 8 $ reverse xs
          readAux xs | "\nz q[0];\n" `isSuffixOf` xs = GateZ $ readAux $ reverse $ drop 8 $ reverse xs
          readAux xs | "\ns q[0];\n" `isSuffixOf` xs = GateS $ readAux $ reverse $ drop 8 $ reverse xs
          readAux xs | "\nt q[0];\n" `isSuffixOf` xs = GateT $ readAux $ reverse $ drop 8 $ reverse xs
          readAux xs | "reset q[0];\n" `isSuffixOf` xs = CInput 0


-- GATE FUNCTIONS --
--------------------
-- Most functions below emit probability amplitude for a given gate one Qbit value at a time, because
-- their corresponding tranformation matrices don't cross-depend on other Qbit's amplitudes.
-- A notable exception is the Pauli Y gate, which works on a complete state vector.

-- [H] Hadamard Gate
emitGateH :: QBitPhases -> BinaryState
emitGateH (qb, pvs)
    | qb == 0 = [(0, Map.map (* (sqrt 0.5)) pvs),
                 (1, Map.map (* (sqrt 0.5)) pvs)]
    | qb == 1 = [(0, Map.fromList [(phase, value * (sqrt 0.5)) | (phase, value) <- Map.toList $ pvs])] ++
                [(1, Map.fromList [(mod (phase + 180) 360, value * (sqrt 0.5)) | (phase, value) <- Map.toList $ pvs])]

-- [X] Pauli X Gate, aka Q-NOT
emitGateX :: QBitPhases -> BinaryState
emitGateX (qb, pvs) = [(oppQBit $ qb, pvs)]

-- [Y] Pauli Y Gate
emitGateY :: BinaryState -> BinaryState
emitGateY (bs) = [(0, qbs `shiftPhases` 270) | (q, qbs) <- bs, q == 1] ++
                 [(1, qbs `shiftPhases` (-270)) | (q, qbs) <- bs, q == 0]

-- [Z] Pauli Z Gate
emitGateZ :: QBitPhases -> BinaryState
emitGateZ (qb, pvs) 
    | qb == 0 = [(qb, pvs)]
    | qb == 1 = [(qb, pvs `shiftPhases` (-180))]

-- [S] S-Gate
emitGateS :: QBitPhases -> BinaryState
emitGateS (qb, pvs) 
    | qb == 0 = [(qb, pvs)]
    | qb == 1 = [(qb, pvs `shiftPhases` (-90))]

-- [T] T-Gate
emitGateT :: QBitPhases -> BinaryState
emitGateT (qb, pvs) 
    | qb == 0 = [(qb, pvs)]
    | qb == 1 = [(qb, pvs `shiftPhases` (-45))]


-- USE EXAMPLES --
------------------

-- Phase/Value map representing a 100% certainty with a 0 degree phase shift
one_nophase :: PhaseValues
one_nophase = Map.fromList [(0, 1)]

-- State Vector (BinaryState) representing a 50/50 distribution of Qbits randomly shifted
sure_zero :: BinaryState
sure_zero = [(0, Map.fromList [(45, sqrt(0.5))]),(1, Map.fromList [(135, sqrt(0.5))])]

-- Sample circuit with an input, gates and output measurement
circuit = (CInput 0) -: GateH -: GateH -: GateH -: GateH -: COutput

-- Sample QASM circuit which can be parsed into a Circuit
qasm = "qreg q[1];\nqreg c[1];\nreset q[0];\nh q[0];\nh q[0];\n"