import Test.HUnit
import Fq
import Data.Map (Map)
import Data.List
import qualified Data.Map as Map

pvs_1 = Map.fromList [(90, 1 + sqrt(0.5)), (270, sqrt(0.5))]
pvs_2 = Map.fromList [(0, 0), (90, 1)]
pvs_1_plus_pvs_2 = Map.fromList [(0, 0), (90, 2 + sqrt(0.5)), (270, sqrt(0.5))]

vec_1 = (90, 3)
vec_2 = (180, 4)
vec_1_plus_vec_2 = (143, 5)

bs_1 = [(0,Map.fromList [(0,0.70710677)]),(1,Map.fromList [(0,0.70710677)])]

big_circuit = (CInput 0) -: GateH -: GateX -: GateY -: GateZ -: GateS -: GateT -: GateH -: COutput
big_qasm = "qreg q[1];\nqreg c[1];\nreset q[0];\nh q[0];\nx q[0];\ny q[0];\nz q[0];\ns q[0];\nt q[0];\nh q[0];\n"

-- AUX FUNCTIONS

test001 = TestCase (assertEqual "oppQBit" 1 (oppQBit 0))
test002 = TestCase (assertEqual "shiftPhases" (Map.fromList [(40, 0.1), (20, 0.9)])
    (shiftPhases (Map.fromList [(10, 0.1), (350, 0.9)]) 30))
test003 = TestCase (assertEqual "mergeQbitPhases a b" pvs_1_plus_pvs_2
    (mergeQBitPhases (Just pvs_1) (Just pvs_2)))
test004 = TestCase (assertEqual "mergeQbitPhases a 0" pvs_1 (mergeQBitPhases (Just pvs_1) Nothing))
test005 = TestCase (assertEqual "mergeQbitPhases 0 b" pvs_2 (mergeQBitPhases Nothing (Just pvs_2)))
test006 = TestCase (assertEqual "mergeQbitPhases 0 0" (Map.fromList []) (mergeQBitPhases Nothing Nothing))
test007 = TestCase (assertEqual "mergeVectors a b" vec_1_plus_vec_2 (mergeVectors vec_1 vec_2))
test008 = TestCase (assertEqual "mergeVectors a 0" vec_1 (mergeVectors vec_1 (0, 0)))
test009 = TestCase (assertEqual "mergeVectors 0 b" vec_2 (mergeVectors (0, 0) vec_2))
test010 = TestCase (assertEqual "mergeVectors 0 0" (0, 0) (mergeVectors (0, 0) (0, 0)))

-- GATE EMITTERS

test101 = TestCase (assertEqual "emitGateH 0"
                    [(0,Map.fromList [(0,sqrt(0.5))]),(1,Map.fromList [(0,sqrt(0.5))])]
                    (emitState $ (CInput 0) -: GateH -: COutput))
test102 = TestCase (assertEqual "emitGateX 0"
                    [(0,Map.fromList []),(1,Map.fromList [(0,1.0)])]
                    (emitState $ (CInput 0) -: GateX -: COutput))
test103 = TestCase (assertEqual "emitGateY 0"
                    [(1,Map.fromList [(90,1.0)])]
                    (emitState $ (CInput 0) -: GateY -: COutput))
test104 = TestCase (assertEqual "emitGateZ 0"
                    [(0,Map.fromList [(0,1.0)]),(1,Map.fromList [])]
                    (emitState $ (CInput 0) -: GateZ -: COutput))
test105 = TestCase (assertEqual "emitGateS 1"
                    [(0,Map.fromList []),(1,Map.fromList [(270,1.0)])]
                    (emitState $ (CInput 1) -: GateS -: COutput))
test106 = TestCase (assertEqual "emitGateT 1"
                    [(0,Map.fromList []),(1,Map.fromList [(315,1.0)])]
                    (emitState $ (CInput 1) -: GateT -: COutput))
test107 = TestCase (assertEqual "calcProb bs_1" 1000
    (round $ (foldr (\x -> (+) (snd x)) 0 (calcProb bs_1)) * 1000))

-- QASM

test201 = TestCase (assertEqual "parseElements bc" big_qasm (parseElements big_circuit))
test202 = TestCase (assertEqual "readElements bq" big_circuit (readElements big_qasm))

-- INTEGRATION TESTING
-- We now test the code in its entirety by calculating probabilities of |0> and |1> for
-- a complete circuit given as a QASM. Refernce values provided by IBM Quantum Composer.
-- https://quantum-computing.ibm.com/composer/

test901 = TestCase (assertEqual "QuantumComposer |0>" 1464
    (round $ 10000 * (snd $ (calcProb $ emitState $ readElements big_qasm) !! 0)))
test902 = TestCase (assertEqual "QuantumComposer |0>" 8536
    (round $ 10000 * (snd $ (calcProb $ emitState $ readElements big_qasm) !! 1)))


main :: IO Counts
main = runTestTT $ TestList [test001, test002, test003, test004, test005, test006, test007, test008, test009, test010,
                             test101, test102, test103, test104, test105, test106, test107,
                             test201, test202,
                             test901, test902]
