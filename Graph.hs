module Graph where

import Data.Array as Array
import Data.List

import EntangleMonad
import Examples


type Vertex = Int
type Table a = Array Vertex [Edge]
type Graph = Table [(Edge, Vertex)]

-- Needed for the buildG function (the second element of the tuple is the number of vertices)
type Bounds  = (Vertex, Vertex)

-- Edges data type
data Edge
		= GateEdge Vertex (String,[QubitId],[QubitId]) Vertex		-- Edge representing a gate
	    | MeasureEdge Vertex (QubitId,BitId) Vertex					-- Edge representing a Measurement on one qubit
	    | LoopEdge Vertex String									-- Edge used to represent recursive circuits

-- A Labeling contains the information about bits in a certain state (vertex)
type Labeling a = Vertex -> [(BitId,Int)]

-- Labelled Graph
data LabGraph n e = LabGraph (Graph) (Labeling n)


-- Returns a list of vertices
vertices :: LabGraph t t1 -> [Vertex] 
vertices (LabGraph gr _) = indices gr

-- Returns all the vertices' labels of the graph
labels :: LabGraph t t1 -> [[(BitId,Int)]]
labels (LabGraph gr l) = map l (indices gr)

-- Returns a list of edges
edges :: Graph -> [Edge]
edges g = [(GateEdge v1 l v2) | v1 <- indices g, (GateEdge v1 l v2) <- g!v1] ++
		  [(MeasureEdge v1 c v2) | v1 <- indices g, (MeasureEdge v1 c v2) <- g!v1] ++
		  [(LoopEdge v s) | v <- indices g, (LoopEdge v s) <- g!v]




-- Generates a labGraph from a tree representing the quantum circuit
labGraphFromTree tree = LabGraph graph labels
	where
		graph = buildG bounds edges
		labels = getNodeLabels tree $ initLabels tree
		bounds = (1,numOfGates tree)
		edges = extractGates tree 1

-- Builds a (non labelled) graph from a list of edges
buildG :: Bounds -> [Edge] -> Graph
buildG bounds edges = accumArray (flip (:)) [] bounds ([(v1, (GateEdge v1 l v2)) | (GateEdge v1 l v2) <- edges] ++
													   [(v1, (MeasureEdge v1 c v2)) | (MeasureEdge v1 c v2) <- edges] ++
													   [(v, (LoopEdge v s)) | (LoopEdge v s) <- edges])

-- Computes the labels needed to build the graph
getNodeLabels :: (Num a, Num t, Ord a) => CircTree t1 -> [(BitId, t)] -> a -> [(BitId, t)]
getNodeLabels (GateNode _ _ _ t) s n
		| n>1 		= getNodeLabels t s (n-1)
		| otherwise = s

getNodeLabels (MeasureNode _ bit t1 t2) s n
		| n>1 		= if n <= numOfGates t1 + 1
						then getNodeLabels t1 s (n-1)
						else getNodeLabels t2 ((bit,1):(filter ((/=bit).fst) s)) (n - (numOfGates t1) - 1)
		| otherwise = s

getNodeLabels (LeafNode _ ) s _ = s

-- Initially set the value of all bits to be 0
initLabels (GateNode _ _ _ t) = initLabels t
initLabels (MeasureNode qubit bit t1 t2) = (bit,0):(initLabels t1)
initLabels (LeafNode _) = []


-- Calculates the number of gates required to represent the (sub)tree rooted in t
numOfGates :: Num a => CircTree t -> a
numOfGates (GateNode _ _ _ t) = numOfGates t + 1
numOfGates (MeasureNode _ _ t1 t2) = numOfGates t1 + numOfGates t2 + 1
numOfGates (LeafNode _) = 1


-- Computes the edges required to represent the whole circuit
extractGates (GateNode gate qubits1 qubits2 t) n = (GateEdge n (gate,qubits1,qubits2) (n+1)):extractGates t (n+1)
extractGates (MeasureNode qubit bit t1 t2) n = (MeasureEdge n (qubit,bit) (n+1)):
											   (MeasureEdge n (qubit,bit) (n + numOfGates t1 + 1)):
											   (extractGates t1 (n+1)) ++ (extractGates t2 (n + (numOfGates t1) + 1))
extractGates (LeafNode Loop) n = [(LoopEdge n "Loop")]			-- needed for recursive programs
extractGates (LeafNode _) _ = []



-- Outputs a graph in .dot format
showGraphViz :: LabGraph t t1 -> String
showGraphViz (LabGraph gr lab)  = 
    "digraph {\n" ++
    --"rankdir=LR;\n" ++									-- uncomment -> render graph from left to right
    (concatMap showNode $ indices gr) ++
    (concatMap showEdge $ edges gr) ++
    "}\n"
    where
		showEdge (GateEdge from t to) = show from ++ " -> " ++ show to ++ " [label = \"" ++ showTripleGate t ++ "\"];\n"
		showEdge (MeasureEdge from t to) = show from ++ " -> " ++ show to ++ " [label = \"" ++ showTripleMeasure t ++ "\"];\n"
		showEdge (LoopEdge from t) = show from ++ " -> " ++ "1" ++ " [label = \"" ++ t ++ "\"];\n"
		showNode v = show v ++ " [label = " ++  (show $ intercalate ", " $ map showPair $ sort $ lab v) ++ "];\n"
		showTripleGate (gate,qubits1,qubits2) = "Gate '" ++ gate ++ "' on qubit" ++ (if length qubits1 == 1 then " " else "s ") ++ showQubitList qubits1 ++
												(if length qubits2 > 0 then " controls " else "") ++ showQubitList qubits2
		showTripleMeasure (qubit,bit) = "Measure qubit " ++ (show $ unqubit qubit) ++ " -> bit " ++ (show $ unbit bit)
		showPair (bit,int) = "Bit " ++ (show $ unbit bit) ++ "=" ++ show int
		showQubitList l = (intercalate "," $ map (show . unqubit) l)

