import Examples
import Transitions

import Graph



main :: IO ()
main = putStrLn $ showGraphViz $ labGraphFromTree tree
       where 
            tree = circToTree circ
            circ = recCirc'
            --circ = groverRec      -- needs a long time to compute the .pdf
            --circ = groverRecFive
            --circ = branchCirc
