# quipks

The software requires some Cabal packages to be run, all the required dependencies are listed in "quipks.cabal".

Running  
ghc Main.hs  
produces an executable which outputs a string that is the .dot representation of the graph.  
You can use  
./Main > mygraph.dot  
followed by  
dot -Tps mygraph.dot -o mygraph.pdf  
and then open your pdf file.  
You'll need Graphviz to visualize the .dot files: http://www.graphviz.org/  
  
At the moment, the software aims to visualize recursive programs (you can run example programs changing line 12 of Main.hs, choosing one of the quipper programs that are contained in Examples.hs).  
If you want to run non-recursive programs, just comment line 94 of Graph.hs and recompile (this will be changed soon).    

Note: parametrized gates aren't supported yet.
