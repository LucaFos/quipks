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

Note: parametrized gates aren't supported yet.
