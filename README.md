# Statistical Analysis of Social Networks

This project aims to analyze some of the key features of a chosen crawled social network, combine with the comparison and contrast of other random networks. 

The network we're going to analyze is the undirected Facebook graph from snap.stanford.edu, with 4,039 of nodes and 88,234 edges. I also create another two random networks respectively with the same size and same degree of the Facebook graph. 

Summary of dataset statistics:

|               | Max. Degree | Avg. Clustering Coefficient  | Modularity | Avg. Path Length | Diameter
| ------------- |-------------| ----- | --- | --- | --- |
| Facebook network | 1045 | 0.6170038 | 0.8347863 | 3.692507 | 8
| Random network A | 68 | 0.01087863 | 0.1341609 | 2.605787 | 4
| Random network B | 1045 | 0.06705884 | 0.1089524 | 2.592925 | 5


Software and Tools: 
- R Studio 0.99.484
- Gephi 0.9.1
- XQuartz 2.7.8


Objectives:
- Analyze three key characteristics of a chosen crawled graph such as degree distribution, modularity and clustering coefficient.
- Generate a random graph and make a comparison against the original one in terms of the different characteristics.
- Explore further of the social behaviors and community characteristics.
