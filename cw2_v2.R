# Load the igraph package
require(igraph)
# Load the data. The data needs to be loaded as a table first
fbd<-read.table("facebook_combined.txt", sep=' ', header=F)
# Transform the table into the required graph format
fbg<-graph.data.frame(fbd, directed=F)
# Inspect the data:
# Prints the list of vertices (people)
length(V(fbg))
# Prints the list of edges (relationships)
length(E(fbg))
summary(fbg)
# Removing loops from the graph:
fbg <- simplify(fbg, remove.multiple = F, remove.loops = T)
summary(fbg)

# All about Degree
# print the number of edges per vertex (relationships per people)
fbdt<-table(degree(fbg, mode="all"))
plot(fbdt, xlab = "degree", ylab = "frequency")
# Nodes with degree less than 20
fbgdl20<-V(fbg)[degree(fbg)<20]
length(fbgdl20)
# Nodes with degree less than 2
fbgdl2<-V(fbg)[degree(fbg)<2]
length(fbgdl2)
# Nodes with degree greater than 200
fbgdg200<-V(fbg)[degree(fbg)>200]
length(fbgdg200)
# Random Graph Nodes with degree less than 20
fbgranvedl20<-V(fbgranve)[degree(fbgranve)<20]
length(fbgranvedl20)
# Random Graph Nodes with degree less than 2
fbgranvedl2<-V(fbgranve)[degree(fbgranve)<2]
length(fbgranvedl2)
# Random Graph Nodes with degree less than 20
fbgrandl20<-V(fbgrand)[degree(fbgrand)<20]
length(fbgdl20)
# Random Graph Nodes with degree less than 2
fbgrandl2<-V(fbgrand)[degree(fbgrand)<2]
length(fbgrandl2)
fbg<-delete.vertices(fbg, nodes_degree_lesser_than_2)
summary(fbg)
fbgdg200<-V(fbg)[degree(fbg)>200]
length(fbgdg200)

# Max Degree
n.maxd<-max(degree(fbg, mode="all"))
nodes_with_max_degree<-V(fbg)[degree(fbg)==n.maxd]
nodes_with_max_degree

# Degree Distribution
par(mfrow=c(1,2), mar=c(1,1))
fbd.deg.dist<-degree.distribution(fbg, mode="all")
plot(fbd.deg.dist, main = "log-log graph", log="xy", xlab = "degree", ylab = "frequency")
plot(fbd.deg.dist, main = "simple graph", xlab = "degree", ylab = "frequency")


# All about Modularity
fbgcw <- walktrap.community(fbg)
modularity(fbgcw)
modularity(fbg, membership(wtc), weights = NULL)
modularity_matrix(fbg, membership(wtc), weights = NULL)

# All about Cluster
transitivity(fbg)
triangles(fbg)
tb_triangles<-table(triangles(fbg))
count_triangles(fbg, vids = V(fbg))
no.clusters(fbg)
table(clusters(fbg)$csize)
# Transitivity of a random graph of the same size
g <- erdos.renyi.game(vcount(fbg), ecount(fbg), type="gnm")
transitivity(g)
# Transitivity of a random graph with the same degrees
g2 <- degree.sequence.game(degree(fbg,mode="all"), method="vl")
transitivity(g2)

# Attributes of data
is.connected(fbg)
is.directed(fbg)
graph.density(fbg) # Density
diameter(fbg)
average.path.length(fbg)
tb_shpth<-table(shortest.paths(fbg, v=V(fbg), to = V(fbg),mode = "all"))
plot(tb_shpth)
# Making four plots
par(mfrow=c(3,3), mar=c(1,1,1,1))
fbgcw <- cluster_walktrap(fbg)
plot(fbgcw,fbg)
# bw<-cluster_edge_betweennessbw(fbg)
fbgceb <- cluster_edge_betweenness(fbg)
plot(fbgceb,fbg)
fbgcs <- cluster_spinglass(fbg)
plot(fbgcs,fbg)
fbgco <- cluster_optimal(fbg)
plot(fbgco,fbg)

# Network Ploting
plot(fbg, edge.arrow.size=100, vertex.label=NA, vertex.color="dark blue", edge.color="grey")
# The LGL algorithm is meant for large, connected graphs. 
# Here you can also specify a root: a node that will be placed in the middle of the layout.
l<-layout.lgl(fbg)
# Normalize them so that they are in the -1, 1 interval:
l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
plot(fbg, rescale=F, layout=l*1.3)
tkplot(fbg, vertex.label=NA, rescale=F, layout=l*1.5)
tkplot(fbg, edge.arrow.size=100, vertex.size=5, vertex.label=NA, vertex.color="blue", edge.color="red", rescale=F, layout=l)
# Compute node degree (#links) and use it to set node size:
deg <- (degree(fbg, mode="all")*.10)
plot(fbg, edge.arrow.size=100, vertex.size=deg, vertex.label=NA, vertex.color="blue", edge.color="red", rescale=F, layout=l)
tkplot(fbg, edge.arrow.size=100, vertex.size=deg, vertex.label=NA, vertex.color="blue", edge.color="red", rescale=F, layout=l*1.3)


# Subset the data. 
# If we want to exclude people who are in the network only tangentially (participate in one or two relationships only)

# net<-delete.vertices(bsk.network, bad.vs3)
# Making and ploting a subgraph
nodewith_degree_3<- V(fbg)[degree(fbg)<4]
length(nodewith_degree_3)
nodewith_degree_1<- V(fbg)[degree(fbg)<2]
length(nodewith_degree_1)
subgraph(fbg, nodewith_degree_3)
tkplot(subgraph(fbg, nodewith_degree_3), vertex.size=10, edge.arrow.size=50, vertex.label=NA, vertex.color="blue", edge.color="red")
#tkplot(net, edge.arrow.size=50, vertex.label=NA, vertex.color="blue", edge.color="red")
#tkplot(bsk.network)
