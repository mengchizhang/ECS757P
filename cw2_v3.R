library(igraph)
# Get data in
relaList <- relaData$relation[1:100,] #only the first 100 relationships
# Create a directed graph
fbg <- graph.data.frame(relaList,directed=TRUE)
# Simplify the graph: Remove loop and multiple edges
fbg <- simplify(fbg)
# Plot the graph
# Note that plotting a very big graph can be very painful, and sometimes cause error (memory issues)
out=layout.fruchterman.reingold(fbg) 
# Set layout so to make the display more friendly
plot(fbg,layout=out,vertex.size=5,vertex.label.cex = 0.2)
tkplot(fbg,layout=layout.kamada.kawai,vertex.size=5,vertex.label.cex = 0.8)
# Examine the measurements
plot(degree.distribution(fbg, mode="all"),log="xy",pch="+",xlab = "degree",ylab="fraction of user",main = "degree distribution") # Degree distribution not fit power law
plot(degree.distribution(fbg, mode="all")*nrow(relaList),x=c(0:max(degree(fbg))),pch=20,xlab="degree",ylab="freq",type="b",main="degree distribution",col="dark gray")
lines(degree.distribution(fbg,mode="in")*nrow(florida),pch=25,xlab="degree",ylab="freq",col="red",type="b")
lines(degree.distribution(fbg,mode="out")*nrow(florida),pch=24,xlab="degree",ylab="freq",col="blue",type="b")
legend("topright",c("total degree","indegree","outdegree"), col=c("dark gray","red","blue"), pch=c(20,25,24), lty=1, lwd = 2, text.font = 6)
average.path.length(fbg)
diameter(fbg)
clusters(fbg)
betweenness(fbg)
closeness(fbg)

# Plot the label and node size based on user degree
V(fbgranve)$degree <- degree(fbgranve) 
V(fbgranve)$label.cex <- 1.5 * V(fbgranve)$degree / max(V(fbgranve)$degree)+ .4
V(fbgranve)$label.color <- rgb(0, 0, .2, .8)
V(fbgranve)$size <- 10 * V(fbgranve)$degree / max(V(fbgranve)$degree)+ 3
V(fbgranve)$frame.color <- NA
E(fbgranve)$arrow.size <- .4
plot(fbgranve, layout=out)

# Community detection
out2 = layout.kamada.kawai(fbg)
system.time(fbgceb <- edge.betweenness.community(fbg))
print(modularity(fbgceb))
plot(fbgceb,fbg,layout=out2,vertex.label=V(fbg)$name,vertex.size=2,vertex.label.cex = 0.8)
# Using tkplot tool to plot community
member = fbgcl$membership
# Manually specify the color of nodes based on their community id
color = rainbow(max(member))
membercol = as.character(matrix(1,1,length(member)))
for(i in 1:length(color)){
  membercol[member==i] = color[i]
}
idg <- tkplot(fbg,layout=out2,vertex.label=V(fbg)$name,vertex.size=5,vertex.color = membercol,vertex.label.cex = 0.8)

# Save the plot as image
# Normal plot
setEPS()
postscript("whatever.eps") #delete the previous command, and change this with png("whatever.eps") for a png file, same with jpeg etc
pairs(relaCount[2:4])
dev.off()
# For tkplot
tkp <- igraph:::.tkplot.get(idg)
tkpostscript(tkp$canvas, file = 'filename.eps')
all.deg.testgraph<-degree(fbg,v=V(fbg),mode="all")
# And the expected power law distribution
lines(1:20,10*(1:20)^((-power$alpha)+1))

# Diameter is essentially the longest path between two vertices
diameter(fbg)

all.deg.fbg<-degree(fbg,v=V(fbg),mode="all")
deg.distr<-degree.distribution(fbg,cumulative=T,mode="all")
power<-power.law.fit(all.deg.fbg)

plot(deg.distr,log="xy",
     ylim=c(.01,10),
     bg="black",pch=21,
     xlab="Degree",
     ylab="Cumulative Frequency")
lines(1:20,10*(1:20)^((-power$alpha)+1))
transitivity(fbg)
out=layout.fruchterman.reingold(fbg)
plot(fbg,layout=out,vertex.size=5,vertex.label.cex = 0.2)
tkplot(fbg,layout=layout.kamada.kawai,vertex.size=5,vertex.label.cex = 0.8)
