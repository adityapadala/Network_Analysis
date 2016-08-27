#Project 2
#Team OptimizeR

install.packages("igraph")
install.packages("png")
library("igraph")
library("png")

# creating an adjacency matrix
a=c(0,1,0,1,0,1,0,1)
b=c(1,0,1,0,0,1,1,0)
c=c(0,1,0,1,1,0,0,1)
d=c(1,0,1,0,1,0,1,0)
e=c(0,0,1,1,0,1,0,0)
f=c(1,1,0,0,1,0,0,1)
g=c(0,1,0,1,0,0,0,0)
h=c(1,0,1,0,1,1,0,0)

am=matrix(c(a,b,c,d,e,f,g,h),nrow=8,byrow=TRUE) 
#assigning the row and column labels
dimnames(am)=list(c("A","B","C","D","E","F","G","H"),
                  c("A","B","C","D","E","F","G","H"))
am
#function to convert the adjacency matrix to a graph object. Creating an unidirectional network graph.
am_graph = graph.adjacency(am,mode = "undirected")
am_graph
plot(am_graph)

#Creating an incidency matrix
a=c(1,1,0,0,0,0,0,0,0,1) 
b=c(0,0,1,0,0,1,0,1,0,0) 
c=c(1,0,1,0,1,0,0,0,1,0) 
d=c(0,1,0,1,0,0,0,0,1,0) 
e=c(0,0,1,0,1,0,0,1,1,0)
f=c(0,0,0,0,0,0,1,0,0,1)
g=c(1,0,1,0,0,1,0,0,1,1)
h=c(1,1,0,1,1,0,1,0,1,0)

im=matrix(c(a,b,c,d,e,f,g,h),nrow=8,byrow=TRUE) 
#assigning different row and column labels
dimnames(im)=list(c("A","B","C","D","E","F","G","H"),
                   c("G1","G2","G3","G4","G5","G6","G7","G8","G9","G10"))
im
#graph.incidence is a function that is used to convert the incidence matrix to a graph object
im_graph = graph.incidence(im)
im_graph
#to find out the type and the names of the vertices
V(im_graph)$type
V(im_graph)$name
#assigning different shapes to row and columns to differenciate between the groups
shapes=c(rep("circle",nrow(im)),rep("square",ncol(im)))
labeldistances=c(rep(0,nrow(im)+ncol(im)))
#plotting the incidence graph
plot(im_graph,vertex.shape=shapes,vertex.label.degree=-pi/2,
     vertex.label.dist=labeldistances,vertex.color=V(im_graph)$type)

# Creating an incidence matrix to form a bipartite networks
a=c(1,1,0,0,0) 
b=c(0,0,1,0,0) 
c=c(1,0,1,0,1) 
d=c(0,1,0,1,0) 
e=c(0,0,1,0,1)
f=c(0,1,0,1,0)
bi=matrix(c(a,b,c,d,e,f),nrow = 6,byrow = TRUE)
dimnames(bi)=list(c("A","B","C","D","E","F"),
                   c("G1","G2","G3","G4","G5"))
bi_graph=graph.incidence(bi)
#to bipartite the network we use "bipartite.projection" function
bp=bipartite.projection(bi_graph)
bp
#1st graph object
get.adjacency(bp$proj1,sparse = FALSE,attr = "weight")
#2nd graph object
get.adjacency(bp$proj2,sparse = FALSE,attr = "weight")

#plotting the row vertices with their strength of connectivity
plot(bp$proj1,edge.width=E(bp$proj1)$weight^2,edge.color="black",vertex.label=V(bp$proj1)$name)
#plotting the column vertices with their strength of connectivity
plot(bp$proj2,edge.width=E(bp$proj2)$weight^2,edge.color="black",vertex.label=V(bp$proj2)$name)

#another way to obtain individual co-member relationships
as_incidence_matrix(bi_graph)  %*% t(as_incidence_matrix(bi_graph))
t(as_incidence_matrix(bi_graph)) %*% as_incidence_matrix(bi_graph)

#Network Metrics

zach <- read.table("C:/Users/VISWANATH/Desktop/Aditya/Classwork/R/Project 2/zach.txt", quote="\"")
#for simplicity we have selected 10 rows and 10 columns
zach1 <- zach[0:10,0:10]
#assigning the names to the rows and the columns
dimnames(zach1)=  list(  c("A","B","C","D","E","F","G","H","I", "J"),
                         c("A","B","C","D","E","F","G","H","I", "J")    )
#converting the data table to matrix
zach1 = as.matrix(zach1)

#converting to igraph objects
g<-  graph_from_adjacency_matrix(zach1 , mode = "undirected")
plot(g)

#Node attributes 

#degree_centralization  of the g 
deg <- degree(g,mode = "all")
centr_degree(g, mode="in", normalized=T)
# To plot the g 
plot(g,vertex.size = deg*6)
# histogram of degree of g  
hist(deg)

# To calculate the betweenness centrality 
bet <- betweenness(g,directed = TRUE ,weights = NULL, normalized = T )
plot(g,vertex.size = bet* 100)

# To calculate the closeness centrality 
closenes <- closeness(g,weights = NULL, normalized = T)
plot(g, vertex.size = closenes * 50)


# edge-attributes  

# To find the diameter across the network 
diameter(g , directed = TRUE , unconnected = FALSE , weights = NULL)

# To get the path which diameter is passing through 
d = get_diameter(g, directed = TRUE , unconnected = FALSE , weights = NULL) 
#assigning different colors to diameter path and others.
vcol <- rep("gray40", vcount(g))
vcol[d] <- "gold"
ecol <- rep("gray80", ecount(g))
ecol[E(g, path=d)] <- "orange" 
#plotting the network with the diameter path
plot(g, vertex.color=vcol, edge.color=ecol, edge.arrow.mode=0)

#shortest path
news.path <- shortest_paths(g, 
                            from = V(g)[V(g)$name == "E"], 
                            to  = V(g)[V(g)$name == "D"],
                            output = "both") # both path nodes and edges

# Generate edge color variable to plot the path:
ecol <- rep("gray80", ecount(g))
ecol[unlist(news.path$epath)] <- "orange"
# Generate edge width variable to plot the path:
ew <- rep(2, ecount(g))
ew[unlist(news.path$epath)] <- 4
# Generate node color variable to plot the path:
vcol <- rep("gray40", vcount(g))
vcol[unlist(news.path$vpath)] <- "gold"
#plotting the shorted distance in the network
plot(g, vertex.color=vcol, edge.color=ecol, 
     edge.width=ew, edge.arrow.mode=0)

# The end vertices of the diameter 
farthest_vertices(g,directed = TRUE , unconnected = FALSE , weights = NULL)

# density  for a undirected network 
edge_density(g)

# reciprocity 
reciprocity(g , ignore.loops = TRUE , mode =c("default" ,"ratio"))


#Exploring different layouts
vertex <- read.csv("C:/Users/VISWANATH/Desktop/Aditya/Classwork/R/Project 2/NODES.csv", header=T, as.is=T)
edge <- read.csv("C:/Users/VISWANATH/Desktop/Aditya/Classwork/R/Project 2/EDGES.csv", header=T, as.is=T)

#to reduce the duplicate edge present in the edge dataset
edge <- aggregate(edge[,3], edge[,-3], sum)
edge <- edge[order(edge$from, edge$to),]
#changing the name of one of the columns and deleting the row label from the data frame.
colnames(edge)[4] <- "weight"
rownames(edge) <- NULL

#converting a data frame to a graph object using graph_from_data_frame function.
net <- graph_from_data_frame(d=edge, vertices=nodes, directed=F) 
class(net)
net
#to remove the multiple loops and make the graph look better
net <- simplify(net, remove.multiple = T, remove.loops = T) 
plot(net)

#random layout
plot(net, layout=layout_randomly)

#in circle
l <- layout_in_circle(net)
plot(net, layout=l)

#tkplot helps you to customize the layout in the run time.
tkplot(net)

#manula layout
l <- cbind(1:vcount(net), c(1, vcount(net):2))
plot(net, layout=l)

#3d sphere layout
l <- layout_on_sphere(net)
plot(net, layout=l)

#Fruchterman-Reingold Layout
l <- layout_with_fr(net)
plot(net, layout=l)

#other layouts
layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] 
# Remove layouts that do not apply to our graph.
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

par(mfrow=c(3,5))
for (layout in layouts) {
  print(layout)
  l <- do.call(layout, list(net)) 
  plot(net, edge.arrow.mode=0, layout=l, main=layout) 
}

#to stop the par function. Please dont execute dev.off() untill all the layouts are visible.
dev.off()

#adding images to the vertex according to the type

nodes2<- read.csv("C:/Users/VISWANATH/Desktop/Aditya/Classwork/R/Project 2/NODES2.csv",header=T, as.is=T)
links2<-read.csv("C:/Users/VISWANATH/Desktop/Aditya/Classwork/R/Project 2/EDGES2.csv", header=T, row.names = 1)
#converting the data frame to matrix
links2 <- as.matrix(links2)
net2 <- graph_from_incidence_matrix(links2)

#reading the images
img.1 <- readPNG("C:/Users/VISWANATH/Desktop/Aditya/Classwork/R/Project 2/newschannel.png")
img.2 <- readPNG("C:/Users/VISWANATH/Desktop/Aditya/Classwork/R/Project 2/user.png")

#assiging the images to different types of vertices present in the incidency matrix from net2
V(net2)$raster <- list(img.1, img.2)[V(net2)$type+1]

#plotting the graph object net2
plot(net2, vertex.shape="raster", vertex.label=NA,
     vertex.size=10, vertex.size2=10, edge.width=2)

#community detection using edge between algorithm
ceb <- cluster_edge_betweenness(net) 
#plotting dendplots
dendPlot(ceb, mode="hclust")
#plot the clustering on the network
plot(ceb, net)

#community detection using fast greedy algorithm
cfg <- cluster_fast_greedy(as.undirected(net))
#plotting dendplots
dendPlot(cfg, mode="hclust")
#plot the clustering on the network
plot(cfg, as.undirected(net))

#forming clusters manually by assiging colors.
V(net)$community <- cfg$membership
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
plot(net, vertex.color=colrs[V(net)$community])

