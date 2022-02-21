##############  #Lektion02 ################
# 
# Virksomhedsstrategi i et netværksperspektiv 
# Komponenter og sammenhængskraft 
#
#########################################

# load working directory 
setwd("/Users/alexandergamerdinger/Desktop/PhD/teaching/virksomhedsstrategi_forår_2022")

# install new packages 

install.packages('graphlayouts')

# loading libraries 
library(data.table)
library(tidyverse)
library(igraph)
library(ggraph)
library(graphlayouts)

# Load and prepare data  --------------------------------------------------
# load elitedb from the input folder.



# we'll be looking only at corporations


# select only the people who are in the data at least twice 
# create a new variable, with tidyverse. 




# Remove any whose name only is there once 



# Creates incidence matrix



# We want a one-mode network. This is either done by making a graph and then creating a bipartite.projection() or by doing a matrix computation. We want an adjacency between companies. 



# create an undirected, and unweighted network (that is, there is no information about the *strength* of the relations)



# simplify the network - remove "edges-to-self" and potential weights (we know there is none because we just told the former function not to, but just to be sure) 




# Density in a graph ------------------------------------------------------

# empty graph
e1 <- make_empty_graph(40)
plot(e1, vertex.size=10, vertex.label=NA) # uses igraphs own plotting function, better than autograph() for this specific purpose
# calculate the density
# connections to one-self shouldn't be part of the calculation, just in case

# Full graph
e2 <- make_full_graph(40)
plot(e2, vertex.size=10, vertex.label=NA)
edge_density(e2, loops=FALSE)

# Star graph 
e3 <- make_star(40)
plot(e3, vertex.size=10, vertex.label=NA) 
edge_density(e3, loops=FALSE)

e4 <- make_tree(40, children = 3, mode = "undirected") # try putting 'out' or 'in' in the mode-argument
plot(e4, vertex.size=10, vertex.label=NA) 
edge_density(e4, loops=FALSE)

# eliteDB graph


# plot





# Components of a graph ---------------------------------------------------
# get a list of the components



# What type of element is this?



# How long are the elements of the list?


# what does they contain?


# so: csize is just a table of membership!

# Now: decompose the actual graph into components


# select the biggest component 
# create a table and arrange by number of nodes


# what does tbl look like? 

# V1 is of class character, we need to change it. 


# create index variable that tells us where on the list the component is located


# select the largest one


# Density of largest component


# plot the biggest component



# select the second-largest component


# density second-largest component


# plot the second largest 




# A word of caution: be sure to remove loops 
g <- graph( c(1,2, 2,2, 2,3) )
plot(g)
edge_density(g, loops=FALSE)              # this is wrong!!!
edge_density(g, loops=TRUE)               # this is right!!!
edge_density(igraph::simplify(g), loops=FALSE)    # this is also right, but different

# who are in the second-largest component?






# or look at everyone in the same affiliations




# Transitivity ------------------------------------------------------------
# Number of actual triads out of all possible triads

# Example graph transitivity

g1 <- make_ring(10)
plot(g1)
transitivity(g1) # 0 - no triads
g2 <- sample_gnp(10, 4/10)
transitivity(g2, type='global')   # this is about 10/1000
plot(g2)

# eliteDB transitivity


# global transitivity, e.g. "transitivity coeffient" (default argument to type is 'global')


# mean local transitivity, e.g. "clustering coefficient" (not that reliable)




# Path lengths ------------------------------------------------------------
# Length of all the shortest paths between all nodes



# The mean of the shortest distance between each pair of nodes in the network 




# mean distance for all components, one by one, with sapply



# distance from specific node to all other nodes






# create attribute that contains the distance to LEGO



# and a logical variabel that tells us if it's LEGO or not



# plot it







# save it



# Diameter  ----------------------------------------------------------------
# diameter of network 



# Who is on the outskirts?


# How to traverse it?


# create attributes to both edges and vertices for this trip
# default is that it doesn't go through any given vertex and edge



# get the indices of the vertices that are in the path of the diameter




# The code for edges are kind of different, this is igraph-specific


# plot it






# save the output to the output folder



# Highlighting certain paths  ---------------------------------------------
# get the igraph object that contains the path
path_of_interest <- shortest_paths(net1_largest, 
                            from = V(net1_largest)[name=="A.P. Moeller - Maersk"], 
                             to  = V(net1_largest)[name=="Advice A/S"],
                             output = "both") # both path nodes and edges

# create attributes like in the previous section and fill it with TRUE/FALSE values
E(net1_largest)$path1 <- FALSE
V(net1_largest)$path1 <- FALSE
# get the indices of the vertices that are in the path of interest
index <- which(V(net1_largest)$name %in% names(path_of_interest$vpath[[1]])) 
V(net1_largest)$path1[index] <- TRUE
E(net1_largest, path=path_of_interest$vpath[[1]])$path1 <- TRUE


# plot it
ggraph(net1_largest, layout='fr') + 
geom_edge_link0(aes(filter=path1==TRUE), color='red', width=1.2) + 
geom_edge_link0(aes(filter=path1==FALSE), color='grey50', alpha=0.5) + 
geom_node_point(aes(filter=path1==TRUE), color='red', size=5, alpha=0.5) + 
geom_node_point(aes(filter=path1==FALSE), color='black', size=5, alpha=0.25) + 
geom_node_label(aes(filter=path1==TRUE, label=name), color='black', size=3, alpha=0.25, nudge_y=-1) + 
theme_graph() 
# save to the output folder
ggsave('output/lektion02-path-example.png', width=30, height=17.5, unit='cm')

