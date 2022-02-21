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

den <- read_csv("input/den17-no-nordic-letters.csv")

# we'll be looking only at corporations

den1 <- den %>% filter(sector == "Corporations")

# how many did we filter out?  

nrow(den) - nrow(den1)

# select only the people who are in the data at least twice 
# create a new variable, with tidyverse. 

den2 <- den1 %>% 
  group_by(name) %>% 
  mutate(N = n()) %>% 
  select(N, everything())

# Remove any whose name only is there once 

den2 <- den2 %>% 
  filter(N > 1)

# Creates incidence matrix

incidence <- xtabs(formula = ~ name + affiliation, data = den2, sparse = T)

# We want a one-mode network. This is either done by making a graph and then creating a bipartite.projection() or by doing a matrix computation. We want an adjacency between companies. 

adjacency <- Matrix::t(incidence) %*% incidence

# create an undirected, and unweighted network (that is, there is no information about the *strength* of the relations)

net <- graph_from_adjacency_matrix(adjacency, mode = "undirected", weighted = NULL)

# simplify the network - remove "edges-to-self" and potential weights (we know
# there is none cuz we just told the former function not to, but just tobe
# sure) 

net1 <- simplify(net, remove.multiple = TRUE, remove.loops = TRUE)


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
edge_density(net1, loops=FALSE)

# plot

net1 %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(color = "gray70", alpha = 0.5) +
  geom_node_point(size=1.5, alpha = 0.3) +
  theme_graph()

# Components of a graph ---------------------------------------------------
# get a list of the components

complist <- components(net1)

# What tpye of element is this?
class(complist)
sapply(complist, class)

# How long are the elements of the list?
sapply(complist, length)

# what does they contain?
complist$membership
complist$csize
# so: csize is just a table of membership!

# Now: decompose the actual graph into components
net2 <- decompose.graph(net1)

# select the biggest component 
# create a table and arrange by number of nodes
tbl <- table(complist$membership) %>% data.table() %>% arrange(desc(N))

# what does tbl look like? 
str(tbl)
# V1 is of class character, we need to change it. 
tbl$V1 <- as.numeric(tbl$V1)

# create index variable that tells us where on the list the component is located
index <- tbl$V1[1]

# select the largest one
large_net <- net2[[index]]

# Density of largest component
edge_density(large_net, loops = F)

# plot the biggest component
large_net %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(color= "gray70", alpha= 0.5) +
  geom_node_point(size=1.5, color="darkblue", alpha = 0.6) +
  theme_graph()

# select the second-largest component
second <- net2[[tbl$V1[2]]]

# density second-largest component
edge_density(second, loops = F)

# plot the second largest 
second %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(color= "gray70", alpha= 0.5) +
  geom_node_point(size=1.5, color="darkblue", alpha = 0.6) +
  geom_node_label(aes(label = name), size = 2) +
  theme_graph()

# A word of caution: be sure to remove loops 
g <- graph( c(1,2, 2,2, 2,3) )
plot(g)
edge_density(g, loops=FALSE)              # this is wrong!!!
edge_density(g, loops=TRUE)               # this is right!!!
edge_density(igraph::simplify(g), loops=FALSE)    # this is also right, but different

# who are in the second-largest component?

names <- names(V(second))

den2 %>% 
  filter(affiliation %in% names) %>% 
  group_by(name) %>% 
  view()

# or look at everyone in the same affiliations

den %>% 
  filter(affiliation %in% names) %>% 
  group_by(affiliation) %>% 
  view()

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
transitivity(net1, type='global') 

# global transitivity, e.g. "transitivity coeffient" (default argument to type is 'global')
transitivity(net1, type='global') 

# mean local transitivity, e.g. "clustering coefficient" (not that reliable)
mean(
  transitivity(net1, type='local'), 
  na.rm = T)


# Path lengths ------------------------------------------------------------
# Length of all the shortest paths between all nodes

dist <- distances(net1_largest)

# The mean of the shortest distance between each pair of nodes in the network 

mean(distances(net1_largest))
mean_distance(net1_largest)

# mean distance for all components, one by one, with sapply

sapply(comp1, mean_distance)

# distance from specific node to all other nodes

distances <- distances(net1_largest, v= names(V(net1_largest)) == "LEGO A/S", to = names(V(net1_largest)), 
                       weights = NA)

all.equal(colnames(distances), names(V(net1_largest)))

# create attribute that contains the distance to LEGO

V(net1_largest)$distance <- distances 

# and a logical variabel that tells us if it's LEGO or not

V(net1_largest)$lego <- ifelse(names(V(net1_largest))== "LEGO A/S", TRUE, FALSE) 

# plot it
net1_largest %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(color="gray50", alpha =0.5) +
  geom_node_point(aes(filter=lego==FALSE, color=distance, size=distance)) +
  geom_node_point(aes(filter=lego==TRUE), color="red", size=3) +
  geom_node_text(aes(filter=lego==FALSE, label = distance), color= "gray90", alpha =0.6, size=2.5) +
  scale_color_viridis() +
  theme_graph() +
  labs(color = "Distance to Lego")

ggsave("output/distance_to_lego.png", width = 30, height = 17.5, units = "cm")


# Diameter  ----------------------------------------------------------------
# diameter of network 

diameter(net1_largest, directed = F, weights = NULL)

# Who is on the outskirts?
farthest.nodes(net1_largest, directed = F, weights = NULL )

# How to traverse it?
diam <- get.diameter(net1_largest, directed = F)

# create attributes to both edges and vertices for this trip
# default is that it doesn't go through any given vertex and edge
V(net1_largest)$diameter <- FALSE
E(net1_largest)$diameter <- FALSE

# get the indices of the vertices that are in the path of the diameter
index <- which(names(V(net1_largest)) %in% names(diam))

V(net1_largest)$diameter[index] <- TRUE

# The code for edges are kind of different, this is igraph-specific
E(net1_largest, path = diam)$diameter <- TRUE

# plot it
net1_largest %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(aes(filter=diameter==TRUE), color = "red", width = 1.5) +
  geom_edge_link0(aes(filter=diameter==FALSE), color = "gray60") +
  geom_node_point(aes(filter=diameter==FALSE), color = "black", alpha = 0.5) +
  geom_node_point(aes(filter=diameter==TRUE), color = "red", alpha = 1, size =2) +
  geom_node_label(aes(filter=diameter==TRUE, label = name), nudge_y = -0.3) +
  theme_graph() + ggtitle('diameter in EliteDBs largest component')

# save the output to the output folder
ggsave("output/diameter_eliteDB_largest.png", width = 30, height = 17.5, units = "cm")


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

