##############  #Lektion02 ################
# 
# Virksomhedsstrategi i et netværksperspektiv 
# Komponenter og sammenhængskraft 
#
#########################################

# load working directory 
setwd("/Users/alexandergamerdinger/Desktop/PhD/teaching/virksomhedsstrategi_forår_2022")

# install new packages 

#install.packages('graphlayouts')

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

# select only the people who are in the data at least twice 
# create a new variable, with tidyverse. 

den2 <- den1 %>% 
  group_by(name) %>% 
  mutate(N = n()) %>% 
  select(N, everything())

# Remove any whose name only is there once 

den3 <- den2 %>% 
  filter(N > 1)

# Creates incidence matrix
# Sparse T means that instead of 0, it will just make a "."
incidence <- xtabs(formula = ~ name + affiliation, data = den3, sparse = T)

# We want a one-mode network. So first, we make a two-mode network. 

net <- graph_from_incidence_matrix(incidence, directed = FALSE)

# Now, we split the network in two one-node networks
net1 <- bipartite.projection(net)

# And select $proj2 because we would like to work with the company-company network
# This is an undirected, and unweighted network (that is, there is no information about the *strength* of the relations)
net2 <- net1$proj2
net2

# Let us also simplify the network - remove "edges-to-self" and potential weights (we know there is none because we just told the former function not to, but just to be sure) 

net2 <- simplify(net2, remove.multiple = TRUE, remove.loops = TRUE)
# Because we filtered all people that appear in Den less than 2 times, there are no loops either, so simplify() will not do anything with net2. But still, it is important to understand the function. 


# Density in a graph ------------------------------------------------------
# Density or "Tæthed" is an measure for network coherence, and tells us about the probability that two random nodes are linked with each other.
# EXAMPLE:  
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
edge_density(net2, loops=FALSE)

# plot
net2 %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(color = "gray70") +
  geom_node_point(size=1.5) +
  theme_graph()

# Components of a graph ---------------------------------------------------
# We subdivide graphs into its components, since some measures such as diameter only make sense to calculate within a component.
# get a list of the components
complist <- components(net2)

# What tpye of element is this?
class(complist)
sapply(complist, class)

# How long are the elements of the list?
sapply(complist, length)

# what does they contain?
complist$membership
complist$csize
complist$no

# Now: decompose the actual graph into components
net3 <- decompose.graph(net2)

# select the biggest component 
# create a table of complist$membership to see how many members each component has and arrange by number of nodes
tbl <- table(complist$membership) %>% data.table() %>% arrange(desc(N))

# what does tbl look like? 
str(tbl)
# V1 is of class character, we need to change it. 
tbl$V1 <- as.numeric(tbl$V1)

# create index variable that tells us where on the list the component is located. Since we would like to select the biggest component, (and since we know that tbl is ordered), we can just select the first entry. 
index <- tbl$V1[1]

# select the largest one
# remember, that we need to have two quadratic brackets here since net
net_largest <- net3[[index]]

# Density of largest component
edge_density(net_largest, loops = FALSE)

# plot the biggest component
net_largest %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(color = "gray70") +
  geom_node_point(size=1.5) +
  theme_graph()

# select the second-largest component
index1 <- tbl$V1[2]
net_second <- net3[[index1]]

# density second-largest component
edge_density(net_second, loops = FALSE)

# plot the second largest 
net_second %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(color = "gray70") +
  geom_node_point(size=1.5) +
  theme_graph()

# A word of caution: be sure to remove loops 
g <- graph( c(1,2, 2,2, 2,3) )
plot(g)
edge_density(g, loops=FALSE)              # this is wrong!!!
edge_density(g, loops=TRUE)               # this is right!!!
edge_density(igraph::simplify(g), loops=FALSE)    # this is also right, but different
# We use simplify, since we do not want to have loops in our dataset. Hence, when using density, always say loops = FALSE.

# who are in the second-largest component?
# in names, we store all affiliation names from the second largest component
names <- names(V(net_second))

# who are the people (and all of their attributes) that are a part of these boards?
den3 %>% filter(affiliation %in% names) %>% view()

# or look at everyone in the same affiliations from original data set. 
den %>% filter(affiliation %in% names) %>% view()

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
transitivity(net2, type='global') #type global is the default argument
transitivity(net2) #so leaving it out will give you the exact same result
# for the largest component? 
transitivity(net_largest) 

# Path lengths ------------------------------------------------------------
# Length of all the shortest paths between all nodes shown in a matrix format. 

distances(net_largest) %>% view()

# The mean of the shortest distance between each pair of nodes in the network 
mean_distance(net_largest, directed = FALSE)
mean_distance(net_largest) #same as the above, since directed = FALSE is the default argument. 

# mean distance for all components, one by one, with sapply
sapply(net3, mean_distance)

# distance from specific node to all other nodes
# A network attribute can either belong to a node V(net_largest) or an edge E(net_largest). 
# The attribute "name" is a node attribute, and can be viewed, either through names(V(net_largest)) OR V(net_largest)$name - its the same. 
distances <- distances(net_largest, v= names(V(net_largest)) == "LEGO A/S", to = V(net_largest), 
                       weights = NA)

# distances is very similar to the matrix we have just seen, but now, only shows one row, namely LEGO A/S
view(distances)

####### BONUS ##### For improved understanding ########
# the object distances has exactly as many columns than nodes in the net_largest graph. 
ncol(distances) == vcount(net_largest)
# Another way of telling if the column names of the distances object fits to the graph object is to check if the column names are the same than the vertex names of the graph
all.equal(target = colnames(distances), 
          current = names(V(net_largest)))
# They are the same.

# Let us create a new attribute that contains the distance to LEGO
V(net_largest)$distance <- distances 

# and a logical variable that tells us if it's LEGO or not
V(net_largest)$lego <- ifelse(names(V(net_largest))== "LEGO A/S", TRUE, FALSE) 

# If the IF ELSE statement is too confusing for you, think of it as this way. 
V(net_largest)$lego <- FALSE # default argument is that the node is not Lego 
V(net_largest)$lego[names(V(net_largest)) == "LEGO A/S"] <- TRUE #but when it is Lego, overwrite the FALSE with TRUE

# plot it
net_largest %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(color="gray50") +
  geom_node_point(aes(filter=lego==FALSE, color=distance), size =3) +
  geom_node_point(aes(filter=lego==TRUE), color="red", size=3) +
  geom_node_text(aes(filter=lego==FALSE, label = distance), color= "gray90", size=2.5) +
  theme_graph() +
  labs(color = "Distance to Lego") +
  scale_color_viridis() 

#save it - we save it in our output folder. 
ggsave("output/distance_to_lego.png", width = 30, height = 17.5, units = "cm")


# Diameter  ----------------------------------------------------------------
# diameter of network - the longest shortest path in the network. 

diameter(net_largest, directed = FALSE, weights = NULL)

# Who is on the outskirts?
farthest.nodes(net_largest, directed = FALSE, weights = NULL )

# How to traverse it?
diam <- get.diameter(net_largest, directed = FALSE)

# create attributes to both edges and vertices for this time
# default is that it doesn't go through any given vertex and edge
V(net_largest)$diameter <- FALSE
E(net_largest)$diameter <- FALSE

# get the index of the vertices that are in the path of the diameter
# it shows the location (number) of which nodes are a part of the diameter
index <- which(names(V(net_largest)) %in% names(diam))

# Here, set those locations in the diameter attribute (of the vertex) to TRUE
V(net_largest)$diameter[index] <- TRUE

# The code for edges are kind of different, this is igraph-specific
E(net_largest, path = diam)$diameter <- TRUE

# plot it
net_largest %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(aes(filter=diameter==TRUE), color = "red", width = 1.5) +
  geom_edge_link0(aes(filter=diameter==FALSE), color = "gray60") +
  geom_node_point(aes(filter=diameter==FALSE), color = "black") +
  geom_node_point(aes(filter=diameter==TRUE), color = "red", size =2) +
  geom_node_label(aes(filter=diameter==TRUE, label = name), nudge_y = -0.3) +
  theme_graph() + ggtitle('Diameter in EliteDBs largest component')

# save the output to the output folder
ggsave("output/diameter_eliteDB_largest.png", width = 30, height = 17.5, units = "cm")


# Highlighting certain paths  ---------------------------------------------
# get the igraph object that contains the path
path_of_interest <- shortest_paths(net_largest, 
                            from = names(V(net_largest)) =="A.P. Moeller - Maersk", 
                             to  = names(V(net_largest)) =="Advice A/S",
                             output = "both") # both path nodes and edges

path_of_interest
# path_of_interest object gives us a path for nodes ($vpath) and one for edges ($epath)

# create attributes like in the previous section and fill it with TRUE/FALSE values
E(net_largest)$path1 <- FALSE
V(net_largest)$path1 <- FALSE

# get the indices of the vertices that are in the path of interest
index <- which(names(V(net_largest)) %in% names(path_of_interest$vpath[[1]])) 
V(net_largest)$path1[index] <- TRUE
# to get the index for the links, we use this formula
E(net_largest, path=path_of_interest$vpath[[1]])$path1 <- TRUE

# plot it
# alpha, is a way to make a point, or an edge more transparent. alpha = 1 means that the object is untransparent and alpha = 0 means that it is totally transparent. 
ggraph(net_largest, layout='fr') + 
geom_edge_link0(aes(filter=path1==TRUE), color='red', width=1.2) + 
geom_edge_link0(aes(filter=path1==FALSE), color='grey50', alpha=0.5) + 
geom_node_point(aes(filter=path1==TRUE), color='red', size=5, alpha=0.5) + 
geom_node_point(aes(filter=path1==FALSE), color='black', size=5, alpha=0.25) + 
geom_node_label(aes(filter=path1==TRUE, label=name), color='black', size=3, alpha=0.25, nudge_y=-1) + 
theme_graph() 
# save to the output folder
ggsave('output/lektion02-path-example.png', width=30, height=17.5, unit='cm')

