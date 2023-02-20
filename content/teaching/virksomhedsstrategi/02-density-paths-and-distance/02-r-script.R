##########################
#
#  Øvelse 2: Network components, density and paths
#
###########################


# SETTING UP --------------------------------------------------------------

#setting the working directory
setwd("/Users/alexandergamerdinger/Library/CloudStorage/OneDrive-CBS-CopenhagenBusinessSchool/PhD/teaching/virksomhedsstrategi_2023")

# install new packages 
install.packages('graphlayouts')

#loading the libraries
library(tidyverse)
library(data.table)
library(ggraph)
library(igraph)
library(graphlayouts)

# loading data | If there is an error, it is probably because of your working directory
den <- read_csv("input/den17-no-nordic-letters.csv")

#filter dataset den to only include Corporations, then call it den1
den1 <- den %>% filter(sector == "Corporations")

# select only the people who are in the data at least twice 
# create a new variable, with tidyverse. 

den2 <- den1 %>% 
  # group by the column "name"
  group_by(name) %>% 
  # create a new variable called "N" and fill it with the number of rows per group
  mutate(N = n()) %>% 
  # this is a function to reorder columns, and to place the N function as first col of the data set
  select(N, everything()) %>% 
  # now, filter by the new column variable and select those, who are in the data set more than once. 
  filter(N > 1)

# Create incidence matrix
# Sparse T means that instead of 0, it will just make a "."
incidence <- xtabs(formula = ~ name + affiliation, data = den2, sparse = TRUE)

adj_c <- Matrix::t(incidence) %*% incidence

# load a corporation network object and call it "gr". Then, simplify it, to get rid of self-loops and weights

gr <- graph_from_adjacency_matrix(adj_c, mode = "undirected") %>% 
  simplify(remove.multiple = TRUE, remove.loops = TRUE)

# Density ---------------------------------------------------
# The density of a network is the probability that two random nodes of the network are linked with each other. It is calculated by dividing the given number of edges by the total possible number of edges.
# examples

e1 <- make_full_graph(40)
plot(e1, vertex.size=10, vertex.label=NA)
edge_density(e1, loops=FALSE)

e2 <- make_star(40)
plot(e2, vertex.size=10, vertex.label=NA) 
edge_density(e2, loops=FALSE)

e3 <- make_tree(40, children = 3, mode = "undirected") # try putting 'out' or 'in' in the mode-argument
plot(e3, vertex.size=10, vertex.label=NA) 
edge_density(e3, loops=FALSE)

# what the density of the graph object gr?

edge_density(gr, loops= FALSE)

# Components ---------------------------------------------------
# A component is connected sub graph 

# plot the graph and see that there is a couple of components
gr %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(color = "gray70") +
  geom_node_point(size=1.5) +
  theme_graph()

# what are the components
complist <- components(gr)

# what does they contain?
complist$membership[1:20] # notice that I am only looking at the first 20 because it is a massive vector
complist$csize
complist$no

# the. components() function is like map, it shows us how many components there are and who's belonging to which. But decompose.graph() actually cuts the graph.
# the decomposed graph - you are welcome to take a look if you want - but be wanted, it is a long list. 
comps <- decompose.graph(gr)

# create an index 
index <- 
  # tabulate complist$membership
  table(complist$membership) %>% 
  # save as tibble
  as_tibble(.name_repair = make.names) %>% 
  # make X numeric
  mutate(X = as.numeric(X)) %>% 
  # sort
  arrange(desc(n)) %>% 
  # pull the vector
  pull(1) 

# The object index shows the position of the graph components in comps ordered from biggest graph component to smallest. So, index[1] reflects the position in comps which contains the biggest component, index[2] reflects the position in comps with the second biggest component, and so on…
# 
# To get the actual graph object, we just need to select it in comps and then assign it to a new name.

#largest component 
comp1 <- comps[[index[1]]]

#second largest component 
comp2 <- comps[[index[2]]]

# density of the components
edge_density(comp1, loops = FALSE)

# visualizing the components
comp1 %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(color = "gray70") +
  geom_node_point(size=1.5) +
  theme_graph()

comp2 %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(color = "gray70") +
  geom_node_point(size=1.5) +
  theme_graph()

# How to find people and affiliations in components?

# in names, we store all affiliation names from the second largest component
names <- V(comp1)$name

# who are the people (and all of their attributes) that are a part of these boards?
den2 %>% 
  filter(affiliation %in% names) %>% 
  view()

# or look at everyone in the same affiliations from original data set. 
den %>% 
  filter(affiliation %in% names) %>% 
  view()


# Transitivity ------------------------------------------------------------
# Transitivity is a measure that represents the number of actual triads out of all possible triads in the network.
# Thereby, it measures the existence of tightly connected communities.

# examples
# a ring graph, which does not contain any triads
g1 <- make_ring(10)
plot(g1)

transitivity(g1) # 0 - no triads

# a sample graph
g2 <- sample_gnp(10, 4/10)
plot(g2)
transitivity(g2) 

# from component and graph object 
transitivity(gr) 

transitivity(comp1) 


# Path Lengths ------------------------------------------------------------

# Path lengths between nodes
# To get the path lengths between all nodes in the network, we simply use the distances() function. To view a matrix with all distances

distances(comp1) %>% view()

# mean distances in a connected graph (component)
# mean distance of the biggest comp1
mean_distance(comp1)

# iterates through mean distances of all of the components 
map_dbl(comps, mean_distance)

# Distances between two specific nodes
# To calculate the distance from specific node to all other nodes in a graph component, we use the following function:

distances <- 
  # distances function 
  distances(comp1, 
            # origin
            v = which(V(comp1)$name =="LEGO A/S"),
            # to 
            to = V(comp1))

# to view the object 
view(distances)

# how to visualize this?
# first, we need to make a network attribute
# we check if the cols of the distances object are equal to the names of the graph object

all.equal(target = colnames(distances), 
          current = names(V(comp1)))

# let us add a new graph attribute. It is a vertex (node) attribute, and hence, it has to include V(comp1). 
# So first, we create $distance and assign distances to it

V(comp1)$distance <- distances 

# and now, we create a second attribute, which is called lego, which is TRUE only if LEGO A/S appears

V(comp1)$lego <- ifelse(
  # the condition
  names(V(comp1)) == "LEGO A/S", 
  # what happens if condition is true 
  TRUE, 
  # what happens if it is not
  FALSE) 

# second, we plot

comp1 %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(color="gray50") +
  # filtering for FALSE, which is not LEGO
  geom_node_point(aes(filter=lego==FALSE, 
                      color=distance), size =3) +
  # filtering for TRUE, which is LEGO
  geom_node_point(aes(filter=lego==TRUE), 
                  color="darkred", size=3) +
  # distances as text for all nodes that are not lego
  geom_node_text(aes(filter=lego==FALSE, 
                     label = distance), 
                 color= "gray90", size=2.5) +
  # to change the legend
  labs(color = "Distance to Lego") +
  # to scale colors
  scale_color_viridis() +
  theme_graph() 

# save it - we save it in our output folder. 
ggsave("output/02-distance_to_lego.png", width = 30, height = 17.5, units = "cm")


# Diameter ----------------------------------------------------------------
# The diameter is a measure that only makes sense to use in connected graph components. It is the shortest path in the network. Networks with smaller diameters are often considered close communities. The shortest path in the network is calculated using the following functions: 

# diameter for the network component
diameter(comp1, directed = FALSE)

# Who is on the outskirts?
farthest.nodes(comp1, directed = FALSE)

# How to traverse it?
diam <- get.diameter(comp1, directed = FALSE)
diam

# Now, if we want to visualize the diameter, we need to first create two network attributes. A vertex attribute V(comp1)$diameter as well as an edge attribute E(comp1)$diameter

# making new graph attribute for nodes
V(comp1)$diameter <- 
  # condition to evaluate
  ifelse(V(comp1)$name %in% names(diam), 
         # if condition is true
         TRUE, 
         # if condition is false
         FALSE)

# making new graph attribute for edges. The code for edges are kind of different, this is igraph-specific
E(comp1)$diameter <- FALSE
E(comp1, path = diam)$diameter <- TRUE

# plot it
comp1 %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(aes(filter=diameter==FALSE), color = "gray60") +
  geom_edge_link0(aes(filter=diameter==TRUE), color = "red", width = 1.5) +
  geom_node_point(aes(filter=diameter==FALSE), color = "black") +
  geom_node_point(aes(filter=diameter==TRUE), color = "red", size =2) +
  geom_node_label(aes(filter=diameter==TRUE, label = name), nudge_y = -0.3, size =2.5, repel = TRUE) + 
  labs(title = 'Diameter in EliteDBs largest component') +
  theme_graph() 


# highlighting certain paths ----------------------------------------------
# Last, we can highlight the shortest path between two specific nodes in a connected network. First, we make the path and add this path as a network attribute.

# get the igraph object that contains the path
path_of_interest <- shortest_paths(comp1, 
                                   from = names(V(comp1)) =="A.P. Moeller - Maersk", 
                                   to  = names(V(comp1)) =="Advice A/S",
                                   output = "both") # both path nodes and edges

# path_of_interest object gives us a path for nodes ($vpath) and one for edges ($epath)
path_of_interest

# making new graph attribute for nodes
V(comp1)$path1 <- 
  # condition to evaluate
  ifelse(V(comp1)$name %in% names(path_of_interest$vpath[[1]]), 
         # if condition is true
         TRUE, 
         # if condition is false
         FALSE)

# making new graph attribute for edges.
E(comp1)$path1 <- FALSE
E(comp1, path = path_of_interest$vpath[[1]])$path1 <- TRUE

# visualize
comp1 %>% 
  ggraph(layout='fr') + 
  geom_edge_link0(aes(filter=path1==TRUE), color='red', width=1.2) + 
  geom_edge_link0(aes(filter=path1==FALSE), color='grey50', alpha=0.5) + 
  geom_node_point(aes(filter=path1==TRUE), color='red', size=5, alpha=0.5) + 
  geom_node_point(aes(filter=path1==FALSE), color='black', size=3, alpha=0.25) + 
  geom_node_label(aes(filter=path1==TRUE, label=name), color='black', size=3, alpha=0.25, nudge_y=-1) + 
  theme_graph() 

# save to the output folder
ggsave('output/lektion02-path-example.png', width=30, height=17.5, unit='cm')

 









