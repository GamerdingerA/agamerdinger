
##########################
#
#  Øvelse 6: Communities and Cliques
#
###########################


# SETTING UP --------------------------------------------------------------

#setting the working directory
setwd("/Users/alexandergamerdinger/Library/CloudStorage/OneDrive-CBS-CopenhagenBusinessSchool/PhD/teaching/virksomhedsstrategi_2023")

# Package that makes it easier to work with lists. 
install.packages("purrr") 
# Package that enables you to change colors in visualizations
install.packages("RcolorBrewer")

#loading the libraries
library(tidyverse)
library(data.table)
library(ggraph)
library(igraph)
library(graphlayouts)
# for cleaning functions in orbis
source("r/custom_functions.R")
library(purrr)
library(RColorBrewer)

# Subsetting den17 dataset by tags ----------------------------------------

# load data 
den <- read_csv("input/den17-no-nordic-letters.csv")

# subset of corporations that have a valid cvr affiliation
den1 <- 
  den %>% filter(sector %in% "Corporations")

# Let us create a graph using an incidence matrix
# Create the incidence matrix
incidence <- xtabs(formula = ~ name + affiliation, 
                   data = den1, 
                   sparse = TRUE)

# adjacency matrix for chosing corporations
adj_c <- Matrix::t(incidence) %*% incidence

# one-mode graph for corporations
gr <- graph_from_adjacency_matrix(adj_c, mode = "undirected") %>% 
  simplify(remove.multiple = TRUE, remove.loops = TRUE)


# components & plotting ---------------------------------------------------

# What are the components?
complist <- components(gr)

# Decompose graph
comps <- decompose.graph(gr)

# Create an index
index <- 
  table(complist$membership) %>% 
  as_tibble(.name_repair = make.names) %>% 
  arrange(desc(n)) %>% 
  mutate(X = as.numeric(X)) %>% 
  pull(1)

# Select the largest component
comp1 <- comps[[index[1]]] 

# plot 
comp1 %>% 
  ggraph(layout='fr') + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.45) + 
  geom_node_point(color='black', alpha=0.6)  + 
  theme_graph()

# Communities -------------------------------------------------------------
# In this lesson, we are looking the louvain clustering method. This is how you can cluster nodes by communities. 

louvain <- cluster_louvain(comp1)

# Look inside the object
names(louvain)

# How large are the clusters?
louvain$membership # which community does a node belong to?
length(louvain$membership) # number of communities
table(louvain$membership) # distribution of members

# How many clustering iterations?
dim(louvain$memberships) 

# How effective has it clustered?

# Modularity is a measure that tells us the percentage of edges in the network that fall within the given communities compared to what we would expect if the graph was drawn at random. It tells us how dense edge connections are within communities, comparing to what we would expect in a random graph. 

louvain$modularity # 3 rounds of clustering
modularity(louvain) # last round 

# Visualizing communities -------------------------------------------------
# Do they have the same sequence of names?
all.equal(louvain$names, V(comp1)$name)

# adding membership as an attribute to community (with sprintf() function which makes membership values look nicer )
V(comp1)$community <- sprintf("%02d",louvain$membership) 

# DOES NOT NEED TO BE UNDERSTOOD --
# Creating edge attribute that gives the edge the same community color if it connects two nodes within an community, and gives it a black color if it connects two different communities. 

a1 <- as_tibble(as_edgelist(comp1), .repair_names = "unique")

E(comp1)$community <- map2_chr(a1$V1, a1$V2, function(.x, .y){
  ifelse(
    V(comp1)$community[which(V(comp1)$name==.x)] ==
      V(comp1)$community[which(V(comp1)$name==.y)],
    V(comp1)$community[which(V(comp1)$name==.x)],
    "9999") 
})

# Plotting 
comp1 %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(aes(filter=community!= "9999", color = community), width = 0.65, alpha = 0.6) +
  geom_edge_link0(aes(filter=community == "9999"), color = "black", width = 0.65, alpha = 0.4) + 
  geom_node_point(aes(color=community), alpha=0.95, size=2.5) + 
  theme_graph() 

# making sense of the community membership

# to find the nodes who are a part of a given community
names_c1 <- V(comp1)$name[which(V(comp1)$community == "07")]

# to visualize them in the graph

comp1 %>% 
  ggraph() +
  geom_edge_link0(aes(filter=community!='9999', color=community), width=0.6, alpha=0.55) + 
  geom_edge_link0(aes(filter=community=='9999'), color='black', width=0.6, alpha=0.35) + 
  geom_node_point(aes(color=community), alpha=0.95, size=3) + 
  geom_node_point(aes(filter=name %in% names_c1), color = "black", size=1.5) + 
  geom_node_label(aes(filter=name %in% names_c1, label=name, color=community), size = 2, repel=TRUE, force = 10) +
  theme_graph() 
  

# Improving or changing colors: 
# Option one: if not many unique colors present, I recommend using the viridis functions: scale_color_viridis() or scale_edge_color_viridis() 
# Option two: if many colors are present, use the RColorBrewer palettes

# RcolorBrewer ------------------------------------------------------------

# see all the color scales available
display.brewer.all()

# choose color and adapt to the amount of different colors you need 
amount <- V(comp1)$community %>% unique() %>% length()
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(amount)

comp1 %>%        
  ggraph() + 
  geom_edge_link0(aes(filter=community!='9999', color=community), width=0.6, alpha=0.55) + 
  geom_edge_link0(aes(filter=community=='9999'), color='black', width=0.6, alpha=0.35) + 
  geom_node_point(aes(color=community), alpha=0.95, size=3) + 
  theme_graph(base_family = 'Helvetica') + 
  scale_edge_color_manual(values = mycolors) +
  scale_color_manual(values = mycolors)

# Cliques  ----------------------------------------------------------------
# is a subset of nodes in a graph whose edge density is 1 - meaning that every member has ties. This is understood as the most intense possible type of community in an undirected graph. In comparison to community detection, cliques can be overlapping.

# How to find cliques: 
clique <- cliques(comp1, min = 2) # specify min 2 as there should be at least two people (exclude isolates)

# How many do we have?
length(clique)

# How many of them have a different size?
table(sapply(clique, length)) # you could  also use a function from the purr package: table(map_dbl(clique, length)). 

# For strategic queries: 
# Example: to find how many cliques Sydbank is in.
a1 <- keep(clique, function(x) any(x$name == 'Sydbank'))
length(a1)

# How large are the cliques that Sydbank is in?
table(sapply(a1, length)) # or from the purr package table(map_dbl(a1, length))

# Maximum cliques 
# A non-overlapping amount of cliques that are largest, meaning that they cannot be extended by including one more adjacent vertex
max_cliques <- maximal.cliques(comp1, min=2)
length(max_cliques) # how many?
table(sapply(max_cliques, length)) # how big?

# Further resources -------------------------------------------------------
all_layouts <- c(
  # these are from igraph
  'stress',
  'dh',
  'drl',
  'fr',
  'gem',
  'graphopt',
  'kk',
  'lgl',
  'mds',
  'sugiyama',
  'nicely', # automatic choosing of a fitting graph layout
  'bipartite',
  'star',
  'tree',
  # these are from ggraph 
  'dendrogram', 
  'manual', 
  'linear', 
  'matrix', 
  'treemap',  
  'circlepack', 
  'partition',  
  'hive'  
)

# This might be used to highlight communities in graphs
# https://blog.ouseful.info/2021/08/13/fragment-tools-of-production-ggalt/

# Other community detection algorithms ------------------------------------
# There are also other community detection algorithms that work in similar ways - attempting to maximize modularity. 
# As they all have the elements: membership and modularity, they can be used in the code above. 
# Others include: 

# cluster_louvain(comp1)
# cluster_edge_betweenness(comp1)
# cluster_fast_greedy(comp1)
# cluster_label_prop(comp1)
# cluster_leading_eigen(comp1)
# cluster_walktrap(comp1)
# cluster_spinglass(comp1)
# cluster_infomap(comp1)
# multilevel.community(comp1)

#  This post shortly explains the intuition between some of them: 
# https://stackoverflow.com/questions/9471906/what-are-the-differences-between-community-detection-algorithms-in-igraph

