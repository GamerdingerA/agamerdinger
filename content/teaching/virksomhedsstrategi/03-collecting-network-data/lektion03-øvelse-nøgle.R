##############  #Lektion03 ################
# 
# Virksomhedsstrategi i et netværksperspektiv 
# Centralitetsmål - øvelse
#
###########################################

# set working directory, load libraries and the data set "den17-no-nordic-letters.csv"

setwd("/Users/alexandergamerdinger/Desktop/PhD/teaching/virksomhedsstrategi_forår_2022")

library(tidyverse)
library(ggraph)
library(igraph)
library(data.table)

den <- read_csv("input/den17-no-nordic-letters.csv")

# subset for sector = "Parliament" and select only linkers (or those that occur more than once in the data set)
df <- den %>% 
  filter(sector == "Parliament") %>% 
  group_by(name) %>% 
  mutate(N = n()) %>% 
  select(N, everything()) %>% 
  filter(N > 1)

# create a one-mode affiliation graph
incidence <- xtabs(~name+affiliation, data = df, sparse = T)

# Option 1: 
net <- graph_from_incidence_matrix(incidence, directed = FALSE)
net1 <- bipartite.projection(net)
net2 <- net1$proj2

# Option 2: making an adjacency and loading it from graph_from_adjacency_matrix() 
# adjacency <- Matrix::t(incidence) %*% incidence
# net <- graph_from_adjacency_matrix(adjacency, weighted = NULL, mode = "undirected")

# simplify the graph
net2 <- simplify(net2, remove.multiple = T, remove.loops = T)

# visualize the graph
net2 %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(color = "gray") +
  geom_node_point(size=3, alpha = 0.65) +
  theme_graph()

# select the biggest component
complist <- components(net2)
net3 <- decompose.graph(net2)

tbl <- table(complist$membership) %>% data.table() %>% arrange(desc(N))
tbl$V1 <- as.numeric(tbl$V1) 
net_largest <- net3[[tbl$V1[1]]]

# calculate the degree, closeness and betweenness of all nodes. and make a table. 

table <- tibble(

names = names(degree(net_largest, mode = "all")),
degree = degree(net_largest, mode = "all"),
betweenness = betweenness(net_largest, directed = FALSE, weights = NA),
closeness = closeness(net_largest, mode = "all", weights = NULL)
  
)


# Rank the table for each metric and try to find an explanation for why they are at the top. 

table <- table %>% arrange(desc(closeness))
table <- table %>% arrange(desc(betweenness))
table <- table %>% arrange(desc(degree))

# you can also do that in the bigger window with view()


# Now, add the centrality measures to the graph object of the biggest component and visualize it. Make sure that you each attributes are given to the correct names. Show the node size by betweenness, and highlight highly ranked affiliations by betweenness. Now compare your explanation that you have just before with the conclusions that you can draw from the graph. 
# test 
all.equal(names(degree(net_largest, mode = "all")), names(V(net_largest)))

# if you load the centrality measures directly to the attributes (for the same graph), then the sequence will be correct
V(net_largest)$betweenness <- betweenness(net_largest, directed = F, weights = NA)
V(net_largest)$degree <- degree(net_largest, mode = "all")
V(net_largest)$closeness <- closeness(net_largest, mode = "all")

# another way of loading attributes so you are very sure that they are in the correct sequence: 
V(net_largest)$betw <- table$betweenness[match(V(net_largest)$name, table$names)]

all.equal(names(table$betweenness[match(V(net_largest)$name, table$names)]), 
          names(V(net_largest)))

# Visualize the network and show the node size by betweenness
net_largest %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(color = "gray", alpha = 0.65) +
  geom_node_point(aes(size = betweenness, color = betweenness)) +
  scale_color_viridis() +
  geom_node_label(aes(filter=betweenness %in% sort(betweenness, decreasing = TRUE)[1:5], label = name), repel = TRUE, alpha = 0.55, force = 50, size = 3) +
  theme_graph()
  
