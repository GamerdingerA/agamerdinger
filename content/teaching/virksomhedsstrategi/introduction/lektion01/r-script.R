##########################
#
#  Øvelse 1: Introduktion til netværksanalyse 
#
###########################


# SETTING UP --------------------------------------------------------------

#setting the working directory
setwd("")

# Install packages (only execute once) - if there is an error, check troubleshoot panel here https://agamerdinger.com/teaching/virksomhedsstrategi/
install.packages("data.table")
install.packages("tidyverse")
install.packages("ggraph")
install.packages("igraph")

#loading the libraries
library(tidyverse)
library(data.table)
library(ggraph)
library(igraph)

# loading data | If there is an error, it is probably because of your working directory
den <- read_csv("input/den17-no-nordic-letters.csv")

#select columns name and affiliation
den %>% select(name, affiliation) 

#count affiliation 
den %>% count(affiliation, sort= TRUE)
# check that this does the same thing than this: den %>% group_by(affiliation) %>% summarize(n = n())

#filter dat aset den to only include Commissions, then call it den2
den1 <- den %>% filter(sector == "Corporations")

# Making a two-mode network --------------------------------------------------------

# Make an incidence matrix
incidence <- xtabs(formula = ~name + affiliation, data=den1, sparse=TRUE)

# View the incidence matrix 
incidence[1:5,1:5]

# make an adjacency matrix for individuals
adj_i <- incidence %*% Matrix::t(incidence)
adj_i[1:5,1:5]

# make an adjacency matrix for corporations
adj_a <- Matrix::t(incidence) %*% incidence 
adj_a[1:5,1:5]


# make a graph object "net" from the incidence matrix
gr <- graph_from_incidence_matrix(incidence, directed = FALSE)

# make a graph from adjacency 
gr1 <- graph_from_adjacency_matrix(adj_i, mode = "undirected") %>% 
  simplify(remove.multiple = TRUE, remove.loops = TRUE)

# make a graph from adjacency 
gr2 <- graph_from_adjacency_matrix(adj_a, mode = "undirected") %>% 
  simplify(remove.multiple = TRUE, remove.loops = TRUE)

# Visualize the network ---------------------------------------------------

# Visualize the two mode graph
gr %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(color = "gray") +
  geom_node_point(aes(color = type), size=2) +
  geom_node_text(aes(filter=type==TRUE, label =name), repel = TRUE) +
  scale_color_manual(values=c("red", "blue"), labels=c("individuals", "corporations")) +
  theme_graph() 


# Make and visualize the one-mode network ---------------------------------

# Visualize the new graph
gr2 %>% 
  ggraph(layout="mds") +
  geom_edge_link0(color = "gray60") +
  geom_node_point() +
  geom_node_text(aes(label=name), size=1) +
  theme_graph()
