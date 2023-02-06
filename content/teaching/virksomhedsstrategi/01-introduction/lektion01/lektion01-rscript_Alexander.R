##########################
#
# What we have covered in Exercise Class 1
#
###########################


# SETTING UP --------------------------------------------------------------

#setting the working directory
setwd("/Users/alexandergamerdinger/Desktop/PhD/teaching/virksomhedsstrategi_forår_2022")

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

# look at the data 
den
#look at the data (big screen)
view(den)


# What is the pipe operator? ----------------------------------------------
# The pipe operator is this %>% and you can write it, using option + cmd + M (on mac) or Ctrl + Shift + M (on Windows)
# What does it do? It makes your code less error-prone and better understandable. 

# Look at the first 10 lines of the data set in the view mode. 
view(head(den)) # that is written without the pipe and it is difficult to read. 
den %>% head() %>% view() # that is how you do it with the pipe. Looks great, right?

# how many unique people are in the data set?
den$name %>% unique() %>% length()

# How many columns does the data set have and what are their names?
den %>% ncol() # same as ncol(den)
den %>% names() # same as names(den)


# Data manipulation -------------------------------------------------------

#select columns name and affiliation
den %>% select(name, affiliation) 

#count affiliation 
den %>% count(affiliation, sort= TRUE)
# check that this does the same thing than this: den %>% group_by(affiliation) %>% summarize(n = n())

#filter dat aset den to only include Commissions, then call it den2
den1 <- den %>% filter(sector == "Commissions")


# Making a two-mode network --------------------------------------------------------

# Make an incidence matrix
incidence <- xtabs(formula = ~name + affiliation, data=den1, sparse=TRUE)

# View the incidence matrix 
incidence %>% as.matrix() %>% view()

# make a graph object "net" from the incidence matrix
net <- graph_from_incidence_matrix(incidence, directed = FALSE)


# Visualize the network ---------------------------------------------------

# Visualize the two mode graph
net %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(color = "gray") +
  geom_node_point(aes(color = type), size=2) +
  geom_node_text(aes(filter=type==TRUE, label =name), size=2.5, repel = TRUE) +
  scale_color_manual(values=c("red", "blue"), labels=c("individuals", "commissions")) +
  theme_graph() 


# Make and visualize the one-mode network ---------------------------------

# Make a one mode graph and call it net1
net1 <- bipartite.projection(net)
# Choose the commissions-commissions graph
net2 <- net1$proj2

# Visualize the new graph
net2 %>% 
  ggraph(layout="fr") +
  geom_edge_link0(color = "gray60") +
  geom_node_point() +
  geom_node_text(aes(label=name), size=2) +
  theme_graph()
