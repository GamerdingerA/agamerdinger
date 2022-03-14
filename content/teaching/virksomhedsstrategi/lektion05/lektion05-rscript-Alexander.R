
##############  #Lektion05 ################
# 
# Virksomhedsstrategi i et netværksperspektiv 
# Analyse af strategiske netværksroller
#
#########################################

## load working directory 
setwd("/Users/alexandergamerdinger/Desktop/PhD/teaching/virksomhedsstrategi_forår_2022")


# libraries
library(data.table)
library(tidyverse)
library(igraph)
library(ggraph)
library(readxl)
library(writexl)
library(graphlayouts)

# load custom functions to search for tags
# before loading, make sure to save this r script in your r folder, otherwise it will not be loaded properly. 
source('r/custom-functions-lektion05.r')

# Load data,  make network object,  select largest component --------------
den <- read_csv("input/den17-no-nordic-letters.csv")

# Select only Corporations with CVR number 
den1 <- den %>% filter(sector == "Corporations" & !is.na(cvr_affiliation))

# incidence matrix 
incidence <- xtabs(formula = ~ name + affiliation, data = den1, 
    sparse = TRUE)

# graph object 
net <- graph_from_incidence_matrix(incidence, directed = F)

# Make one-mode network and simplify 
net1 <- bipartite.projection(net, multiplicity = FALSE)
net2 <- net1$proj2
net2 <- simplify(net2, remove.multiple=TRUE, remove.loops=TRUE)

# Select largest component 
complist <- components(net2)
net3 <- decompose.graph(net2)
tbl <-table(complist$membership) %>% data.table() %>% arrange(desc(N))
tbl$V1 <- as.numeric(tbl$V1)
net_largest <- net3[[tbl$V1[1]]]

# Burt's constraint -------------------------------------------------------
# A measure of brokerage. It a measure on the node level that is higher for those that have less structural opportunities for bridging structural holes within a network. It measures how many direct ties of a node are redundant (connected with themselves). The more nodes are redundant, the more constraint a node is. 


# Let us make a data.frame with tibble() and look at the different measures. 
net_metrics <- tibble(
  
  name = names(constraint(net_largest)),
  constraint = constraint(net_largest),
  degree = degree(net_largest)                                                                                          
  
)

# Let us see who is least constraint?
net_metrics %>% arrange(constraint)

# We can also turn the metric around because low values for Burt's constraint means high values for brokerage. 
net_metrics <- net_metrics %>% mutate(brokerage = 1-constraint)
net_metrics %>% arrange(desc(brokerage))


# Visualize Burt's constraint ---------------------------------------------
# Make sure to test if sequence is correct before adding it to a network attribute. 
all.equal(net_metrics$name, V(net_largest)$name) 

# Add to our graph object
V(net_largest)$brokerage <- net_metrics$brokerage

# how to select the 10 most central people?
ten_largest <- net_metrics %>% 
  arrange(desc(brokerage)) %>% #arranage by brokerage
  slice(1:10) %>% # slice the data frame and look at the first 10 nodes
  pull(name) # extract only the names

# Make a graph layout that does not change every single time you run it. You can do that with the create_layout() function which we have covered in lektion04.
net_largest_layout <- create_layout(net_largest, 'fr')

# visualize
net_largest_layout %>% 
ggraph() +
  geom_edge_link0(color='grey', width=0.6, alpha=0.35) + 
  geom_node_point(aes(size=brokerage), alpha=0.75) + 
  theme_graph() + scale_color_viridis() +
  geom_node_label(aes(filter=name %in% ten_largest, label=name), alpha=0.75, repel=T) +
  geom_node_point(aes(filter=name %in% ten_largest,), color='red', size=.75, alpha=0.75) 


# Group assignment --------------------------------------------------------
# 1. Select a valid subset using the tags column of the den data set. 

# What are all the tags?
show.all.tags(den1)

# Let us look at some of the tags more specifically
has.tags(den1, 'Banks', result = "den") #returns the whole data frame den
bank <- has.tags(den1, 'Banks', result = "affil") #returns only the affiliations with the tags
has.tags(den1, 'Banks', result = "name") #returns only the names with the tags

# other examples
finans <- has.tags(den1, 'Finance')
pension <- has.tags(den1, 'Pensions')

finance <- c(bank, finans, pension)

# Select all names from the data set 
den2 <- den1 %>% filter(affiliation %in% finance)

