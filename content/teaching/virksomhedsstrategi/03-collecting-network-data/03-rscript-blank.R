##########################
#
#  Øvelse 3: Collecting network data
#
###########################


# SETTING UP --------------------------------------------------------------

#setting the working directory
setwd("")

#loading the libraries
library(tidyverse)
library(data.table)
library(ggraph)
library(igraph)
library(graphlayouts)
# for cleaning functions in orbis
source("custom_functions.R")

# Subsetting den17 dataset by tags ----------------------------------------

# load data 
den <- read_csv("input/den17-no-nordic-letters.csv")

# subset of corporations that have a valid cvr affiliation
den1 <- den %>% filter(sector %in% "Corporations" & !is.na(cvr_affiliation))

# what kind of sectors are included in the data set?
sector <- standard.sectors(sets = "Danish")

# How to see all the tags. 
# all tags of the raw data set including tags from other sectors 
show.all.tags(den) 
# all tags of company boards 
show.all.tags(den1) 

# Subsetting den1 by one tag

# suppose we want to get a subset of den1 that includes all names and affiliations with the tag "Banks"


# Subsetting den1 by several tags

# we can also subset by several tags, first we make a vector for "Banks", "Finance" and "Pensions"



# How to find those nodes that are members of two tags?



# Orbis -------------------------------------------------------------------

df <- clean_orbis(path = "input/public_companies_cph.xlsx")

# data set on "firm" level and call it f1. 
# get rid of name and title cols
# only look at the unique cols


# share of men and board size per company (will be shown in later exercises too)


# the firm data set together



# Loading graph object ----------------------------------------------------

# 1. Creating an incidence matrix and then an adjacency matrix

incidence <- xtabs(formula = ~ name + affiliation, data = finance, sparse = TRUE)

adj_i <- incidence %*% Matrix::t(incidence)

adj_c <- Matrix::t(incidence) %*% incidence

# 2. Loading two-mode network "gr" and one-mode networks "gr1" and "gr2"

gr <- graph_from_incidence_matrix(incidence, directed = FALSE) %>% 
  simplify(remove.multiple = TRUE, remove.loops = TRUE)

gr1 <- graph_from_adjacency_matrix(adj_i, mode = "undirected") %>% 
  simplify(remove.multiple = TRUE, remove.loops = TRUE)

gr2 <- graph_from_adjacency_matrix(adj_c, mode = "undirected") %>% 
  simplify(remove.multiple = TRUE, remove.loops = TRUE)

# Visualizing two-mode networks  ---------------------------------------

gr %>% 
  ggraph("fr") +
  geom_edge_link0(color = "gray40") +
  geom_node_point(aes(color = type)) +
  geom_node_point(aes(filter=type==FALSE), alpha = 0.8, size = 1) +
  geom_node_point(aes(filter=type==TRUE), alpha = 0.5, size = 2) +
  # changing the legend content
  scale_color_manual(values = c("steelblue", "salmon2"), 
                     labels = c("individuals", "corporations")) +
  labs(title = "Corporate interlocks in the Danish financial sector", 
       # changing the legend header
       color = "Node types") +
  theme_graph()

# Select largest component 
complist <- components(gr)
comps <- decompose.graph(gr)

index <- 
  table(complist$membership) %>% 
  as_tibble(.name_repair = make.names) %>% 
  arrange(desc(n)) %>% 
  mutate(X = as.numeric(X)) %>% 
  pull(1)

comp1 <- comps[[index[1]]]

comp1 %>% 
  ggraph("fr") +
  geom_edge_link0(color = "gray40") +
  geom_node_point(aes(color = type)) +
  geom_node_point(aes(filter=type==FALSE), alpha = 0.8, size = 1) +
  geom_node_point(aes(filter=type==TRUE), alpha = 0.5, size = 2) +
  geom_node_text(aes(filter=type==TRUE, label = name), size = 1.5,
                 # no overlap
                 repel = TRUE) +
  # changing the legend content
  scale_color_manual(values = c("steelblue", "salmon2"), 
                     labels = c("individuals", "corporations")) +
  labs(title = "Biggest component of corporate interlocks in the Danish financial sector", 
       # changing the legend header
       color = "Node types") +
  theme_graph()

# Adding graph attributes  ---------------------------------------

# person-person network: example: add gender
# quick visualization
gr1 %>% autograph()

# make a tibble (a1) where you make a name and gender column



# check if the sequence of names are similar in the tibble and the graph object and then add it


# visualize
gr1 %>% 
  ggraph("fr") +
  geom_edge_link0(color = "gray40") +
  geom_node_point(aes(color = gender), size = 1) +
  theme_graph()

# organization-organization network: example: add sector
bank <- has.tags(den1, "Banks", "affil")
fin <- has.tags(den1, "Finance", "affil")
pension <- has.tags(den1, "Pensions", "affil")

# first, let us create a data frame (a1) which binds three data frames together into one. We make a tibble with a "name" column and a "tag" column. 


# second, let us make an incidence matrix on name and tag 


#let us look at the result


# change matrix to tibble format 
a2 <- a2 %>% as.matrix() %>% as_tibble(rownames = "name")

# make vectors with different names depending on the possible firm types
only_bank <- a2 %>% filter(bank==1 & finance==0 & pension ==0) %>% pull(name)
only_finance <- a2 %>% filter(bank==0 & finance==1 & pension ==0) %>% pull(name)
only_pension <- a2 %>% filter(bank==0 & finance==0 & pension ==1) %>% pull(name)
bank_finance <- a2 %>% filter(bank==1 & finance==1 & pension ==0) %>% pull(name)
bank_pension <- a2 %>% filter(bank==1 & finance==0 & pension ==1) %>% pull(name)
finance_pension <- a2 %>% filter(bank==0 & finance==1 & pension ==1) %>% pull(name)
bank_finance_pension <- a2 %>% filter(bank==1 & finance==1 & pension ==1) %>% pull(name)

# Option 1: naming each manually
# make an empty network attribute
V(gr2)$firmtype <- NA

# rename for reach firm type 


# V(gr2)$firmtype[which(V(gr2)$name %in% only_pension)] <- "only_pension"
# V(gr2)$firmtype[which(V(gr2)$name %in% bank_finance)] <- "bank_finance"
# V(gr2)$firmtype[which(V(gr2)$name %in% bank_pension)] <- "bank_pension"
# V(gr2)$firmtype[which(V(gr2)$name %in% finance_pension)] <- "finance_pension"
# V(gr2)$firmtype[which(V(gr2)$name %in% bank_finance_pension)] <- "bank_finance_pension"

# [more advanced] Option 2: naming each through a loop (not obligatory to know)
# create a list with all 7 options
list <- c("only_bank", "only_finance", "only_pension", "bank_finance", "bank_pension", "finance_pension", "bank_finance_pension")

# create a loop where you loop over the list
for (i in list) {
  names <- which(V(gr2)$name %in% get(i)) # use get() to get the values that are within i
  V(gr2)$firmtype[names] <- i # name each firm type
}


# Look at the result
V(gr2)$firmtype #make sure that there are no NA values. 

# Select largest component 
complist <- components(gr2)
comps <- decompose.graph(gr2)

index <- 
  table(complist$membership) %>% 
  as_tibble(.name_repair = make.names) %>% 
  arrange(desc(n)) %>% 
  mutate(X = as.numeric(X)) %>% 
  pull(1)

comp1 <- comps[[index[1]]]

# visualize
comp1 %>% 
  ggraph(layout = "fr") + 
  geom_edge_link0(width=.5, alpha=0.4) + 
  geom_node_point(aes(color=firmtype), size=4) + 
  geom_node_label(aes(color=firmtype, label=name), size=2, nudge_y=-0.05, repel=TRUE) + 
  theme_graph() 


# Extra -------------------------------------------------------------------

###### LOADING THROUGH EDGE AND NODE LISTS ######

# getting rid of all other cols and just load an edge list
edgelist <- df %>% select(name, affiliation)

# create a graph from tibble
gr3 <- graph_from_data_frame(edgelist) %>% 
  simplify(remove.multiple = TRUE, remove.loops = TRUE)

# we have to manually add the fact that we are dealing with a two-mode network
# https://rpubs.com/pjmurphy/317838
V(gr3)$type <- bipartite_mapping(gr3)$type

# now, we can make a bipartite projection, and then we do not have to make a matrix multiplication. 
gr4 <- bipartite.projection(gr3)

person_network <- gr4$proj1
organ_network <- gr4$proj2

