---
title: "Session 5 - Brokerage and assortativity"
author: Alexander Gamerdinger
date: '2023-01-01'
slug: /brokerage-and-assortativity/
categories:
  - R
  - Teaching
tags:
description: 'Session on brokerage and assortativity'
summary: 'This session will focus on two topics. First, we will look at Burts constraint as a measure for brokerage. Second, we will cover assortativity as a measure of homophily in a network.'
---

## Session 5 - Brokerage and assortativity

This session connects to session 4 which introduces analysis tools for the node level. Here, we further investigate the broker role with Burt's constraint as a measure for brokerage. Second, we cover assortativity which measures homophily among nodes - or in other words, the likelihood that nodes with similar properties are connected with each other. 

But first, after having set working directory and loaded packages, we load our data set, create a graph object and select the biggest component. 




```r
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
```








