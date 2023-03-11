---
title: "Session 6 - Communities and cliques"
author: Alexander Gamerdinger
date: '2023-01-01'
slug: /community-detection/
categories:
  - R
  - Teaching
tags:
description: 'Session on  ommunities and cliques'
summary: 'This session will focus on the Louvain community detection algorithm and the visualization of communities in a network. We will also discover what cliques are, and how they differ from communities.' 
---

## Session 6 - Communities and cliques

This session marks the last 'tools' session of the course and focuses on node-communities. Communities are basically groups of nodes in the network that are very densely connected with each other. Detecting communities tells us something about the overall network structure.

As always, we are working with the data set `den17`. We also install two packages in the beginning of our script. The `purrr` packages, which facilitates to work with lists, and the `RcolorBrewer` package, which contains a ready-to-use color palettes for creating beautiful graphics. After having loaded the packages, we can execute the usual functions to transform `den17` into a graph object. See the code-snippet below:




```r
#setting the working directory
setwd("")

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
library(purrr)
library(RColorBrewer)

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

## 6.1 Community detection

## 
