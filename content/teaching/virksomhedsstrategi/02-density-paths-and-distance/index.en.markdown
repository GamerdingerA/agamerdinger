---
title: "Session 2: Network components, density and paths" 
author: Alexander Gamerdinger
date: '2023-01-26'
slug: /density-and-components/
categories:
  - R
  - Teaching
tags:
description: 'A session on network components, density and paths'
summary: "In this session, you will learn how to select components of a network, and how to calculate network density, shortest paths, and the diameter of a network."
---

## 2.1 Loading packages and data

We start where we ended session 02. Since we create a new `r-script`, we also need to load the packages and the data again. We install a new package called `graphlayouts`.


```r
# load working directory 
setwd("/Users/alexandergamerdinger/Library/CloudStorage/OneDrive-CBS-CopenhagenBusinessSchool/PhD/teaching/virksomhedsstrategi_2023")

# install new packages 

install.packages('graphlayouts')

# loading libraries 
library(data.table)
library(tidyverse)
library(igraph)
library(ggraph)
library(graphlayouts)

# load den17 from the input folder.

den <- read_csv("input/den17-no-nordic-letters.csv")

# we'll be looking only at corporations

den1 <- den %>% filter(sector == "Corporations")
```



#### 2.1.1 Making data subset

Similar to session 01, we are interested in looking at the `corporations` subset only, which we specify with the `filter()` function. However, this time, we want to further subset the data tibble to only include those individuals in the data set that appear at least twice.

To do this, we make use of the `group_by()` function which allows us to group our data set into "homogeneous" blocks before perform any operation on them. Here, we group by name.

Next, we use the `mutate()` function to add a new variable to the data set. In the mutate function, we specify the name of the new column before the equality sign, and then specify the operations with which the old column is mutated by after the sign. Here, we simply want to count the number of rows by group with the `n()` function.

Last, we filter the `N` column to select only those individuals, who appear in the data set more than once. We assign the series of modifications of `den1` to the the object `den2`.


```r
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
```

#### 2.1.2 Creating corporations graph object

After having modified out data set, we now create a graph object. For this, we re-use code from session 01.


```r
# Create incidence matrix
# Sparse T means that instead of 0, it will just make a "."
incidence <- xtabs(formula = ~ name + affiliation, data = den2, sparse = TRUE)

adj_c <- Matrix::t(incidence) %*% incidence

# load a corporation network object and call it "gr". Then, simplify it, to get rid of self-loops and weights

gr <- graph_from_adjacency_matrix(adj_c, mode = "undirected") %>% 
  simplify(remove.multiple = TRUE, remove.loops = TRUE)

#view graph object 
gr
```

```
## IGRAPH cebe942 UN-- 661 1332 -- 
## + attr: name (v/c)
## + edges from cebe942 (vertex names):
## [1] 3C Groups          --Nielsen & Nielsen Holding                
## [2] 3xN                --Hildebrandt & Brandi                     
## [3] 3xN                --Lead Agency                              
## [4] 5E Byg (Bestyrelse)--E 3-Gruppen                              
## [5] 5E Byg (Bestyrelse)--T. Hansen Gruppen                        
## [6] 7N                 --Brdr. A. & O. Johansen (AO)              
## [7] 7N                 --Kontorfaellesskabet paa Sankt Annae Plads
## [8] A-pressen          --Arbejdernes Landsbank                    
## + ... omitted several edges
```

## 2.2 Density

The density of a network is a measure that tells us about the probability that two random nodes of the network are linked with each other. It is calculated by dividing the given number of edges by the total possible number of edges. A complete graph, for example, would have a density of 1.

Here are some examples:

**Full Graph**


```r
e1 <- make_full_graph(40)
plot(e1, vertex.size=10, vertex.label=NA)
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/egraph-1.png" width="672" />

```r
edge_density(e1, loops=FALSE)
```

```
## [1] 1
```

**Star Graph**


```r
e2 <- make_star(40)
plot(e2, vertex.size=10, vertex.label=NA) 
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/sgraph-1.png" width="672" />

```r
edge_density(e2, loops=FALSE)
```

```
## [1] 0.025
```

**Tree Graph**


```r
e3 <- make_tree(40, children = 3, mode = "undirected") # try putting 'out' or 'in' in the mode-argument
plot(e3, vertex.size=10, vertex.label=NA) 
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/tgraph-1.png" width="672" />

```r
edge_density(e3, loops=FALSE)
```

```
## [1] 0.05
```

If we calculate the density of our graph object `gr` then we find it at 0.01. For those of you, who wonder why we add the argument `loop = FALSE` in the `edge_density()` function; this is because we only deal with simplified networks without self-loops. Hence, we need to add this argument.

## 2.3 Components of a graph

The graph object `gr`, which has `661` nodes and`1332` ties, looks like this if we visualize it:


```r
# plot
gr %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(color = "gray70") +
  geom_node_point(size=1.5) +
  theme_graph()
```

```
## Warning: Using the `size` aesthetic in this geom was deprecated in ggplot2 3.4.0.
## ℹ Please use `linewidth` in the `default_aes` field and elsewhere instead.
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/vis1-1.png" width="672" />

As we can see, there is a main graph component in the middle of the visualization, a few small components on the edges of the graph object.

In this case, it is interesting to look at the biggest component of the graph. We can get an overview over all components of a graph with the `igraph` function `components()`.

This function generates a list containing the following vectors:

-   `membership`, which is a vector assigning each node to a numbered component

-   `csize`, which returns the size of each component

-   `no`, which is the number of connected components


```r
# what are the components
complist <- components(gr)

# what does they contain?
complist$membership[1:20] # notice that I am only looking at the first 20 because it is a massive vector
```

```
##                            3C Groups                                  3xN 
##                                    1                                    1 
##                  5E Byg (Bestyrelse)                                   7N 
##                                    1                                    1 
##                            A-pressen                  A. Enggaard Holding 
##                                    1                                    2 
##                A.P. Moeller - Maersk        A/S DANSK ERHVERVSINVESTERING 
##                                    1                                    1 
##     Aalborg Engineering (bestyrelse)                    Aalborg Forsyning 
##                                    3                                    1 
##                Aalborg Stiftstidende Aalborg Zoologiske Have (Bestyrelse) 
##                                    1                                    1 
##      Aarhus Letbane I/S (Bestyrelse)                             Aarsleff 
##                                    1                                    1 
##         Accura Advokatpartnerselskab                       Actona Company 
##                                    4                                    1 
##    Adserballe & Knudsen (Bestyrelse)                 Advance (Bestyrelse) 
##                                    5                                    1 
##                           Advice A/S                      AGRO SUPPLY A/S 
##                                    1                                    1
```

```r
complist$csize
```

```
##  [1] 533   2   4   3   4   2   3   2   2   3   3   4   6   4   2   2   2   2   2
## [20]   2   5   2   2   2   2   2   3   2   2   2   2   2   2   3   3   2   3   2
## [39]   2   2   2   2   2   2   3   2   2   2   2   2   2   2   2
```

```r
complist$no
```

```
## [1] 53
```

To actually decompose the graph, and visualize the biggest and next biggest component, we use the function `decompose.graph()` which renders a list of small graph components.

Since we are interested in choosing the biggest component, we need to make sure that we select the correct graph component. This is done using a little trick, namely by tabulating `complist$membership` and then sorting it, and creating an index which is further used to select the biggest graph component.


```r
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
```

The object `index` shows the position of the graph components in `comps` ordered from biggest graph component to smallest. So, `index[1]` reflects the position in `comps` which contains the biggest component, `index[2]` reflects the position in `comps` with the second biggest component, and so on...

To get the actual graph object, we just need to select it in `comps` and then assign it to a new name.


```r
#largest component 
comp1 <- comps[[index[1]]]

#second largest component 
comp2 <- comps[[index[2]]]
```

After finding the components, we can now calculate their edge density by writing `edge_density(comp1, loops = FALSE)`. We can also visualize them:


```r
comp1 %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(color = "gray70") +
  geom_node_point(size=1.5) +
  theme_graph()
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/vis2-1.png" width="672" />

```r
comp2 %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(color = "gray70") +
  geom_node_point(size=1.5) +
  theme_graph()
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/vis2-2.png" width="672" />

#### 2.3.1 How to find people and affiliations in components?

A natural question that arises when cutting the graph into components is: How do I find out more about the members?

This is done in the following way:


```r
# in names, we store all affiliation names from the second largest component
names <- V(net_second)$names

# who are the people (and all of their attributes) that are a part of these boards?
den2 %>% 
  filter(affiliation %in% names) %>% 
  view()

# or look at everyone in the same affiliations from original data set. 
den %>% 
  filter(affiliation %in% names) %>% 
  view()
```

## 2.4 Transitivity

Transitivity is a measure that represents the number of actual triads out of all possible triads in the network. To understand the measure a little better, take a look at these graphs:


```r
# a ring graph, which does not contain any triads
g1 <- make_ring(10)
plot(g1)
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/trans-1.png" width="672" />

```r
transitivity(g1) # 0 - no triads
```

```
## [1] 0
```

```r
# a sample graph
g2 <- sample_gnp(10, 4/10)
plot(g2)
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/trans-2.png" width="672" />

```r
transitivity(g2) 
```

```
## [1] 0.4864865
```

Let us calculate the transitivity of the whole graph `gr` and the biggest component `comp1`.


```r
transitivity(gr) 
```

```
## [1] 0.3115276
```

```r
transitivity(comp1) 
```

```
## [1] 0.3089441
```

## 2.5 Path Lengths

The path length between two nodes is simply the sum of the weights of the edges traversed in the path. If a graph does not have any weights, it is assumed to be equal to 1. Since we are here looking at an unweighted graph, the length of the path is the number of edges traversed on that path.

**Path lengths between nodes**

To get the path lengths between all nodes in the network, we simply use the `distances()` function. To view a matrix with all distances, type `distances(comp1) %>% view()`

**Mean distances**

To get the mean distances between nodes in a component, try the following:


```r
# mean distance of the biggest comp1
mean_distance(comp1)
```

```
## [1] 5.168073
```

```r
# iterates through mean distances of all of the components 
map_dbl(comps, mean_distance)
```

```
##  [1] 5.168073 1.000000 1.666667 1.333333 1.000000 1.000000 1.333333 1.000000
##  [9] 1.000000 1.333333 1.333333 1.500000 1.400000 1.166667 1.000000 1.000000
## [17] 1.000000 1.000000 1.000000 1.000000 1.700000 1.000000 1.000000 1.000000
## [25] 1.000000 1.000000 1.000000 1.000000 1.000000 1.000000 1.000000 1.000000
## [33] 1.000000 1.000000 1.333333 1.000000 1.333333 1.000000 1.000000 1.000000
## [41] 1.000000 1.000000 1.000000 1.000000 1.333333 1.000000 1.000000 1.000000
## [49] 1.000000 1.000000 1.000000 1.000000 1.000000
```

**Distances between two specific nodes**

To calculate the distance from specific node to all other nodes in a graph component, we use the following function:


```r
distances <- 
  # distances function 
  distances(comp1, 
            # origin
            v = which(V(comp1)$name =="LEGO A/S"),
            # to 
            to = V(comp1))

# to view the object 

# view(distances)
```

Having retrieved these distances is informative, but maybe a little hard to imagine. Let us spend some time in visualizing this in a network graph. For this, we create two network attributes:

-   the network attribute `distance` which shows the distance between `LEGO A/S` and every other node in the graph

-   a logical network attribute `lego` which specifies where in the network `LEGO A/S` can be found

This is how we operate it in R.


```r
# first, we check if the cols of the distances object are equal to the names of the graph object

all.equal(target = colnames(distances), 
          current = names(V(comp1)))
```

```
## [1] TRUE
```

```r
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
```

Let us plot this.


```r
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
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/plot1-1.png" width="672" />


```r
# save it - we save it in our output folder. 
ggsave("output/distance_to_lego.png", width = 30, height = 17.5, units = "cm")
```
