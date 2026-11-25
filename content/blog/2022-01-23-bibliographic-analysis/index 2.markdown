---
title: 'A different kind of literature review: networks and online data bases'
author: Alexander Gamerdinger
date: '2022-01-23'
showDate: true
draft: yes
slug: []
categories:
  - R
tags:
  - research
  - R Markdown
  - quantiative analysis
description: ''
---
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />

Keeping updated with the rapid pace and number of academic publications within almost every field of study has become increasingly difficult. At the same time, using the existing knowledge base correctly is a crucial ingredient for advancing a line of research.

In this blog post, I am presenting my approach of reviewing literature - that is, using **bibliographic analysis** to map existing fields of study.

Starting an academic project or a review without prior knowledge of the literature can be challenging. Luckily, there are some methods that can help you to get a better overview of the literature and some of its most representative papers. In this blog post, I will use bibliographic methods to investigate the literature on algorithmic work in the insurance sector.

## The intuition behind bibliographic methods

Bibliographic methods allow you to map out and understand the relational structure between papers in an academic literature. By using the reference lists of academic papers as edge lists, these methods allow for:

1.  The assessment of logical relationships between documents using density, connectivity and average distance measures between papers.

2.  The identification of widely-cited and centered documents, including those that bridge knowledge flows between groups of papers.

3.  The discovery of knowledge and research communities and the description of their similarities.

I will visualize bibliographic methods by using social network analysis. In this post, I will go in depth with two bibliographic methods, namely **bibliographic coupling** and **co-citation coupling**. Both are commonly used by scholars, but are operationalized in slightly different ways. While bibliographic coupling visualizes the interrelations of *citing papers*, co-citation coupling shows associations between *cited papers*.

<div class="figure" style="text-align: center">
<img src="Figure on difference between methods.png" alt="Bibliographic coupling and Co-citation Analysis" width="100%" />
<p class="caption">Figure 1: Bibliographic coupling and Co-citation Analysis</p>
</div>

Figure 1 reveals that bibliographic coupling links two papers if they share a common reference. Co-citation analysis on the other hand links two references with each other if they both appear in the same bibliographies. Both methods allow to account for the coupling strength of two papers by using edge weights.

## How to get started

1.  You need a sample of papers to analyze. This sample can be found and downloaded from websites like [Scopus](https://www.scopus.com) or [Web of Science](https://www.webofknowledge.com). The sample of scholarly works can either be found by keywords (using Boolean numerators) or by selecting all papers that are cited by an article of interest.

2.  Make sure to download all information about articles, *including* their list of references. Download them either in .csv or .bib format.

3.  Install and load three R packages. These are the [bibliometrix](https://www.bibliometrix.org/) package by Massimo Aria & Corrado Cuccurullo, the [igraph](https://igraph.org/r/) package by Gábor Csárdi and Tamás Nepusz, and the [ggraph](https://www.data-imaginist.com/2017/ggraph-introduction-layouts/) package by Thomas Lin Pedersen.


```r
install.packages(c("bibliometrix", "igraph", "ggraph"))

library(bibliometrix) ; library(igraph) ; library(ggraph)
```

## Loading the data

Once you have downloaded the data, convert it into a data frame, using the `convert2df()` function, specifying the source as well as the format of your downloaded file.


```r
# loading data using scopus 
df <- convert2df("transnational_networks.bib", dbsource = "scopus", format ="bibtex")

# loading data using web of science
df <- convert2df("transnational_networks.bib", dbsource = "wos", format ="bibtex")
```

The bibliometrix package offers other platforms to load data from, a more comprehensive guide can be found [here](https://www.bibliometrix.org/vignettes/Introduction_to_bibliometrix.html).



## Summary statistics

The function `biblioAnalysis()` allows you to get an overview of the sample you have collected. This gives insights into ranking such as most-cited works, most productive authors or journals. In addition, you can use the `plot()` function to visualize the same output which can be helpful to get a quick overview.


```r
output <- biblioAnalysis(df, sep = ";")

#plotting the result 
plot(output)[1] #most productive authors 

plot(output)[2] #most productive countries

plot(output)[3] #Annual Scientific Production
```

### Most-cited papers

Starting to scroll through the most cited papers is probably not the worst idea when getting to know a new literature. I used the following code, to make a tibble containing the name, DOI and number of citations for each paper. I added a little prefix that allows you to directly access the paper through it's DOI which saved me quite some time.


```r
tbl <- output$MostCitedPapers %>% as_tibble()

tbl %>% 
  rename(Paper = "Paper         ", 
         `Total Citations` = TC, 
         `Citations Per Year` = TCperYear) %>% 
  mutate(DOI = paste0("https://doi.org/", DOI), 
         across(DOI, str_replace_all, "https://doi.org/NA$", "NA"), 
         Paper = str_to_title(Paper), 
         `Citations Per Year` = round(`Citations Per Year`, 2)) %>% 
  select(Paper:`Citations Per Year`) %>% 
  slice_max(n = 10, order_by = `Citations Per Year`) 
# n = 10 is to show the top 10 most cited papers.
```

And this is how `tbl` looks like. Here, I only chose to show the first five entries.


```
## Warning: There was 1 warning in `mutate()`.
## ℹ In argument: `across(DOI, str_replace_all, "https://doi.org/NA$", "NA")`.
## Caused by warning:
## ! The `...` argument of `across()` is deprecated as of dplyr 1.1.0.
## Supply arguments directly to `.fns` through an anonymous function instead.
## 
##   # Previously
##   across(a:b, mean, na.rm = TRUE)
## 
##   # Now
##   across(a:b, \(x) mean(x, na.rm = TRUE))
```

<table class="table table-striped table-hover table-responsive" style="margin-left: auto; margin-right: auto;">
<caption>Table 1: Most cited papers</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Paper </th>
   <th style="text-align:left;"> DOI </th>
   <th style="text-align:center;"> Total Citations </th>
   <th style="text-align:center;"> Citations Per Year </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;width: 3.5cm; "> Brenner N, 2010, Global Netw </td>
   <td style="text-align:left;width: 8cm; "> https://doi.org/10.1111/j.1471-0374.2009.00277.x </td>
   <td style="text-align:center;width: 1.5cm; "> 1156 </td>
   <td style="text-align:center;"> 82.57 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 3.5cm; "> Betsill Mm, 2006, Global Gov </td>
   <td style="text-align:left;width: 8cm; "> https://doi.org/10.1163/19426720-01202004 </td>
   <td style="text-align:center;width: 1.5cm; "> 477 </td>
   <td style="text-align:center;"> 26.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 3.5cm; "> Stone D, 2004, J Eur Public Policy </td>
   <td style="text-align:left;width: 8cm; "> https://doi.org/10.1080/13501760410001694291 </td>
   <td style="text-align:center;width: 1.5cm; "> 521 </td>
   <td style="text-align:center;"> 26.05 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 3.5cm; "> Salehyan I, 2006, Int Organ </td>
   <td style="text-align:left;width: 8cm; "> https://doi.org/10.1017/S0020818306060103 </td>
   <td style="text-align:center;width: 1.5cm; "> 458 </td>
   <td style="text-align:center;"> 25.44 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 3.5cm; "> Erel U, 2010, Sociology </td>
   <td style="text-align:left;width: 8cm; "> https://doi.org/10.1177/0038038510369363 </td>
   <td style="text-align:center;width: 1.5cm; "> 314 </td>
   <td style="text-align:center;"> 22.43 </td>
  </tr>
</tbody>
</table>

### Most productive authors and journals

Table 2 shows the most productive authors in this sample including the code on how to produce it.


```r
tbl1 <- output$Authors %>% as_tibble() 

tbl1 %>% 
  mutate(AU = str_to_title(AU)) %>% 
  rename(Author = AU, 
         `Number of papers` = n) %>% 
  slice_max(n = 5, order_by = `Number of papers`) 
```

<table class="table table-striped table-hover table-responsive" style="margin-left: auto; margin-right: auto;">
<caption>Table 2: Most productive authors</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Author </th>
   <th style="text-align:center;"> Number of papers </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;width: 2cm; "> Carroll Wk </td>
   <td style="text-align:center;width: 2cm; "> 9 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 2cm; "> Mazzucato V </td>
   <td style="text-align:center;width: 2cm; "> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 2cm; "> Seabrooke L </td>
   <td style="text-align:center;width: 2cm; "> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 2cm; "> Henriksen Lf </td>
   <td style="text-align:center;width: 2cm; "> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 2cm; "> De Graaff N </td>
   <td style="text-align:center;width: 2cm; "> 5 </td>
  </tr>
</tbody>
</table>

A ranking of most productive journals is computed the following way.


```r
tbl2 <- output$Sources %>% as_tibble() 

tbl2 %>% 
  mutate(SO = str_to_title(SO)) %>% 
  rename(Journal = SO, 
         `Number of papers` = n) %>% 
  slice_max(n = 5, order_by = `Number of papers`) 
```

## Bibliographic Coupling

As stated already, bibliographic coupling links two *citing papers* if they have at least one common reference in their bibliography. I will use edge weights to account for the coupling strength of two articles. In other words, edge weights count the number of common references between two articles.

### Loading the data

To load the data, we use the `biblioNetwork()` function from the bibliometrix package which uses a very intuitive syntax to create an adjacency matrix. The adjacency matrix (which is basically is a symmetric matrix) can then be read using the `graph_from_adjacency_matrix()` function.


```r
# making an adjacency matrix (papers x papers)
ma <- biblioNetwork(df, 
                    analysis = "coupling", 
                    network = "authors", 
                    sep = ";", 
                    shortlabel = FALSE)

#making a weighted graph from adjacency matrix
graph <- graph_from_adjacency_matrix(ma, 
                                     mode = "undirected", 
                                     weighted = TRUE)
```

### Visualizing the graph

To visualize the graph object, I utilize the `ggraph` package. However, prior to that, I calculate the node degree (measure counting direct connections between nodes) and delete the isolating nodes from the graph since we are mostly interested in the connections between papers.


```r
# calculating degree of each node and placing it in variable called "degree"
V(graph)$degree <- degree(graph, mode = "all")

# deleting isolated nodes. 
Isolated <- which(V(graph)$degree == 0)
graph1 <- delete.vertices(graph, Isolated)

#visualizing the graph 
graph1 %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(aes(color = weight, width = weight), show.legend = FALSE) +
  scale_edge_color_gradient(low = "lightyellow3", high = "olivedrab4") +
  geom_node_point(aes(size = degree)) + 
  scale_size(range = c(0.2,3)) +
  geom_node_text(aes(filter=degree>30, label = name), repel = TRUE, size = 3, force = 20) +
  labs(title = "Bibliographic Coupling Network", 
       subtitle = "Visualization of different research communities") +
  theme_graph() +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(size = 18, hjust = 0.5, face = "italic"))
```

I use the *mds* layout which uses multidimensional scaling for generating the coordinates to get a more accurate picture of similarity and distance. In other words, the algorithm places two strongly tied papers closer together, and repels papers without ties.


```
## Warning: Using the `size` aesthetic in this geom was deprecated in ggplot2 3.4.0.
## ℹ Please use `linewidth` in the `default_aes` field and elsewhere instead.
```

```
## Warning: ggrepel: 712 unlabeled data points (too many overlaps). Consider
## increasing max.overlaps
```

<div class="figure" style="text-align: center">
<img src="{{< blogdown/postref >}}index_files/figure-html/making-graph-1.png" alt="Bibliographic Coupling Network" width="960" />
<p class="caption">Figure 2: Bibliographic Coupling Network</p>
</div>
