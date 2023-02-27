---
title: "Session 3 - Collecting network data"
author: Alexander Gamerdinger
date: '2023-01-08'
slug: /collecting-network-data/
categories:
  - R
  - Teaching
tags:
description: 'Session on how to collecting network data'
summary: 'In this session, You will learn how to subset the data set den17 by tags, and how to collect network data from Orbis. Last, we cover how to visualize a two-mode network and to add network attributes'
---

## Session 3 - collection and construction of network data

In this session, you are faced with an important decision in regard to data collection for your exam projects. There are three possibilities:

1.  You use the `den17` data set, find a subset and analyze it with appropriate tools covered during the course

2.  You collect data using `orbis`, find a suitable subset and analyze it with appropriate tools covered during the course

3.  You collect your own relational data, create a graph object and analyze it with appropriate tools covered during the course

First, I will highlight how you can find subsets of the data set `den17`. We start by setting our working directory using `setwd()` and loading our packages. We load the data set `den17` and filter the sector column by corporations.

This time, we also load an additional R.file called `custom_functions.R` which can be downloaded below. Please place this R.file in your `r` folder.

{{< button href="r/custom_functions.R" target="_self" download= "custom_functions.R" >}}
Download customs_functions.R
{{< /button >}}


```r
## load working directory 
setwd("/Users/alexandergamerdinger/Desktop/PhD/teaching/virksomhedsstrategi_forår_2022")

# libs
library(data.table)
library(tidyverse)
library(igraph)
library(ggraph)
library(readxl)
library(writexl)
library(graphlayouts)
source("r/custom_functions.R")

# Load and manipulate data set --------------------------------------------
# Load
den <- read_csv("input/den17-no-nordic-letters.csv")

# we'll be looking only at corporations, that have a valid company ID in the
# Danish register for firms (the CVR register)
den1 <- 
  den %>% 
  filter(sector == "Corporations") %>% 
  filter(!is.na(cvr_affiliation))
```



We load the `custom_functions.R` file by putting it into the **`source()`** function. After this is done and executed, we have loaded, among others, the following custom functions into our environment:

1.  the function `show.all.tags()` which allows us to see all the tags of the data set `den`. This can be applied either on the raw data set or on any subset of `den` that contains the same column structure

2.  the function `has.tags()` which allows us to subset the data set `den` by specific tags

3.  the function `clean_orbis()` which loads an excel file from Orbis, selects a specific set of columns and cleans them

## 3.1 Sub-setting the data set `den`

We can see all tags of the raw data set `den` by using the function `show.all.tags()`. Below, only the first 20 entries are shown.


```r
# all tags of company boards
show.all.tags(den1)[1:20, ] 
```

```
##               Positions affiliations
## Animals              27            3
## Architecture         93           14
## Banks               354           36
## BILH                186           44
## Business              8            1
## BYGG                282           43
## Charity              16            1
## Children              8            1
## Clothing             11            1
## Commerce             46            6
## Commercials          37            7
## Communication        51           12
## Construction         48            8
## Consultants          97           21
## Consumers            11            1
## Corporation        7322         1120
## Counseling            5            1
## Culture             159           23
## Design               13            2
## DETH                184           28
```

To actually subset the data set, we can use the `has.tags()` function. This function can be applied to each tag (or several tags) that are found in the list above. There are three output options: the option `"den"`, which reproduces the `den17` data set, the option "`affil"` which gives a vector of affiliations and the option `"name"` which gives a vector of names.

The R chunk below only shows the first 20 rows for each output.


```r
# suppose we want to get a subset of den1 that includes all names and affiliations with the tag "Banks"
bank <- has.tags(den1, "Banks", result = "den")
```

```
##       Matched positions
## Banks               354
## 
## 
```

```r
# now, we only want the UNIQUE affiliations 
bank1 <- has.tags(den1, "Banks", result = "affil")
```

```
##       Matched positions
## Banks               354
## 
## 
```

```r
# and only UNIQUE names
bank2 <- has.tags(den1, "Banks", result = "name")
```

```
##       Matched positions
## Banks               354
## 
## 
```

```r
# we can also subset by several tags, first we make a vector
tags <- c("Banks", "Finance", "Pensions")

# this gives us the whole data tibble 
finance <- has.tags(den1, tags, result = "den")
```

```
##          Matched positions
## Banks                  354
## Finance                881
## Pensions               217
## 
## 
```

```r
finance[1:20, ]
```

```
## # A tibble: 20 × 17
##    name                  affil…¹ role  tags  posit…²     id sector type  descr…³
##    <chr>                 <chr>   <chr> <chr>   <dbl>  <dbl> <chr>  <chr> <chr>  
##  1 Aage Almtoft          Middel… Memb… Corp…       1  95023 Corpo… <NA>  Automa…
##  2 AAke Per-Urban Bäcks… Danske… Vice… Corp…     416 132356 Corpo… <NA>  Automa…
##  3 Alice Lykke           SEB Pe… Memb… Corp…     689  97986 Corpo… <NA>  Automa…
##  4 Allan Buch            Middel… Chai… Corp…     785  95018 Corpo… <NA>  Automa…
##  5 Allan Michael Luplau  Sygefo… Chie… Corp…    1064  99077 Corpo… <NA>  Automa…
##  6 Amund Skarholt        SOS In… Chai… Corp…    1274  98467 Corpo… <NA>  Automa…
##  7 Anders Bondo Christe… Laerer… Chai… Corp…    1364  58555 Corpo… Virk… Formand
##  8 Anders Bondo Christe… Laan &… Chai… Corp…    1384  94515 Corpo… <NA>  Automa…
##  9 Anders Buhl-Christen… Sparek… Memb… Corp…    1420  98558 Corpo… <NA>  Automa…
## 10 Anders Christen Obel  ERHVER… Memb… Corp…    1459  87665 Corpo… <NA>  Automa…
## 11 Anders Christian Dam  BRFkre… Chai… Bank…    1514 128889 Corpo… <NA>  Automa…
## 12 Anders Christian Dam  Jyske … Exec… Corp…    1518  93008 Corpo… <NA>  Automa…
## 13 Anders Eldrup         JURIST… Chai… Corp…    1650  92935 Corpo… <NA>  Automa…
## 14 Anders Hjulmand       Tryg    Memb… Corp…    1870 130413 Corpo… <NA>  Automa…
## 15 Anders Jensen 677     Nykred… Exec… Bank…    1979  96205 Corpo… Virk… Automa…
## 16 Anders Kristian Bech  Vestjy… Memb… Corp…    2111 100297 Corpo… <NA>  Automa…
## 17 Anders Rasmussen 827  MP PEN… <NA>  Corp…    2416 111468 Corpo… Virk… Cand.s…
## 18 Anette Eberhard       Alm. B… Memb… Corp…    3090  82511 Corpo… <NA>  Automa…
## 19 Anker Boye            Sampen… Chai… Corp…    3503  58509 Corpo… Virk… (forma…
## 20 Anne Gleerup          Sparek… Vice… Corp…    4071  98559 Corpo… <NA>  Automa…
## # … with 8 more variables: created <dttm>, archived <dttm>,
## #   last_checked <dttm>, cvr_person <dbl>, cvr_affiliation <dbl>,
## #   person_id <dbl>, affiliation_id <dbl>, gender <chr>, and abbreviated
## #   variable names ¹​affiliation, ²​position_id, ³​description
```

Some people and affiliations can have several tags. To find people with several tags, we filter the data by the tags we are interested in, and then use the `intersect()` function.


```r
a1 <- den1 %>% filter(grepl("Farming", tags)) %>% pull(name)

b1 <- den1 %>% filter(grepl("Banks", tags)) %>% pull(name)

c1 <- intersect(a1, b1)
```

## 3.2 Downloading data from Orbis

Second, I highlight how you can collect data from Orbis. As a student of CBS, you have free access to Orbis, which is a website that contains information about corporations around the world. Orbis is just one available option to collect data. Feel free to use others as well.

[CBS Link to Orbis](https://www.cbs.dk/en/library/databases/orbis)

To download data from Orbis, please follow the steps explained in this video. Please note, that Lasse recorded this video in 2020, and some filters have changed since then. This means that while you are able to reproduce the same filter choices, you will get different results.

[Video about how to use Orbis](https://cbs.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=7745539a-08ed-45d6-8da4-ab8b00ceb972)

For this exercise class, I have created a different data set than what you see in the video. Specifically, I have applied the following filters which you can see in the image below.

![Screenshot from Orbis](images/image-1015133488.png)

The data set that you can download here contains information about all public limited Companies in Copenhagen. You can download it by pressing the button below.

{{< button href="input/public_companies_cph.xlsx" target="_self" download= "public_companies_cph.xlsx" >}}
Download Orbis data
{{< /button >}}

If you follow the video above and add columns that include information about the shareholders, their titles, and their unique identification number, you can use the function `clean_orbis()` which I have created to make your lives a little easier. The function cleans the column names, certain data entries and classes and returns structure that is similar to the `den17` data set.


```r
df <- clean_orbis(path = "input/public_companies_cph.xlsx")
```

```
## New names:
## • `` -> `...1`
```

```r
glimpse(df)
```

```
## Rows: 25,917
## Columns: 8
## $ name        <chr> "Mr Ane Maersk Mc-Kinney Uggla", "Mr Lars Erik Brenoe", "M…
## $ affiliation <chr> "A.P. MOLLER HOLDING A/S", "A.P. MOLLER HOLDING A/S", "A.P…
## $ title       <chr> "Chairman", "Member of the Board", "Member of the Board", …
## $ id          <chr> "P043421359", "P041964972", "P645698736", "P041410476", "P…
## $ sector      <chr> "6499", "6499", "6499", "6499", "6499", "6499", "7990", "7…
## $ revenue     <dbl> 62187559, 62187559, 62187559, 62187559, 62187559, 62187559…
## $ n_employees <dbl> 93076, 93076, 93076, 93076, 93076, 93076, 85375, 85375, 85…
## $ gender      <chr> "male", "male", "female", "male", "male", "male", "male", …
```

If you add other columns than specified in the video above, the function `clean_orbis()` might produce errors. The `clean_orbis()` function enables you to use all remaining functions, such as creating an incidence matrix and a graph object, just like with the data set `den17`. There is no need to rename columns.

## 3.3 Loading and visualizing a two-mode network

After we have found a suitable subset of our data, we are ready to load a graph object. Since both `den17` and Orbis data have two columns (name and affiliation) which are used as input variables for our graph object, we *always* load data by first creating an `incidence` matrix.

There are also different ways of loading graph objects, such as through the `igraph` functions `graph_from_data_frame()` or `graph_from_edgelist()`. However, I only recommend to use these functions if you are dealing with an edge list or data frame that shows connections between entities of the same unit (one-mode edge lists). If the raw data contains a two-mode edge list (as in `den17` or in Orbis), it is better to load graph objects through an incidence or adjacency matrix.

Since we already covered on how to load a graph object through an incidence or adjacency matrix [here](https://agamerdinger.com/teaching/virksomhedsstrategi/introduction/#14-creating-graphs), I will just show the code.


```r
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

# look at gr
gr
```

```
## IGRAPH 7b8f29e UN-B 920 931 -- 
## + attr: type (v/l), name (v/c)
## + edges from 7b8f29e (vertex names):
## [1] Aage Almtoft            --Middelfart Sparekasse                    
## [2] AAke Per-Urban Bäckström--Danske Bank                              
## [3] AAse Kogsboell          --JURISTERNES OG OEKONOMERNES PENSIONSKASSE
## [4] Alice Lykke             --SEB Pension                              
## [5] Allan Bisgaard          --Koebstaedernes Forsikring                
## [6] Allan Buch              --Middelfart Sparekasse                    
## [7] Allan Kragh Thaysen     --Nykredit Forsikring (note)               
## [8] Allan Michael Luplau    --Sygeforsikring Danmark                   
## + ... omitted several edges
```

Visualizing two-mode networks is very similar to visualizing one-mode networks. Two mode networks have an attribute called `type` which you can see just above. It is a node attribute.

When we visualize two-mode networks, we need to make the `type` of the node visible, because otherwise, we will not know which node is a corporation and person respectively. We specify this by using the color argument.


```r
gr %>% 
  ggraph("fr") +
  geom_edge_link0(color = "gray40") +
  geom_node_point(aes(color = type)) +
  geom_node_point(aes(filter=type==FALSE), alpha = 0.8, size = 1) +
  geom_node_point(aes(filter=type==TRUE), alpha = 0.5, size = 2) +
  # changing the legend content
  scale_color_manual(values = c("steelblue", "salmon2"), 
                     labels = c("individuals", "corporations")) +
  labs(title = "Financial corporate interlocks in DK", 
       # changing the legend header
       color = "Node types") +
  theme_graph()
```

```
## Warning: Using the `size` aesthetic in this geom was deprecated in ggplot2 3.4.0.
## ℹ Please use `linewidth` in the `default_aes` field and elsewhere instead.
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/two_mode_network-1.png" width="672" />

We select the largest component exactly as we do with one-mode networks too. Let us select the largest component and visualize it.


```r
# Select largest component 
complist <- components(gr)
comps <- decompose.graph(gr)

# make index 
index <- 
  table(complist$membership) %>% 
  as_tibble(.name_repair = make.names) %>% 
  arrange(desc(n)) %>% 
  mutate(X = as.numeric(X)) %>% 
  pull(1)

# select biggest
comp1 <- comps[[index[1]]]

# visualize

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
  labs(title = "Biggest component of financial corporate interlocks in DK", 
       # changing the legend header
       color = "Node types") +
  theme_graph()
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/comp-1.png" width="672" />

## 3.4 Adding graph attributes to one-mode networks

If you want to visualize certain aspects of the underlying raw data, you have to first add them as graph attributes. Adding attributes from the original raw data can be more complicated than adding graph-specific attributes such as distance or degree.

#### 3.4.1 Attributes for person one-mode network

Here, I show how you can add the gender of the person as a graph attribute.


```r
# we want to add attributes to the person one-mode network gr1 

# first, make a tibble where you make a name and gender column
a1 <- tibble(
  name = V(gr1)$name, 
  gender = finance %>% count(name, gender) %>% pull(gender)
)

# second, check if the sequence of names are similar in the tibble and the graph object and then add it
all.equal(V(gr1)$name, a1$name)
```

```
## [1] TRUE
```

```r
# add it as a vertex attribute
V(gr1)$gender <- a1$gender

# visualize
gr1 %>% 
  ggraph("fr") +
  geom_edge_link0(color = "gray40") +
  geom_node_point(aes(color = gender), size = 1) +
  theme_graph()
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/one-mode-graph-att-1.png" width="672" />

#### 3.4.1 Attributes for corporate one-mode network

Here, I show how you can add the sector of a corporation as a graph attribute


```r
# remember that the corporate network is made from the object finance which includes the tags Banks, Finance and Pension
# first, we make an affiliation sub set for each.
bank <- has.tags(den1, "Banks", "affil")
```

```
##       Matched positions
## Banks               354
## 
## 
```

```r
fin <- has.tags(den1, "Finance", "affil")
```

```
##         Matched positions
## Finance               881
## 
## 
```

```r
pension <- has.tags(den1, "Pensions", "affil")
```

```
##          Matched positions
## Pensions               217
## 
## 
```

```r
# second, we create a data frame which binds three data frames together into one. We give each node a tag name. 

a1 <- rbind(
  tibble(name=fin, tag ="finance"), 
  tibble(name=bank, tag ="bank"), 
  tibble(name=pension, tag ="pension")
)

# second, we make an incidence matrix
a2 <- xtabs(formula = ~ name + tag, data = a1, sparse = T)

# change matrix to tibble format 
a2 <- a2 %>% as.matrix() %>% as_tibble(rownames = "name")

# let us look at the object
a2
```

```
## # A tibble: 108 × 4
##    name                                                     bank finance pension
##    <chr>                                                   <dbl>   <dbl>   <dbl>
##  1 A/S DANSK ERHVERVSINVESTERING                               0       1       0
##  2 ALD Automotive                                              0       1       0
##  3 Alm. Brand                                                  0       1       0
##  4 Alpha Holding                                               0       1       0
##  5 ALTOR EQUITY PARTNERS A/S                                   0       1       0
##  6 AP Pension                                                  0       1       1
##  7 Arbejdernes Landsbank                                       1       1       0
##  8 ARKITEKTERNES PENSIONSKASSE                                 0       0       1
##  9 Bank DNB Nord                                               1       1       0
## 10 BDO - Statsautoriseret revisionsaktieselskab (bestyrel…     0       1       0
## # … with 98 more rows
```

The object `a2` denotes `1` if a corporation belongs to a sector and `0` if it does not. With this information, we can use the `filter()` function to make vectors with different names depending on the possible firm types. Consequently, each firm type is assigned to the `firmtpye` attribute.


```r
# make vectors with different names depending on the possible firm types
only_bank <- a2 %>% filter(bank==1 & finance==0 & pension ==0) %>% pull(name)
only_finance <- a2 %>% filter(bank==0 & finance==1 & pension ==0) %>% pull(name)
only_pension <- a2 %>% filter(bank==0 & finance==0 & pension ==1) %>% pull(name)
bank_finance <- a2 %>% filter(bank==1 & finance==1 & pension ==0) %>% pull(name)
bank_pension <- a2 %>% filter(bank==1 & finance==0 & pension ==1) %>% pull(name)
finance_pension <- a2 %>% filter(bank==0 & finance==1 & pension ==1) %>% pull(name)
bank_finance_pension <- a2 %>% filter(bank==1 & finance==1 & pension ==1) %>% pull(name)


# make an empty network attribute
V(gr2)$firmtype <- NA

# rename for reach firm type 
V(gr2)$firmtype[which(V(gr2)$name %in% only_bank)] <- "only_bank"
V(gr2)$firmtype[which(V(gr2)$name %in% only_finance)] <- "only_finance"
V(gr2)$firmtype[which(V(gr2)$name %in% only_pension)] <- "only_pension"
V(gr2)$firmtype[which(V(gr2)$name %in% bank_finance)] <- "bank_finance"
V(gr2)$firmtype[which(V(gr2)$name %in% bank_pension)] <- "bank_pension"
V(gr2)$firmtype[which(V(gr2)$name %in% finance_pension)] <- "finance_pension"
V(gr2)$firmtype[which(V(gr2)$name %in% bank_finance_pension)] <- "bank_finance_pension"

# Look at the result
V(gr2)$firmtype
```

```
##   [1] "only_finance"         "only_finance"         "only_finance"        
##   [4] "only_finance"         "only_finance"         "finance_pension"     
##   [7] "bank_finance"         "only_pension"         "bank_finance"        
##  [10] "only_finance"         "only_finance"         "only_finance"        
##  [13] "bank_finance"         "only_finance"         "only_finance"        
##  [16] "only_finance"         "only_finance"         "only_finance"        
##  [19] "finance_pension"      "only_finance"         "bank_finance"        
##  [22] "bank_finance"         "only_finance"         "bank_finance"        
##  [25] "bank_finance"         "bank_finance"         "only_finance"        
##  [28] "bank_finance"         "bank_finance"         "only_finance"        
##  [31] "only_finance"         "only_finance"         "bank_finance"        
##  [34] "bank_finance"         "only_pension"         "only_finance"        
##  [37] "only_finance"         "finance_pension"      "only_finance"        
##  [40] "only_pension"         "only_finance"         "bank_finance"        
##  [43] "bank_finance"         "only_finance"         "only_finance"        
##  [46] "only_finance"         "bank_finance"         "only_pension"        
##  [49] "finance_pension"      "bank_finance"         "finance_pension"     
##  [52] "only_finance"         "only_finance"         "only_finance"        
##  [55] "only_finance"         "only_finance"         "bank_finance"        
##  [58] "finance_pension"      "only_finance"         "bank_finance"        
##  [61] "only_finance"         "bank_finance_pension" "bank_finance_pension"
##  [64] "bank_finance"         "only_finance"         "only_finance"        
##  [67] "only_finance"         "bank_finance"         "bank_finance"        
##  [70] "only_finance"         "bank_finance"         "finance_pension"     
##  [73] "finance_pension"      "finance_pension"      "only_pension"        
##  [76] "finance_pension"      "only_finance"         "only_finance"        
##  [79] "only_finance"         "only_finance"         "bank_finance"        
##  [82] "finance_pension"      "only_finance"         "only_finance"        
##  [85] "only_finance"         "only_finance"         "only_finance"        
##  [88] "only_finance"         "finance_pension"      "finance_pension"     
##  [91] "bank_finance"         "only_finance"         "bank_finance"        
##  [94] "bank_finance"         "bank_finance"         "bank_finance"        
##  [97] "bank_finance"         "bank_finance"         "bank_finance"        
## [100] "bank_finance"         "only_finance"         "only_finance"        
## [103] "only_finance"         "only_finance"         "only_finance"        
## [106] "bank_finance"         "only_finance"         "bank_finance"
```

Last, we select the largest component, and visualize the network with `firmtype` as the color node attribute.


```r
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
```

```
## Warning: ggrepel: 10 unlabeled data points (too many overlaps). Consider
## increasing max.overlaps
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/vis-firmtype-1.png" width="672" />
