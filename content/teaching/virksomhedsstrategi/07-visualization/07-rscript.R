
##########################
#
#  Øvelse 7: Network Visualization
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
source("r/custom_functions.R")
library(purrr)
library(RColorBrewer)
library(readxl) # if error, then install.packages("readxl")
library(writexl) # if error, then install.packages("writexl")


# Subsetting den17 dataset by tags ----------------------------------------

# load data 
den <- read_csv("input/den17-no-nordic-letters.csv")

# subset of corporations that have a valid cvr affiliation
den1 <- 
  den %>% filter(sector %in% "Corporations")

# selecting finance tag
den2 <- has.tags(den1, c("Finance", "Banks", "Pensions"), result = "den")

# creating an incidence matrix
incidence <- xtabs(formula = ~ name + affiliation, 
                   data = den2, 
                   sparse = TRUE)

# adjacency matrix for chosing corporations
adj_c <- Matrix::t(incidence) %*% incidence

# one-mode graph for corporations
gr <- graph_from_adjacency_matrix(adj_c, mode = "undirected") %>% 
  simplify(remove.multiple = TRUE, remove.loops = TRUE)

# quick visualization 
autograph(gr)

# components & plotting ---------------------------------------------------

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

# plot 
comp1 %>% 
  ggraph(layout='fr') + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.45) + 
  geom_node_point(color='black', alpha=0.6)  + 
  theme_graph()

# Add graph attributes ----------------------------------------------------


###### 1. ADD SECTOR ###### 

# finding subset for only the biggest component
den3 <- den2 %>% filter(affiliation %in% V(comp1)$name)

# finding those with only a tag for finance, pension or banks
finance <- has.tags(den3, "Finance", result = "affil")
bank <- has.tags(den3, "Banks", result = "affil")
pension <- has.tags(den3, "Pensions", result = "affil")

# making a tibble
a1 <- rbind(
  tibble(name = finance, tag = "finance"),
  tibble(name = bank, tag = "bank"),
  tibble(name = pension, tag = "pension")
)

# cross tabulating
a2 <- xtabs(formula = ~ name + tag, data = a1, sparse = TRUE) 

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

# make an empty network attribute
V(comp1)$firmtype <- NA

# rename for reach firm type 
V(comp1)$firmtype[which(V(comp1)$name %in% only_bank)] <- "only_bank"
V(comp1)$firmtype[which(V(comp1)$name %in% only_finance)] <- "only_finance"
V(comp1)$firmtype[which(V(comp1)$name %in% only_pension)] <- "only_pension"
V(comp1)$firmtype[which(V(comp1)$name %in% bank_finance)] <- "bank_finance"
V(comp1)$firmtype[which(V(comp1)$name %in% bank_pension)] <- "bank_pension"
V(comp1)$firmtype[which(V(comp1)$name %in% finance_pension)] <- "finance_pension"
V(comp1)$firmtype[which(V(comp1)$name %in% bank_finance_pension)] <- "bank_finance_pension"

# Look at the result
V(comp1)$firmtype

###### 2. ADD CENTRALITY ###### 

# degree
V(comp1)$degree <- degree(comp1, mode = "all")

#betweenness 
V(comp1)$betweenness <- betweenness(comp1, directed = FALSE)

# closeness 
V(comp1)$closeness <-  closeness(comp1, mode = "all")


# Visualization -----------------------------------------------------------

# all layouts
layouts <- c(
  'stress',
  'dh',
  'drl',
  'fr',
  'gem',
  'graphopt',
  'kk',
  'lgl',
  'mds',
  'sugiyama',
  'nicely', 
  'bipartite',
  'star',
  'tree',
  'dendrogram', 
  'manual', 
  'linear', 
  'matrix', 
  'treemap',  
  'circlepack', 
  'partition',  
  'hive'  
)

# visualization with node color as firmtype and node size as degree
v1 <- comp1 %>% 
  ggraph(
    #layout could be one of the above
    layout = "fr") +
  geom_edge_link0(
    # width of the line
    width=0.5, 
    # how visible the line is (from 0-1)
    alpha=0.4, 
    color = "grey60") + 
  geom_node_point(
    # different for every node
    aes(color=firmtype, size = betweenness),
    alpha = 0.95) + 
  geom_node_label(
    aes(filter=betweenness>250, color=firmtype, label=name), 
    size=2, 
    repel=TRUE) + 
  theme_graph() 

v2 <- v1 +
  # add a little dark point to where the labelled nodes are 
  geom_node_point(aes(filter=betweenness>250), color = "black", size =0.5)

# if you want to change the size range of nodes, you can use this command
v3 <- v2 + scale_size_continuous(range = c(1,7))

# you can also change the breaks
v3 + scale_size_continuous(range = c(1,7), 
                           breaks = c(0,50,100,150,200,250,300,350,400))

# for that, it might be useful to look at the range
range(V(comp1)$betweenness)

# changing the size of the firmtype points in the legend
v3 + guides(color = guide_legend(override.aes = list(size = 3)))

# change the names on the legend
v4 <- v3 + labs(
  size="Betweenness centrality", 
  color="Firmtype")

# change legend names and adapt colors
# choosing different color palette
number <- V(comp1)$firmtype %>% unique() %>% length()

# To retrieve a specific color sheme: 
display.brewer.all()
my_colors <- colorRampPalette(brewer.pal(12, name = "Paired"))(number) #first number represents the max. number of colors in the default palette "Paired". The second number specified at the end in (5) represents the number of colors we need.

# apply on a plot
# change not only the values, but also the labels
v5 <- v4 + 
  scale_color_manual(values = my_colors, 
                     breaks=c('only_finance', 'only_pension', 'bank_finance', 'finance_pension', 'bank_finance_pension'),
                     labels=c('Finance', 'Pension', 'Bank & Pension', 'Finance og Pension', 'Bank, Finance\n& Pension'))

# add a title and a subtitle 
v6 <- v5 + labs(title = "Figure 1: Corporate interlocks in the financial industry", 
                subtitle = "Something")


# Another example with closeness (and a gradient measure)

# base graph
p1 <- comp1 %>% 
  ggraph(layout = "graphopt") + 
  geom_edge_link0(width=.5, alpha=0.4) + 
  geom_node_point(aes(color=closeness, size=betweenness)) + 
  theme_graph() 

# adding a size scalling argument
p1 <-  p1 + scale_size_continuous(range=c(2, 10))

# changing the gradient color: viridis is a good option
p1 + scale_color_viridis() 
# otherwise create it yourself, colors can be chosen by looking at colors()
p1 + scale_color_gradient2(low='white', mid='wheat',  high='mediumvioletred', na.value='green') 

# save as png or as pdf 
ggsave('output/elitedb-graph-lektion06_01.png', plot = p1, width=30, height=17.5, unit='cm')


# Analysis on centrality measures using ggplot2 ---------------------------
# let us create a data frame with the tibble function

metrics <- tibble(
  name = names(degree(comp1, mode="all")),
  degree =        degree(comp1, mode="all"),
  betweenness =   betweenness(comp1, directed=FALSE, weights=NA),
  closeness =     closeness(comp1, mode="all", weights=NA), 
  eigen =         eigen_centrality(comp1, directed=FALSE, weights=NA)$vector,
  brokerage= 1-constraint(comp1) 
)       

#create a column with firmtypes 
list1 <- c("only_bank", "only_finance", "only_pension", "bank_finance", "bank_pension", "finance_pension", "bank_finance_pension")

# make an empty column called firmtype 
metrics$firmtype <- NA

# loop through list1 and rename NAs with names in list1 by their location. 
for(i in list1) {
  
  metrics$firmtype[which(metrics$name %in% get(i))] <- i
  
}
  
# What can you visualize: Option 1
metrics %>% 
ggplot(aes(x = brokerage, y = eigen, color=firmtype, size=closeness)) + 
  geom_point() + 
  scale_color_manual(values=my_colors)

# Option2
metrics %>% 
ggplot(aes(x = degree, fill=firmtype)) + geom_histogram(binwidth=1) + 
  labs(y='amount') + 
  theme_minimal() + 
  facet_wrap(~firmtype) + 
  scale_fill_manual(values=my_colors)

