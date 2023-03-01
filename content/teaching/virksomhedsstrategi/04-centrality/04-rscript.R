##########################
#
#  Øvelse 4: Centrality measures
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

# Subsetting den17 dataset by tags ----------------------------------------

# load data 
den <- read_csv("input/den17-no-nordic-letters.csv")

# subset of corporations that have a valid cvr affiliation
den1 <- 
  den %>% filter(sector %in% "Corporations")

# Now, let us only select linkers
den2 <- 
  den1 %>% 
  group_by(name) %>% 
  mutate(N = n()) %>% 
  select(N, everything()) %>% 
  filter(N >1 ) 

# Let us create a graph using an incidence matrix
# Create the incidence matrix
incidence <- xtabs(formula = ~ name + affiliation, 
                   data = den2, 
                   sparse = TRUE)

# adjacency matrix
adj_c <- Matrix::t(incidence) %*% incidence

# one-mode graph 
gr <- graph_from_adjacency_matrix(adj_c, mode = "undirected") %>% 
  simplify(remove.multiple = TRUE, remove.loops = TRUE)


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


# centrality --------------------------------------------------------------

# create a table with all centrality metrics
metrics <- tibble(
  name = names(degree(comp1, mode="all")),
  degree =        degree(comp1, mode="all"),
  betweenness =   betweenness(comp1, directed=FALSE),
  closeness =     closeness(comp1, mode="all"), 
  eigen_ctr =         eigen_centrality(comp1, directed=FALSE)$vector
)

# look at the tibble 
head(metrics)

# look at the distributions
metrics %>% 
  # changing tibble format 
  pivot_longer(degree:eigen_ctr, 
               values_to = "ctr_value", 
               names_to = "ctr_names") %>% 
  # ggplot
  ggplot(aes(x = ctr_value, y = after_stat(density), group = ctr_names, )) +
  geom_histogram(bins = 20, fill = "gray70") +
  geom_density(alpha=0.4, fill = "steelblue1", color = "gray70") +
  # spreads it out to four panes 
  facet_wrap(~ctr_names, scales = "free" ) +
  theme_minimal()

# Count the number of individuals per affiliation and filter by those that are in the metrics tibble
a1 <- den %>% 
  # count the affiliations
  count(affiliation, sort = TRUE) %>% 
  # filter
  filter(affiliation %in% metrics$name) %>% 
  # rename to metge
  rename(N = n, name = affiliation)

# merge with net_metrics data set 
metrics <- 
  metrics %>% 
  left_join(a1, by = "name") %>% #merge by left data.frame which is net_metrics
  select(name, N, everything())

# make measure index
m1 <- c('degree', 'betweenness', 'closeness', 'eigen_ctr')

for (i in m1) {
  metrics <- metrics %>% arrange(desc(get(i)))
  metrics <- metrics %>% mutate(!!paste0(i, "_rank") := rleid(get(i)))
}

# Making a new variable called sum_rank, and arranging the dataset by this new variable
metrics <- 
  metrics %>% 
  mutate(sum_rank = degree_rank+betweenness_rank+closeness_rank+eigen_ctr_rank) %>% 
  arrange(sum_rank)

# take a look at metrics 
head(metrics)

view(metrics)

# centrality measure visualization ----------------------------------------

# be sure that the names of the affiliations are the same and sorted the same way
all.equal(metrics$name, V(comp1)$name) # woups

# however, they are just in a different order
all.equal(sort(metrics$name),sort(V(comp1)$name)) 

# We need to match these two values 
# The match function reproduces the order (in numerical location) of the first vector, to the second one. 
index <- match(V(comp1)$name,metrics$name)

new_order <- metrics$name[index] #see for yourself

all.equal(new_order, V(comp1)$name) # now it is the same order.

# we can also reorder the whole data frame
metrics <- metrics %>% arrange(factor(name, levels = name[index]))

all.equal(metrics$name, V(comp1)$name)

# add the attributes / variables
V(comp1)$size <- metrics$N
V(comp1)$closeness <- metrics$closeness
V(comp1)$betweenness <- metrics$betweenness

# we can also just add them directly 
V(comp1)$between <- betweenness(comp1, directed = FALSE)

# its the same 
all.equal(V(comp1)$between, V(comp1)$betweenness)

# Visualizing by betweenness 
comp1 %>% 
  ggraph(layout='fr') + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.35) + 
  geom_node_point(aes(color=betweenness, size=size), alpha=0.75) +
  scale_color_viridis() +
  geom_node_label(aes(
    filter=name %in% {metrics %>% filter(betweenness_rank < 10) %>% pull(name)} #baseR version would be: net_metrics$name[net_metrics$betweenness_rank < 10]
    ,label=name), alpha=0.65, size = 3, repel=T, force = 50) +
  theme_graph() 

ggsave('output/elitedb-graph-betweenness.png', width=30, height=17.5, unit='cm')

# another example, with closeness
comp1 %>% 
  ggraph(layout='fr') + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.35) + 
  geom_node_point(aes(color=closeness, size=size), alpha=0.75) + 
  theme_graph() + scale_color_viridis() +
  geom_node_label(aes(
    filter=name %in% {metrics %>% filter(grepl("bank", tolower(name))) %>% pull(name)},
    label=name), alpha=0.65, repel=T,size=3, force = 50)

ggsave('output/elitedb-graph-closeness.png', width=30, height=17.5, unit='cm')

