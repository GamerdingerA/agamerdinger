##############  #Lektion03 ################
# 
# Virksomhedsstrategi i et netværksperspektiv 
# Centralitetsmål 
#
#########################################

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

# Load and manipulate data set --------------------------------------------
# Load
den <- read_csv("input/den17-no-nordic-letters.csv")

# we'll be looking only at corporations, that have a valid company ID in the
# Danish register for firms (the CVR register)
den1 <- 
  den %>% 
  filter(sector == "Corporations") %>% 
  filter(!is.na(cvr_affiliation))

# Now, let us only select linkers: those people who are in the data set at least twice 
den2 <- 
  den1 %>% 
  group_by(name) %>% # group by name
  mutate(N = n()) %>% # make a new variable N, that counts the number of times a unique name is in the data set 
  select(N, everything()) %>% # rearrange columns 
  filter(N >1 ) # get rid of all people that are only in the data set once 

# Let us create a graph using an incidence matrix
# Create the incidence matrix
incidence <- xtabs(formula = ~ name + affiliation, data = den2, 
    sparse = TRUE)

net <- graph_from_incidence_matrix(incidence, directed = FALSE)

# Let us look at the one-node matrix for affiliations
net1 <- bipartite.projection(net)
net2 <- net1$proj2

# simplify
net2 <- igraph::simplify(net2, remove.multiple=TRUE, remove.loops=TRUE)

# how does it look? Quick visualization function
autograph(net2)

# Select components -------------------------------------------------------
# What are the components?
complist <- components(net2)

# Decompose graph
net3 <- decompose.graph(net2)

# Create "translation table" and arrange it
tbl <- table(complist$membership) %>% data.table() %>% arrange(desc(N))
tbl$V1 <- as.numeric(tbl$V1)

# Select the largest component
net_largest <- net3[[tbl$V1[1]]] # here, I just skipped the index step as shown in lektion02 

# plot 
net_largest %>% 
ggraph(layout='fr') + 
geom_edge_link0(color='grey', width=0.6, alpha=0.45) + 
geom_node_point(color='black', size=2, alpha=0.5)  + 
theme_graph()

# Metrics of centrality  --------------------------------------------------
# Degree | Betweenness | Closeness | Eigenvector 

# Degree centrality: How many direct links does a node have. 
# Since our network is undirected, we do not calculate in or out degree, but just use mode = "all"
deg <- degree(net_largest, mode = "all")

# What does the degree centrality distribution look like?
table(deg)

# Let us visualize this
deg1 <- tibble(deg)

deg1 %>% 
  ggplot() +
  geom_histogram(aes(x=deg), binwidth = 1, fill = "black") + #binwidth tells you the range of degree values that should be counted. Here, it is 1 meaning that all values are counted. 
  theme_minimal()

# Betweenness centrality: The number of shortest paths that pass through a node | Measure for broker position
betweenness(net_largest, directed=FALSE, weights=NA) 

# Closeness centrality: Centrality based on distance to others in the graph
closeness(net_largest, mode="all", weights=NA) 

# Eigenvector centrality: Centrality proportional to the sum of connection centralities
eigen_centrality(net_largest, directed=FALSE, weights=NA)$vector


# Centrality metrics in one table -----------------------------------------
# Create a table that includes all metrics 
net_metrics <- tibble(
  name = names(degree(net_largest, mode="all")),
  degree =        degree(net_largest, mode="all"),
  betweenness =   betweenness(net_largest, directed=FALSE, weights=NA),
  closeness =     closeness(net_largest, mode="all", weights=NA), 
  eigen =         eigen_centrality(net_largest, directed=FALSE, weights=NA)$vector
)

# Count the number of individuals per affiliation 
a1 <- den %>% count(affiliation, sort = TRUE) %>% rename(N = n, name = affiliation)

# Only include those that are part of our network
a2 <- a1 %>% filter(name %in% net_metrics$name)

# merge with net_metrics data set 
net_metrics <- 
net_metrics %>% 
  left_join(a2, by = "name") %>% #merge by left data.frame which is net_metrics
  select(name, N, everything())

# who has the highest values?
net_metrics %>% arrange(desc(degree))
net_metrics %>% arrange(desc(betweenness))
net_metrics %>% arrange(desc(closeness)) 
net_metrics %>% arrange(desc(eigen))

# Creating a rank system through a loop (You do not have to understand this)
for (i in c('degree', 'betweenness', 'closeness', 'eigen')) {
  net_metrics <- net_metrics %>% arrange(desc(get(i)))
  net_metrics <- net_metrics %>% mutate(!!paste0(i, "_rank") := rleid(get(i)))
}


# Making a new variable called sum_rank, and arranging the dataset by this new variable
net_metrics <- 
net_metrics %>% 
  mutate(sum_rank = degree_rank+betweenness_rank+closeness_rank+eigen_rank) %>% 
  arrange(sum_rank)

# visualize some metrics
ggplot(net_metrics) + geom_histogram(aes(x=eigen), binwidth=0.01) + theme_minimal()

# relationship between two metrics
ggplot(net_metrics) + geom_point(aes(x=closeness, y=betweenness, size=N)) + theme_minimal() 

# Correlation between the metrics in the graph
round(cor(as.matrix(net_metrics %>% select(closeness, eigen, betweenness, degree))),2)


# Network visualization ---------------------------------------------------
# be sure that the names of the affiliations are the same and sorted the same way
all.equal(net_metrics$name, V(net_largest)$name) # woups
all.equal(sort(net_metrics$name),sort(V(net_largest)$name)) # However the only thing that is different is the order between them

# We need to match these two values 
# The match function reproduces the order (in numerical location) of the first vector, to the second one. 
index <- match(V(net_largest)$name, net_metrics$name)
new_order <- net_metrics$name[index] #see for yourself

all.equal(new_order, V(net_largest)$name) # now it is the same order.

# Now, we want to reorder the whole data frame. 
# tidyverse way. 
net_metrics <- net_metrics %>% arrange(factor(name, levels = name[index]))
# baseR way 
net_metrics[index, ]

all.equal(net_metrics$name, V(net_largest)$name) # there we are

# add the attributes / variables
V(net_largest)$size <- net_metrics$N
V(net_largest)$closeness <- net_metrics$closeness
V(net_largest)$betweenness <- net_metrics$betweenness

# Visualizing by betweenness 
net_largest %>% 
ggraph(layout='fr') + 
geom_edge_link0(color='grey', width=0.6, alpha=0.35) + 
geom_node_point(aes(color=betweenness, size=size), alpha=0.75) + 
theme_graph() + scale_color_viridis() +
geom_node_label(aes(
  filter=name %in% {net_metrics %>% filter(betweenness_rank < 10) %>% pull(name)} #baseR version would be: net_metrics$name[net_metrics$betweenness_rank < 10]
,label=name), alpha=0.65, size = 3, repel=T, force = 50) 


ggsave('output/elitedb-graph-betweenness.png', width=30, height=17.5, unit='cm')

# another example, with closeness
net_largest %>% 
ggraph(layout='fr') + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.35) + 
  geom_node_point(aes(color=closeness, size=size), alpha=0.75) + 
  theme_graph() + scale_color_viridis() +
  geom_node_label(aes(
    filter=name %in% {net_metrics %>% filter(grepl("bank", tolower(name))) %>% pull(name)},
    label=name), alpha=0.65, repel=T,size=3, force = 50) 

ggsave('output/elitedb-graph-closeness.png', width=30, height=17.5, unit='cm')


# Other centrality measures (not so widely used)  -------------------------

# centrality measures on graph-level
centr_clo(net_largest, mode="all")$centralization
centr_degree(net_largest, mode='all')$centralization
eigen_centrality(net_largest)$value

# the page-rank algorithm of centrality, used in Google
page_rank(net_largest, directed=FALSE, weights=NA)$vector
