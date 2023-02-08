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

##############  #section #################
# load and prepare data, make into network object
#########################################

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

# If you want to find out which component a company is a part of: 
table <- tibble(Names = complist$membership %>% names(), Component = complist$membership %>% as.vector())
# search for Lego
table[grep("Maersk", table$Names),]

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
deg <- degree(net2, mode = "all")

# What does the degree centrality distribution look like?
table(deg)

# Let us visualize this
deg1 <- tibble(deg)

deg1 %>% 
  ggplot(aes(x=deg)) +
  geom_histogram(binwidth = 1, fill = "black") +
  theme_minimal()

# Betweenness centrality: The number of shortest paths that pass through a node | Measure for broker position
betweenness(net2, directed=FALSE, weights=NA) 

# Closeness centrality: Centrality based on distance to others in the graph
closeness(net2, mode="all", weights=NA) 

# Eigenvector centrality: Centrality proportional to the sum of connection centralities
eigen_centrality(net2, directed=FALSE, weights=NA)$vector


# Centrality metrics in one table -----------------------------------------
# Create a table that includes all metrics 
net2_metrics <- tibble(
  name = names(degree(net2, mode="all")),
  degree =        degree(net2, mode="all"),
  betweenness =   betweenness(net2, directed=FALSE, weights=NA),
  closeness =     closeness(net2, mode="all", weights=NA), 
  eigen =         eigen_centrality(net2, directed=FALSE, weights=NA)$vector
)

# Count the number of individuals per affiliation 
a1 <- den %>% count(affiliation, sort = TRUE) %>% rename(N = n, name = affiliation)

# Only include those that are part of our network
a2 <- a1 %>% filter(name %in% net2_metrics$name)

# merge with net2_metrics data set 
net2_metrics <- 
net2_metrics %>% 
  left_join(a2, by = "name") %>% 
  select(name, N, everything())

# who has the highest values?
net2_metrics %>% arrange(desc(degree))
net2_metrics %>% arrange(desc(betweenness))
net2_metrics %>% arrange(desc(closeness))
net2_metrics %>% arrange(desc(eigen))

# Creating a rank system through a loop (You do not have to understand this)
for (i in c('degree', 'betweenness', 'closeness', 'eigen')) {
  net2_metrics <- net2_metrics %>% arrange(desc(get(i)))
  net2_metrics <- net2_metrics %>% mutate(!!paste0(i, "_rank") := rleid(get(i)))
}


# Making a new variable called sum_rank, and arranging the dataset by this new variable
net2_metrics <- 
net2_metrics %>% 
  mutate(sum_rank = degree_rank+betweenness_rank+closeness_rank+eigen_rank) %>% 
  arrange(sum_rank)

# look at the metrics in a spreadsheet, save into the output folder
write_xlsx(net2_metrics, 'output/net2_metrics.xlsx')

# visualize some metrics
ggplot(net2_metrics, aes(x=eigen)) + geom_histogram(binwidth=0.01) + theme_minimal()

# relationship between two metrics
ggplot(net2_metrics) + geom_point(aes(x=closeness, y=betweenness, size=N)) + theme_minimal()


# Network visualization ---------------------------------------------------
# be sure that the names of the affiliations are the same and sorted the same way
all.equal(net2_metrics$name, V(net2)$name) # woups
all.equal(sort(net2_metrics$name),sort(V(net2)$name)) # However the only thing that is different is the order between them

# We need to match these two values 
# The match function reproduces the order (in numerical location) of the first vector, to the second one. 
index <- match(V(net2)$name, net2_metrics$name)
new_order <- net2_metrics$name[index] #see for yourself

all.equal(new_order, V(net2)$name) # now it is the same order.
# Now, we want to reorder the whole data frame. 
# tidyverse way. 
net2_metrics <- net2_metrics %>% arrange(factor(name, levels = name[index]))
# baseR way 
net2_metrics[index, ]

all.equal(net2_metrics$name, V(net2)$name) # there we are

# add the attributes / variables
V(net2)$size <- net2_metrics$N
V(net2)$closeness <- net2_metrics$closeness
V(net2)$betweenness <- net2_metrics$betweenness

# Visualizing by betweenness 
net2 %>% 
ggraph(layout='fr') + 
geom_edge_link0(color='grey', width=0.6, alpha=0.35) + 
geom_node_point(aes(color=betweenness, size=size), alpha=0.75) + 
theme_graph(base_family = 'Helvetica') + scale_color_viridis() +
geom_node_label(aes(
  filter=name %in% {net2_metrics %>% filter(betweenness_rank < 10) %>% pull(name)} #baseR version would be: net2_metrics$name[net2_metrics$betweenness_rank < 10]
,label=name), alpha=0.65, size = 3, repel=T, force = 50) 


ggsave('output/elitedb-graph-betweenness.png', width=30, height=17.5, unit='cm')

# Visualizing by closeness 
net2 %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(width = 0.5, color ="gray70", alpha = 0.65) +
  geom_node_point(aes(size = size), alpha = 0.55, color = "black") +
  geom_node_point(aes(color = closeness), size = 1) +
  scale_color_viridis_b() +
  geom_node_label(aes(filter=name %in% {net2_metrics %>% filter(grepl("bank", tolower(name))) %>% pull(name)}, 
                      label = paste(name, "rank:", net2_metrics$closeness_rank[net2_metrics$name == name])), 
                  alpha = 0.75, repel = TRUE, force=60, size=2.5) +
  theme_graph() +
  labs(title = "Corporate interlocks in Denmark", color = "Closeness centrality", size = "Board size", 
       subtitle = "Selected Danish Banks highlighted by closeness centrality") +
  theme(plot.title = element_text(hjust =0.5, face = "bold"), 
        plot.subtitle = element_text(hjust =0.5, face = "italic"))

# another example, with closeness
ggraph(net2, layout='fr') + 
geom_edge_link0(color='grey', width=0.6, alpha=0.35) + 
geom_node_point(aes(color=closeness, size=size), alpha=0.75) + 
theme_graph(base_family = 'Helvetica') + scale_color_viridis() +
geom_node_text(aes(
  filter=tolower(name) %flike% c('bank')
,label=name), alpha=0.75, repel=T,size=5) 
ggsave('output/elitedb-graph-closeness.png', width=30, height=17.5, unit='cm')


##############  #section #################
# Other
# - not widely applied but still good to know
#########################################

# Correlation between the metrics in the graph
round(cor(as.matrix(net2_metrics[, .(closeness, eigen, betweenness, degree)])),2)

  
# centrality measures on graph-level
centr_clo(net2, mode="all")$centralization
centr_degree(net2, mode='all')$centralization
eigen_centrality(net2)$value

# the page-rank algorithm of centrality, used in Google
page_rank(net2, directed=FALSE, weights=NA)$vector







 
 