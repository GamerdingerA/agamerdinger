
##############  #Lektion04 ################
# 
# Virksomhedsstrategi i et netværksperspektiv 
# Identifikation af communities
#
#########################################


# Install and load packages -----------------------------------------------

## load working directory
setwd("/Users/alexandergamerdinger/Desktop/PhD/teaching/virksomhedsstrategi_forår_2022")

# New package that gives access to new functions for ggraph (e.g. drawing ellipses)
install.packages('ggforce')  
# Package that makes it easier to work with lists. 
install.packages('purrr') 

# libs
library(data.table)
library(tidyverse)
library(purrr)
library(igraph)
library(ggraph)
library(graphlayouts)
library(ggforce)
library(readxl)
library(writexl)

# Load and manipulate data  -----------------------------------------------

# load data set 
den <- read_csv("input/den17-no-nordic-letters.csv")

# Let us only look at Corporations that have a cvr affiliation
# This is an even more comprised version of code. You can also write:  den %>% filter(sector == "Corporations") %>% filter(!is.na(cvr_affiliation))
den1 <- den %>% filter(sector == "Corporations" & !is.na(cvr_affiliation))

# Load graph object  ------------------------------------------------------
# Make incidence matrix 
incidence <- xtabs(formula = ~ name + affiliation, data = den1, 
                           sparse = TRUE)

# make a graph object from the incidence matrix 
net <- graph_from_incidence_matrix(incidence, directed = FALSE)

# choose a one-node affiliation network
net1 <- bipartite.projection(net, multiplicity = FALSE) # multiplicity = FALSE says that we do not want attribute weight to be created. 
# Now let us choose the the affiliation network 
net2 <- net1$proj2

# simplify the graph
# we use igraph:: before simplify because the package purr also has a function called simplify. In this way, we specify which package we are using. 
net2 <- igraph::simplify(net2, remove.loops = TRUE, remove.multiple = TRUE)


# Assortativity -------------------------------------------------------------
# Assortativity measures the extend to which nodes with similar properties are connected to each other. The assortativity coefficient ranges between -1 and 1, where 1 indicates that there is a high likelihood of two vertices with the same properties being connected. Networks that have an assortativity of 0 are usually referred to as neutral. 

# For categorical assortativity, we use the function assortativity_nominal()
# For continuous assortativity, we use the function assortativity()
# For assortativity based on degree we use the function assortativity_degree. It measures similarity on degree - e.g. those that have many friends also hang out with other popular people. 

# Assortativity degree 
assortativity_degree(net2, directed=FALSE)

# Continuous Assortativity ------------------------------------------------
# To calculate similarity between affiliations, we need to create a new variable - gender proportion. 

# What is the gender proportion?
den1 %>% count(gender)

# Let's aggregate gender proportions for each affiliation (but only look at women and men)
gender <- den1 %>% 
  filter(gender %in% c("Women", "Men")) %>% #filter for women and men only
  count(affiliation, gender) %>% #count amount of women and men per affiliation
  group_by(affiliation) %>% # do something, but group by affiliation first
  mutate(n_total = sum(n), # generates the total number of board members per affiliation
         share = n/(sum(n)))  # generates the share of women and men. 

# Now, let us focus on one share: the share of men in the dataset. 
Men <- gender %>% filter(gender == "Men")
Women <- gender %>% filter(gender == "Women" & n == n_total)
Women$share = 0

Men <- Men %>% select(affiliation, share)
Women <- Women %>% select(affiliation, share)

# create a common object 
gender2 <- rbind(Men, Women)

# check if they are same than the graph
all.equal(gender2$affiliation, V(net2)$name)
index <- match(V(net2)$name, gender2$affiliation)
gender2 <- gender2 %>% arrange(factor(affiliation, levels = affiliation[index]))
all.equal(gender2$affiliation, names(V(net2)))

# Let us add gender share as an attribute to the graph 
V(net2)$share_men <- gender2$share

# Select largest component 
complist <- components(net2)
net3 <- decompose.graph(net2)
tbl <-table(complist$membership) %>% data.table() %>% arrange(desc(N))
tbl$V1 <- as.numeric(tbl$V1)
net_largest <- net3[[tbl$V1[1]]]

# Visualize 
net_largest %>% 
ggraph(layout = "stress") + 
geom_edge_link0(width=.5, alpha=0.4) + 
geom_node_point(aes(color=share_men), size=5) + 
theme_graph() +
scale_color_gradient2(low='firebrick4', mid='grey80', high='dodgerblue4', midpoint=0.5, na.value='pink') 

# Calculate assortativity for share of men
assortativity(net_largest, V(net_largest)$share_men, directed=FALSE)

# Categorical Assortativity ------------------------------------------------

# let us create a random categorical variable
a1 <- sample(c("Female-dominated", "Male-dominated", "Gender-balanced"), 
               vcount(net_largest), 
               replace = TRUE)

V(net_largest)$random <- a1 

net_largest %>% 
ggraph(layout = "fr") + 
geom_edge_link0(width=.5, alpha=0.4) + 
geom_node_point(aes(color=random), size=3) + 
theme_graph() 

# Make sure to write as.factor(), so it is recognized as a categorical variable
assortativity_nominal(net_largest, as.factor(V(net_largest)$random), directed=FALSE)

# Template to add external variables to EliteDB ---------------------------
# first, you create a data frame that includes the names of the nodes of the network
a1 <- tibble(name = V(net_largest)$name)

# Save this as an excel file where you can manually add a variable
write_xlsx(a1, 'output/template_external_variable.xlsx')

# After you have added new variables, save the file, and then load it again.
a2 <- read_xlsx('output/template_external_variable.xlsx')
a2 <- as_tibble(a2)

# check if you have the correct sequence with all.equal() 
# If yes, then add it to the graph object as an attribute. 
V(net_largest)$random <- a2$V2


# Communities -------------------------------------------------------------
# In this lesson, we are using the louvain clustering method, the one that is described in the article that was assigned for today - https://agamerdinger.com/teaching/virksomhedsstrategi/lektion04/Blondel_et_al_2008.pdf - don't worry, you do not have to understand the math behind it. 

louvain1 <- cluster_louvain(net_largest)

# Look inside the object
names(louvain1)

# How large are the clusters?
louvain1$membership # which community does a node belong to?
length(louvain1$membership) # number of communities
table(louvain1$membership) # distribution of members

# How many clustering iterations?
dim(louvain1$memberships) 

# How effective has it clustered?
louvain1$modularity # 3 rounds of clustering
modularity(louvain1) # last round 

# Modularity is a measure that tells us the percentage of edges in the network that fall within the given communities compared to what we would expect if the graph was drawn at random. It tells us how dense edge connections are within communities, comparing to what we would expect in a random graph. 

# Visualizing communities -------------------------------------------------
# Do they have the same sequence of names?
all.equal(louvain1$names, V(net_largest)$name)

# adding membership as an attribute to community (with sprintf() function which makes membership values look nicer )
V(net_largest)$community <- sprintf("%02d",louvain1$membership) 

# DOES NOT NEED TO BE UNDERSTOOD --
# Creating edge attribute that gives the edge the same community color if it connects two nodes within an community, and gives it a black color if it connects two different communities. 

a1 <- as_tibble(as_edgelist(net_largest))

E(net_largest)$community <- map2_chr(a1$V1, a1$V2, function(.x, .y){
  ifelse(
    V(net_largest)$community[which(V(net_largest)$name==.x)] ==
    V(net_largest)$community[which(V(net_largest)$name==.y)],
    V(net_largest)$community[which(V(net_largest)$name==.x)],
  "9999") 
})

# Plotting 
net_largest %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(aes(filter=community!= "9999", color = community), width = 0.65, alpha = 0.6) +
  geom_edge_link0(aes(filter=community == "9999"), color = "black", width = 0.65, alpha = 0.4) + 
  geom_node_point(aes(color=community), alpha=0.95, size=2.5) + 
  theme_graph() 

# Plotting communities in more distinct groups -----------------------------
# We are doing a little trick, namely making a new network object, where we give weights to edges that are in a cluster together. This will force the graph to plot clusters in a more exclusive way. After we have done this, we are just making use of the new coordinates of nodes and edges, while still plotting the original network graph. 

# copy the graph object, creating weights and setting them to one. 
net2_tmp <- net_largest
E(net2_tmp)$weight = 1

# DOES NOT NEED TO BE UNDERSTOOD --
# A loop that goes through all communities, selects all nodes in each community, and adds an extra weight to them 
for(i in unique(V(net_largest)$community)) {
    net2_vertex = which(V(net_largest)$community == i)
    net2_tmp = add_edges(net2_tmp, combn(net2_vertex, 2), attr=list(weight=0.25))
} 

# Make a layout. Only those layouts that support weights can be used. 
net2_tmp_layout <- create_layout(net2_tmp, 'fr')
net_largest_layout <- create_layout(net_largest, 'fr')

# look at the layout object: 
names(net2_tmp_layout)

# let us copy the coordinates over to our original graph object 
net_largest_layout$x <- net2_tmp_layout$x
net_largest_layout$y <- net2_tmp_layout$y

# Now, let us plot the new version 
net_largest_layout %>% 
ggraph() + 
geom_edge_link0(aes(filter=community!='9999', color=community), width=0.6, alpha=0.55) + 
geom_edge_link0(aes(filter=community=='9999'), color='black', width=0.6, alpha=0.35) + 
geom_node_point(aes(color=community), alpha=0.95, size=3) + 
theme_graph() +
geom_node_label(aes(filter=name %in% grep("bank", V(net_largest)$name, value = TRUE, ignore.case = TRUE), label=name, color=community), repel=TRUE) +
geom_node_point(aes(filter=name %in% grep("bank", V(net_largest)$name, value = TRUE, ignore.case = TRUE)), color='black', size=1) 

# Improving or changing colors: 
# Option one: if not many unique colors present, I recommend using the viridis functions: scale_color_viridis() or scale_edge_color_viridis() 
# Option two: if many colors are present, use the RColorBrewer palettes

# RcolorBrewer ------------------------------------------------------------
# install.packages(RcolorBrewer)
library(RColorBrewer)
# see all the color scales available
display.brewer.all()

# choose color and adapt to the amount of different colors you need 
amount <- V(net_largest)$community %>% unique() %>% length()
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(amount)

net_largest_layout %>%        
ggraph() + 
geom_edge_link0(aes(filter=community!='9999', color=community), width=0.6, alpha=0.55) + 
geom_edge_link0(aes(filter=community=='9999'), color='black', width=0.6, alpha=0.35) + 
geom_node_point(aes(color=community), alpha=0.95, size=3) + 
theme_graph(base_family = 'Helvetica') + 
geom_mark_ellipse(data=net_largest_layout, aes(x=x, y=y, color=community), alpha=0.9) +
scale_edge_color_manual(values = mycolors) +
scale_color_manual(values = mycolors)


# Other community detection algorithms ------------------------------------
# There are also other community detection algorithms that work in similar ways - attempting to maximize modularity. 
# As they all have the elements: membership and modularity, they can be used in the code above. 
# Others include: 

# cluster_louvain(net_largest)
# cluster_edge_betweenness(net_largest)
# cluster_fast_greedy(net_largest)
# cluster_label_prop(net_largest)
# cluster_leading_eigen(net_largest)
# cluster_walktrap(net_largest)
# cluster_spinglass(net_largest)
# cluster_infomap(net_largest)
# multilevel.community(net_largest)

#  This post shortly explains the intuition between some of them: 
# https://stackoverflow.com/questions/9471906/what-are-the-differences-between-community-detection-algorithms-in-igraph


# Cliques  ----------------------------------------------------------------
# is a subset of nodes in a graph whose edge density is 1 - meaning that every member has ties. This is understood as the most intense possible type of community in an undirected graph. In comparison to community detection, cliques can be overlapping.
# How to find cliques: 
clique <- cliques(net_largest, min = 2) # specify min 2 as there should be at least two people (exclude isolates)

# How many do we have?
length(clique)

# How many of them have a different size?
table(sapply(clique, length)) # you could  also use a function from the purr package: table(map_dbl(clique, length)). 

# For strategic queries: 
# Example: to find how many cliques Sydbank is in.
a1 <- keep(clique, function(x) any(x$name == 'Sydbank'))
length(a1)

# How large are the cliques that Sydbank is in?
table(sapply(a1, length)) # or from the purr package table(map_dbl(a1, length))

# Maximum cliques 
# A non-overlapping amount of cliques that are largest, meaning that they cannot be extended by including one more adjacent vertex
max_cliques <- maximal.cliques(net_largest, min=2)
length(max_cliques) # how many?
table(sapply(max_cliques, length)) # how big?


# Further resources -------------------------------------------------------
all_layouts <- c(
  # these are from igraph
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
  'nicely', # automatic choosing of a fitting graph layout
  'bipartite',
  'star',
  'tree',
  # these are from ggraph 
  'dendrogram', 
  'manual', 
  'linear', 
  'matrix', 
  'treemap',  
  'circlepack', 
  'partition',  
  'hive'  
)

# This might be used to highlight communities in graphs
# https://blog.ouseful.info/2021/08/13/fragment-tools-of-production-ggalt/

