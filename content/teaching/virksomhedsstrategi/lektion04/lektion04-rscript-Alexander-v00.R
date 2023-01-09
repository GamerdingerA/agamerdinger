
# OBS! DU KAN MÅSKE BRUGE DENNE HER TIL AT FARVELÆGGE GRUPPERNE?
# https://blog.ouseful.info/2021/08/13/fragment-tools-of-production-ggalt/


## load workingdirs
setwd("/Users/alexandergamerdinger/Desktop/PhD/teaching/virksomhedsstrategi_forår_2022")

#######  #subafsnit #######
# installer nye pakker 
# (husk at "comment them out" bagefter)


# ekstra functioner til at lave communities
install.packages('ggforce')  
# funktioner til at arbejde på lister (vi bruger kun en funktion herfra men
# den er vigtig)
install.packages('purrr') 

##### subafsnit slut ######

# libs
library(data.table)
library(purrr)
library(igraph)
library(ggraph)
library(graphlayouts)
library(ggforce)
library(readxl)
library(writexl)
library(tidyverse)


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


# Assortivity -------------------------------------------------------------
# Assortivitx measures the extend to which nodes with similar properties are connected to each other. The assortivity coefficient ranges between -1 and 1, where 1 indicates that there is a high likelihood of two vertices with the same properties being connected. Networks that have an assortivity of 0 are usually refered to as neutral. 

# For categorical assortativity, we use the function assortativity_nominal()
# For continuous assortativity, we use the function assortativity()
# For assortivity based on degree we use the function assortativity_degree. It measures similarity on degree - those that have many friends also hang out with other popular people. 

# Assortativity degree 
assortativity_degree(net, directed=F)
# those that are similar in terms of degree are relatively unlikely to be directly connected

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
gender %>% 
  mutate(across(share, function(x) {ifelse(x == 1 & gender == "Women", 0, x)})) %>% 
  filter(gender == "Women" & share == 0) 


Men <- gender %>% filter(gender == "Men")
Women <- gender %>% filter(gender == "Women" & n == n_total)
Women$share = 0

Men <- Men %>%  select(affiliation, share)
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
assortativity(net_largest, V(net_largest)$share_men, directed=F)
# There does not seem to be a strong correlation between the share of men in board positions and their associaton. There is a weak positive indication for that boards with similar shares of men in their board are connected with each other. 

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
assortativity_nominal(net_largest, as.factor(V(net_largest)$random), directed=F)


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
# In this lesson, we are using the louvain clustering method, the one that is described in the article that was assigned for today - https://agamerdinger.com/teaching/virksomhedsstrategi/lektion04/Blondel_et_al_2008.pdf

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

# Moduality is a measure that tells us the percentage of edges in the network that fall within the given communities compared to what we would expect if the graph was drawn at random.
# Modularity tells us how dense edge connections are within communities, comparing to what we would expect in a random graph
# Hvor god er gruppe indelingen for at rumme edges i mellem grupper, frem for hvis de bare var tilfældig distribueret. 

# Modulariteten fortæller os fraktionen af forbindelser der opstår i mellem grupper, frem for hvis de bare var tilfældig distribueret. 


# Visualizing communities -------------------------------------------------
# Do they have the same sequence of names?
all.equal(louvain1$names, V(net_largest)$name)

# adding membership as an attribute to commnuity (with sprintf() function wich makes membership values look nicer )
V(net_largest)$community <- sprintf("%02d",louvain1$membership) 

# DOES NOT NEED TO BE UNDERSTOOD 
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

# Plotting communities in more distinct group -----------------------------
# We are doing a little trick, namely making a new network object, where we give weights to edges that are in a cluster together. However, we are then just using the layout of the new object, but still plotting the original network. In that way, the positions of nodes are changing. 

# copy the graph object, creating weights and setting them to one. 
net2_tmp <- net_largest
E(net2_tmp)$weight = 1

# A loop that goes through all communities, selects all nodes in each community, and adds an extra weight to them 
for(i in unique(V(net_largest)$community)) {
    net2_vertex = which(V(net_largest)$community == i)
    net2_tmp = add_edges(net2_tmp, combn(net2_vertex, 2), attr=list(weight=0.25))
} 

# Make a layout. Only those layouts that support weights can be used. 
net2_tmp_layout <- create_layout(net2_tmp, 'fr')
net_largest <- create_layout(net_largest, 'fr')

# look at the layout object: 
names(net2_tmp_layout)

# let us copy the coordinates over to our original graph object 
net_largest$x <- net2_tmp_layout$x
net_largest$y <- net2_tmp_layout$y

# Now, let us plot the new version 
net_largest %>% 
ggraph(layout = "fr") + 
geom_edge_link0(aes(filter=community!='9999', color=community), width=0.6, alpha=0.55) + 
geom_edge_link0(aes(filter=community=='9999'), color='black', width=0.6, alpha=0.35) + 
geom_node_point(aes(color=community), alpha=0.95, size=3) + 
theme_graph(base_family = 'Helvetica') +
geom_node_label(aes(filter=name %ilike% 'bank', label=name, color=community), repel=TRUE) +
geom_node_point(aes(filter=name %ilike% 'bank'), color='black', size=1) 


# kan nu goeres lidt bedre med paenere farver
antal <- length(unique(V(net2)$community))
mine_farver <- colorRampPalette(rev(pal_classic))(antal) # en anden mulighed
ggraph(net2_layout) + 
geom_edge_link0(aes(filter=community!='9999', color=community), width=0.6, alpha=0.55) + 
geom_edge_link0(aes(filter=community=='9999'), color='black', width=0.6, alpha=0.35) + 
geom_node_point(aes(color=community), alpha=0.95, size=3) + 
theme_graph(base_family = 'Helvetica') + 
geom_mark_ellipse(data=net2_layout, aes(x=x, y=y, color=community), alpha=0.9) +
scale_edge_color_manual(values=mine_farver) +
scale_color_manual(values=mine_farver) 


#######  #subafsnit #######
# de andre communities funktioner 
# - ikke helt ens lister, man faar tilbage.
# men indeholder alle 'membership' og modularity,
# som er de vigtigste to elementer

# cluster_louvain(net2)
# cluster_edge_betweenness(net2)
# cluster_fast_greedy(net2)
# cluster_label_prop(net2)
# cluster_leading_eigen(net2)
# cluster_walktrap(net2)
# cluster_spinglass(net2)
# cluster_infomap(net2)
# multilevel.community(net2)

#  her er  en liste med kort gennemgang af de forskellige algoritmer:
# https://stackoverflow.com/questions/9471906/what-are-the-differences-between-community-detection-algorithms-in-igraph


##############  #afsnit #################
# kliker  
#########################################

 # Find kliker
klik1 <- cliques(net2, min=2) # kliker med 1 medlem er ikke rigtig en klike, vel...       
length(klik1)

# hvor mange af de forskellige stoerrelser?
# nb! vi bruger sapply, istedet for map_dlb, som i videoen. sapply er base-R, så det holder vi hos til.
table(map_dbl(klik1, length)) 

# spoergsmaal: hvor mange kliker er Sydbank i?
a1 <- keep(klik1, function(x) any(x$name == 'Sydbank'))
length(a1)
# hvor store er Sydbanks kliker?
table(map_dbl(a1, length))

# maximale kliker
maxklik1 <- maximal.cliques(net2, min=2)
length(maxklik1) # hvor mange
table(map_dbl(maxklik1, length)) # hvor store


# vaer opmaerksom paa hvad dokumentationen til igraph siger om kliker, naar der er tale om meget store netvaerk:
# > The computational cost of calculating cliques grows very sharply in size and network density. It is possible that the expected completion time for your calculation may exceed your life expectancy (and those of subsequent generations).

##############  #afsnit #################
# andet - gennemgaas ikke i screencast 
#########################################

# lav metriks
net2_metrics <- data.table(name=names(constraint(net2)), constraint=constraint(net2), degree=degree(net2)) 

# forskellige muligheder for at se et metrik paa, her: degree
qplot(net2_metrics$degree) # fordel: hurtigt overblik. bruges til "lige at kigge"

# Degree distribution, data processing
deg1 <- net2_metrics[,.N,by=.(degree)][order(-N)]

# degree fordeling, log10 transformeret (nemmere at se eksponentielt stigende vaerdier)
# 0 bliver filtreret fra, fordi log10 til 0 er uendelig.
ggplot(deg1, aes(x = degree, y = N)) +
  geom_point(size=5) +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous() 



##############  #afsnit #################
# ressourcer  
#########################################


#######  #subafsnit #######
#  liste med alle layouts

all_layouts <- c(
  # disse her er fra igraph
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
  'nicely', # vaelger automatisk et "godt" layout til grafen
  'bipartite',
  'star',
  'tree',
  # disse her er fra ggraph 
  'dendrogram', 
  'manual', 
  'linear', 
  'matrix', 
  'treemap',  
  'circlepack', 
  'partition',  
  'hive'  
)


