##############  #Lektion06 ################
# 
# Virksomhedsstrategi i et netværksperspektiv 
# Loading network graphs differently 
# Extra visualizations
# Working with Orbis data
#
#########################################

## set wd og load libs
setwd('/Users/alexandergamerdinger/Desktop/PhD/teaching/virksomhedsstrategi_forår_2022')

library(data.table)
library(purrr)
library(plyr)
library(igraph)
library(ggraph)
library(graphlayouts)
library(ggforce)
library(readxl)
library(writexl)
# install.packages('RColorBrewer')
library(RColorBrewer) # load det nye library
source('r/lektion05-custom-functions.r') # ekstra funktioner til at subsætte data sæt "den"
library(tidyverse)

# Finding a data sub-set and making a network object ----------------------
# loade den rawdata
den <- read_csv("input/den17-no-nordic-letters.csv")

# subset of corporations that have a valid cvr affiliation
den1 <- den %>% filter(sector %in% "Corporations" & !is.na(cvr_affiliation))


# subset by tags column  --------------------------------------------------
# How to see all the tags. 
show.all.tags(den) # all tags of the raw data set including tags from other sectors 
show.all.tags(den1) # all tags of company boards 

# show affiliations for each tags that can then be used to make a data subset 
bank <- has.tags(den1, "Banks", result = "affil")
finance <- has.tags(den1, "Finance", result = "affil")
pension <- has.tags(den1, "Pensions", result = "affil")

# make a new character vector to merge the tags 
finance_sector <- c(bank, finance, pension)
finance_sector <- unique(finance_sector) #getting rid of all dublicates (those that have finance, bank and/or pension tags)

# subset a data frame from this vector
den2 <- den1 %>% filter(affiliation %in% finance_sector)

# How to find those nodes that are members of two tags?
a1 <- den2 %>% filter(grepl("Farming", tags)) %>% pull(name)
b1 <- den2 %>% filter(grepl("Banks", tags)) %>% pull(name)
c1 <- intersect(a1, b1)

# Make a graph object -----------------------------------------------------
# After you have found a relevant data subset, you are able to load a graph object
# 1. Make an incidence_matrix 
incidence <- xtabs(formula = ~ name + affiliation, data = den2, sparse = TRUE)
# 2. Make a two-mode network 
net <- graph_from_incidence_matrix(incidence, directed = FALSE)
# 3. Make a one-mode network of your choice 
net1 <- bipartite.projection(net, multiplicity = FALSE)
person_network <- net1$proj1
organ_network <- net1$proj2

# Additional ways to load a network graph ---------------------------------
# EXTRA
# You can load a network graph through an adjacency matrix directly
# this example if for your understanding of matrices
example <- tibble(
  
  Person = c("Stine", "Stine", "Mads", "Hanna", "Thomas", "Thomas", "Thomas"), 
  Organization = c("Sydbank", "KMD", "Vestas", "Sydbank", "KMD", "Sydbank","Vestas")
  
)

incidence <- xtabs(formula = ~ Person + Organization, data = example, sparse = TRUE)
incidence

# Load a person-person network graph from an adjacency matrix
adjacency_person <- incidence %*% Matrix::t(incidence) 
adjacency_person

net <- graph_from_adjacency_matrix(adjacency_person, mode = "undirected", weighted = NULL) 
net <- simplify(net, remove.multiple = T, remove.loops = T)
net[]
autograph(net, node_label = name)

# Load a organization-organization network graph from an adjacency matrix
adjacency_organization <- Matrix::t(incidence) %*% incidence 
adjacency_organization

net1 <- graph_from_adjacency_matrix(adjacency_organization, mode = "undirected", weighted = NULL) 
net1 <- simplify(net1, remove.multiple = T, remove.loops = T)
autograph(net1, node_label = name)


# Add graph attributes ----------------------------------------------------
# person-person network: example: add gender
# make a tibble where you make a name and gender column
a1 <- tibble(
  name = V(person_network)$name, 
  gender = den2 %>% count(name, gender) %>% pull(gender)
)

# check if the sequence of names are similar in the tibble and the graph object and then add it
all.equal(V(person_network)$name, a1$name)
V(person_network)$gender <- a1$gender

autograph(person_network, node_colour = gender)

# organization-organization network: example: add sector
# first, let us create a data frame which binds three data frames together into one. We give each node a tag name. 
a1 <- rbind(
  tibble(name=finance, tag ="finance"), 
  tibble(name=bank, tag ="bank"), 
  tibble(name=pension, tag ="pension")
)

# second, let us make an incidence matrix
a2 <- xtabs(formula = ~ name + tag, data = a1, sparse = T)

#let us look at the result
a2

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

# Option 1: naming each manually
# make an empty network attribute
V(organ_network)$firmtype <- NA

# now, manually rename for each firm type
V(organ_network)$firmtype[which(V(organ_network)$name %in% only_bank)] <- "only_bank"
V(organ_network)$firmtype[which(V(organ_network)$name %in% only_finance)] <- "only_finance"
# ... now continue with the rest. All other 5 firm types... 

# [more advanced] Option 2: naming each through a loop
# create a list with all 7 options
list <- c("only_bank", "only_finance", "only_pension", "bank_finance", "bank_pension", "finance_pension", "bank_finance_pension")

# create a loop where you loop over the list
for (i in list) {
  names <- which(V(organ_network)$name %in% get(i)) # use get() to get the values that are within i
  V(organ_network)$firmtype[names] <- i # name each firm type
}

# Look at the result
V(organ_network)$firmtype #make sure that there are no NA values. 


# Components --------------------------------------------------------------
# Before we chose components, let us look at the whole graph object. 
organ_network %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(width=0.4, alpha = 0.4) +
  geom_node_point(size =3, color = "darkblue", alpha = 0.75) +
  theme_graph()

# Select largest component 
complist <- components(organ_network)
net3 <- decompose.graph(organ_network)
tbl <- table(complist$membership) %>% data.table() %>% arrange(desc(N))
tbl$V1 <- as.numeric(tbl$V1)

net_largest <- net3[[tbl$V1[1]]]

# select second largest 
net_second <- net3[[tbl$V1[2]]]

# make a layout that does not change everytime you run it. 
net_largest_layout <- create_layout(net_largest, 'kk')

#plot 
net_largest_layout %>% 
ggraph() + 
  geom_edge_link0(width=.5, alpha=0.4) + 
  geom_node_point(aes(color=firmtype), size=5) + 
  geom_node_label(aes(color=firmtype, label=name), size=3, nudge_y=-0.05, repel=TRUE) + 
  theme_graph() + 
  scale_color_brewer(palette = "Dark2") # you can see the palettes by using this command: display.brewer.all()

#note about color brewer: if there are more unique colors than default numbers of the color code, you need to use the code from lektion04
#mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(amount)

# Explorative question: Are similar firm types more likely to be directly connected with each other?
# let us find out: 
# because firmtype is categorical, we use assortativity_nominal and we must specify the attribute to be as.factor()
assortativity_nominal(net_largest, as.factor(V(net_largest)$firmtype), directed = F)


# Visualizing graphs  -----------------------------------------------------
# I will show you step-by-step, how you can change the asthetics of your graph object. 
# Let us start from scratch

# Make a graph attribute that measures brokerage
V(net_largest)$brokerage <- 1-constraint(net_largest) 
# Make graph layout 
net_largest_layout <- create_layout(net_largest, 'kk')
# Plot the graph
p1 <- net_largest_layout %>% ggraph() +
  geom_edge_link0(width=.5, alpha=0.4) +
  geom_node_point(aes(color=firmtype, size=brokerage)) + 
  theme_graph() 

# if you want to change the size range of nodes, you can use this command
p2 <- p1 + scale_size_continuous(range = c(3,10))
# you can also change the breaks
p1 + scale_size_continuous(range = c(3,10), 
                           breaks = c(0,0.25,0.5,0.75,0.99))

range(V(net_largest)$brokerage)

# changing the size of the firmtype points in the legend
p3 <- p2 + guides(color = guide_legend(override.aes = list(size = 5)))
p3

# change the names on the legend
p4 <- p3 + labs(size='Brokerage', color='Firmtype')
p4

# change legend names and adapt colors
mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(5) #first number represents the max. number of colors in the default palette "Paired". The second number specified at the end in (5) represents the number of colors we need.

p5 <- p4 + scale_color_manual(values= mycolors,
                              breaks=c('only_finance', 'only_pension', 'bank_finance', 'finance_pension', 'bank_finance_pension'),
                              labels=c('Finance', 'Pension', 'Bank & Pension', 'Finance og Pension', 'Bank, Finance\n& Pension'))
p5

# add a title and a subtitle 
p6 <- p5 + labs(title = "Figure 1: Corporate interlocks in the financial industry", 
          subtitle = "Something")
p6


# Colors ------------------------------------------------------------------
# to see all existing colors in R: https://www.stat.ubc.ca/~jenny/STAT545A/r.col.black.bkgd.pdf
# After loading the RColorBrewer package, you can see all existing color shemes here: 
display.brewer.all() 
display.brewer.all(colorblindFriendly=TRUE) 
display.brewer.all(n=5) 

# To retrieve a specific color sheme: 
my_colors <- brewer.pal(8, name = "Set2") 

# apply on a plot 
net_largest_layout %>% ggraph() + 
  geom_edge_link0(width=.5, alpha=0.4) + 
  geom_node_point(aes(color=firmtype, size=brokerage)) + 
  theme_graph() + scale_color_manual(values=my_colors)

# If you need more colors fx. when you want to visualize communities
my_colors1 <- colorRampPalette(my_colors)(11) 
my_colors1

# continuous color shemes
V(net_largest)$closeness <-  closeness(net_largest)
V(net_largest)$betweenness <-  betweenness(net_largest)

net_largest_layout1 <- create_layout(net_largest, 'kk')

p1 <- net_largest_layout1 %>% ggraph() + 
  geom_edge_link0(width=.5, alpha=0.4) + 
  geom_node_point(aes(color=closeness, size=betweenness)) + 
  theme_graph() 
p1 <-  p1 + scale_size_continuous(range=c(3, 15))

# viridis is a good option
p1 + scale_color_viridis() 
# otherwise create it yourself, colors can be chosen by looking at colors()
p1 + scale_color_gradient2(low='white', mid='palevioletred1',  high='darkmagenta', na.value='green') 

# A more complex example
p1 <- net_largest_layout1 %>% ggraph() + 
  geom_edge_link0(width=.5, alpha=0.4) + 
  geom_node_point(aes(fill=firmtype, size=brokerage), shape=21, stroke=0.3, color='black')  + theme_graph() 

p1 + 
  scale_size_continuous(range=c(3, 10)) + 
  guides(fill = guide_legend(override.aes = list(size = 5)),
    size = guide_legend(override.aes = list(fill = 'black'))) + 
  scale_fill_manual(values= mycolors,
                     breaks=c('only_finance', 'only_pension', 'bank_finance', 'finance_pension', 'bank_finance_pension'),
                     labels=c('Finance', 'Pension', 'Bank & Pension', 'Finance og Pension', 'Bank, Finance\n& Pension')) +
  geom_node_text(aes(filter=brokerage<=0.75, label=name), size=2.5, nudge_y=-0.15) +
  geom_node_label(aes(filter=brokerage>0.75, label=name), size=4, nudge_y=-0.15) + 
  labs(title='Figure 2: Corporate interlocks in the financial industry', 
    subtitle='Names with brokerage> 0.75 are highlighted', size='Brokerage', fill='Firm type') 

# save as png or as pdf 
ggsave('output/elitedb-graph-lektion06_01.png', width=30, height=17.5, unit='cm')


# Analysis on centrality measures using ggplot2 ---------------------------
# let us create a data frame with the tibble function

net_metrics <- tibble(
  name = names(degree(net_largest, mode="all")),
  degree =        degree(net_largest, mode="all"),
  betweenness =   betweenness(net_largest, directed=FALSE, weights=NA),
  closeness =     closeness(net_largest, mode="all", weights=NA), 
  eigen =         eigen_centrality(net_largest, directed=FALSE, weights=NA)$vector,
  brokerage= 1-constraint(net_largest) 
)       

#create centrality rankings. 
for(i in c('degree', 'betweenness', 'closeness', 'eigen', 'brokerage')) {
net_metrics <- net_metrics %>% arrange(desc(get(i)))
net_metrics <- net_metrics %>% mutate(!!paste0(i, "_rank") := rleid(get(i)))
}

#create a column with firmtypes 
list1 <- c("only_bank", "only_finance", "only_pension", "bank_finance", "bank_pension", "finance_pension", "bank_finance_pension")

# make an empty column called firmtype 
net_metrics$firmtype <- NA

# loop through list1 and rename NAs with names in list1 by their location. 
for(i in list1) {
  
  net_metrics$firmtype[which(net_metrics$name %in% get(i))] <- i
  
}
  
# What can you visualize: Option 1
net_metrics %>% 
ggplot(aes(brokerage, eigen, color=firmtype, size=closeness)) + 
  geom_point() + 
  scale_color_manual(values=my_colors)

# Option2
net_metrics %>% 
ggplot(aes(degree, fill=firmtype)) + geom_histogram(binwidth=1) + 
  labs(y='amount') + 
  theme_minimal() + 
  facet_wrap(~firmtype) + 
  scale_fill_manual(values=my_colors)


# Orbis data  -------------------------------------------------------------
# You can also make use of data sets from Orbis which has different kinds of data on corporations
# this video here tells you how to download data from the website:
# https://cbs.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=7745539a-08ed-45d6-8da4-ab8b00ceb972

orb <- read_xlsx('input/Orbis-Export 27_03_2020 13_37.xlsx', sheet=2)

# view the raw data set with view(orb1)
# look at the column names (they are a bit messy)
colnames(orb)

# let us select only those we need 
orb1 <- orb %>% 
  select('Company name Latin alphabet', 
         'Operating revenue (Turnover)\nth USD Last avail. yr',
         'Number of employees\nLast avail. yr',
         'DM\nFull name') %>% 
  rename(affiliation = 1, 
         revenue = 2, 
         n_employed = 3, 
         name = 4)

# let us quickly look after missing values (NAs)
orb1 %>% count(affiliation, sort = T)
orb1 %>% count(name, sort = T) #there are many NAs here.

# look at them
orb1 %>% filter(is.na(name)) %>% view()

# Exchange n.a. with NAs 
orb2 <- 
orb1 %>% 
  mutate(revenue = na_if(revenue, "n.a.")) %>% 
  mutate(n_employed = na_if(n_employed, "n.a.")) %>% 
  mutate(affiliation = na_if(affiliation, "n.a.")) %>% 
  mutate(name = na_if(name, "n.a.")) 

# or option 2
#orb2 <- 
#orb1 %>% mutate(across(everything(), ~na_if(., "n.a."))) 

# Now, let us get rid of the NAs 
orb3 <- orb2 %>% 
  filter(!is.na(name) | !is.na(affiliation))


# Make a graph object (as usual) ------------------------------------------
incidence1 <- xtabs(formula = ~ name + affiliation, data = orb3, 
    sparse = TRUE)

# Make adjacency matrix, or load from incidence matrix - and select network. Your choice. 
# Option 1

netorb <- graph_from_incidence_matrix(incidence1, directed = FALSE)
neto <- bipartite.projection(netorb)
netorb1 <- neto$proj2
netorb1 <- simplify(netorb1, remove.multiple=TRUE, remove.loops=TRUE)

# option 2
adj1 <- Matrix::t(incidence1) %*% incidence1 # organizations/affiliations

# load graph object through adjacency matrix directly
netorb1 <- graph_from_adjacency_matrix(adj1, weighted=NULL, mode='undirected') 

# simplify graph
netorb1 <- simplify(netorb1, remove.multiple=TRUE, remove.loops=TRUE)

# visualize
netorb1 %>% 
ggraph(layout='fr') + 
geom_edge_link0(width=.5, alpha=0.4) + 
geom_node_point(size=3, color=my_colors[1], alpha=0.75) + 
theme_graph() 

# now, let us choose the biggest component
complist <- components(netorb1)
comp_net <- decompose.graph(netorb1)
tbl <- table(complist$membership) %>% data.table() %>% arrange(desc(N))
tbl$V1 <- as.numeric(tbl$V1)
net_largest1 <- comp_net[[tbl$V1[1]]]

# visualize with labels 
net_largest1 %>% 
ggraph(layout='stress') + 
geom_edge_link0(width=.5, alpha=0.4) + 
geom_node_point(size=3, color=my_colors[1], alpha=0.75) + 
geom_node_label(aes(label=name), size=1.5)+
theme_graph() 

# probably needs some data cleaning...

# How to make ego networks ------------------------------------------------
# we use the focus layout for creating ego networks 
focus_node <- 'Laan & Spar Bank'
# You need to find the nodes index number, only then the function works
focus_index <- which(V(net_largest)$name == 'Laan & Spar Bank') 

net_largest %>% 
ggraph(layout = "focus", focus = focus_index) +
  geom_edge_link0(colour = "grey66") +
  geom_node_point(shape = 21, size=4) +
  theme_graph() +
  theme(legend.position = "none") +
  coord_fixed() +
  draw_circle(col = "grey40", use = "focus",max.circle = 4) +
  geom_node_text(aes(filter=name==focus_node, label=name), color='black')  + theme_graph()  

# centrality layout based on a node with high centrality score
net_largest %>% 
ggraph(layout = "centrality" , cent=betweenness(net_largest)) +
  geom_edge_link0(colour = "grey66")+
  geom_node_point(shape = 21, size=4) +
  coord_fixed()+
  theme_graph()+
  theme(legend.position = "none") 

# Extra resources ---------------------------------------------------------
# List with all community functions.

all_communities <- c(
'cluster_louvain',
'cluster_edge_betweenness',
'cluster_fast_greedy',
'cluster_label_prop',
'cluster_leading_eigen',
'cluster_walktrap',
'cluster_spinglass',
'cluster_infomap',
'multilevel.community'
)

alle_layouts <- c(
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
