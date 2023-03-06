##########################
#
#  Øvelse 5: Node centrality and similarity
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

# Let us create a graph using an incidence matrix
# Create the incidence matrix
incidence <- xtabs(formula = ~ name + affiliation, 
                   data = den1, 
                   sparse = TRUE)

# adjacency matrix for chosing corporations
adj_c <- Matrix::t(incidence) %*% incidence

# one-mode graph for corporations
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

# Burt's constraint -------------------------------------------------------
# A measure of brokerage. It a measure on the node level that is higher for those that have less structural opportunities for bridging structural holes within a network. It measures how many direct ties of a node are redundant (connected with themselves). The more nodes are redundant, the more constraint a node is. 

# constraint - the higher, the less chances for brokerage


# make brokerage variable



# Let us make a data.frame with tibble() and look at the different measures. 
metrics <- tibble(
  
  name = names(constraint(comp1)),
  brokerage = 1- constraint(comp1),
  betweenness = betweenness(comp1, directed = FALSE),
  degree = degree(comp1, mode = "all") 
  
)

# Let us see who is least constraint?
metrics %>% arrange(desc(brokerage))

# Visualize Burt's constraint ---------------------------------------------

# Make sure to test if sequence is correct before adding it to a network attribute. 


# Add to our graph object


# how to select the 10 most central people?



# visualize
comp1 %>% 
  ggraph(layout = "mds") +
  geom_edge_link0(color='grey', width=0.6, alpha=0.35) + 
  geom_node_point(aes(size=brokerage), alpha=0.75) + 
  scale_color_viridis() +
  geom_node_label(aes(filter=name %in% ten_largest, label=name), alpha=0.75, repel=T, size=1) +
  geom_node_point(aes(filter=name %in% ten_largest,), color='red', size=.75, alpha=0.75) +
  theme_graph()


# Assortativity -----------------------------------------------------------
# Assortativity measures the extend to which nodes with similar properties are connected to each other. The assortativity coefficient ranges between -1 and 1, where 1 indicates that there is a high likelihood of two vertices with the same properties being connected. Networks that have an assortativity of 0 are usually referred to as neutral. 

# For categorical assortativity, we use the function assortativity_nominal()
# For continuous assortativity, we use the function assortativity()
# For assortativity based on degree we use the function assortativity_degree. It measures similarity on degree - e.g. those that have many friends also hang out with other popular people. 

# Assortativity degree for gr


# Assortativity degree for biggest component


# To calculate similarity between affiliations, we need to create a new variable - gender proportion. 

# What is the gender proportion?


# Let's aggregate gender proportions for each affiliation (but only look at women and men)
gender <- den1 %>% 
  #filter for women and men only
  filter(gender %in% c("Women", "Men")) %>% 
  #count amount of women and men per affiliation
  count(affiliation, gender) %>% 
  # do something, but group by affiliation first
  group_by(affiliation) %>% 
  # generates the total number of board members per affiliation
  mutate(n_total = sum(n), 
         # generates the share of women and men.
         share = n/(sum(n)))   

# Now, let us focus on one share: the share of men in the dataset. 
Men <- gender %>% filter(gender == "Men")
Women <- gender %>% filter(gender == "Women" & n == n_total)
Women$share = 0

Men <- Men %>% select(affiliation, share)
Women <- Women %>% select(affiliation, share)

# create a common object 
gender1 <- rbind(Men, Women)

# now, just choose those affiliations that are part of the biggest component
gender2 <- 
  gender1 %>% 
  filter(affiliation %in% V(comp1)$name)


# check if they are same sequence than in the comp1 object 
all.equal(gender2$affiliation, V(comp1)$name)
index <- match(V(comp1)$name, gender2$affiliation)
gender2 <- gender2 %>% arrange(factor(affiliation, levels = affiliation[index]))
all.equal(gender2$affiliation, names(V(comp1)))

# Let us add gender share as an attribute to the graph 
V(comp1)$share_men <- gender2$share

# Visualize 
comp1 %>% 
  ggraph(layout = "stress") + 
  geom_edge_link0(width=.5, alpha=0.4) + 
  geom_node_point(aes(color=share_men), size=5) + 
  theme_graph() +
  scale_color_gradient2(low='firebrick4', mid='grey80', high='dodgerblue4', midpoint=0.5, na.value='pink') 

# Calculate assortativity for share of men


# Categorical Assortativity ------------------------------------------------
# let us create a categorical gender variable based on gender2. 

gender3 <- 
  gender2 %>%
  # we are creating a new variable "gender_cat" where we use a case_when() statement
  mutate(gender_cat = case_when(
    # when the share col has a share of males below 0.45, it is female dominated
    share < 0.45 ~ "female_dominated", 
    0.45 < share & share < 0.55 ~ "gender_balanced",
    .default = "male_dominated"))

# let us add it to the graph comp1
all.equal(gender3$affiliation, names(V(comp1)))
V(comp1)$gender_cat <- gender3$gender_cat

comp1 %>% 
  ggraph(layout = "fr") + 
  geom_edge_link0(width=.5, alpha=0.4) + 
  geom_node_point(aes(color=gender_cat), size=3) + 
  scale_color_manual(values = c("salmon1", "olivedrab", "steelblue")) + 
  theme_graph() 

# Make sure to write as.factor(), so it is recognized as a categorical variable





# Template to add external variables to EliteDB ---------------------------
# first, you create a data frame that includes the names of the nodes of the network
a1 <- tibble(name = V(comp1)$name)

# Save this as an excel file where you can manually add a variable
write_xlsx(a1, 'output/template_external_variable.xlsx')

# After you have added new variables, save the file, and then load it again.
a2 <- read_xlsx('output/template_external_variable.xlsx')
a2 <- as_tibble(a2)

# check if you have the correct sequence with all.equal() 
# If yes, then add it to the graph object as an attribute. 
V(comp1)$random <- a2$V2

