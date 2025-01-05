################
#
# Uge 7: lektion01-øvelse
# Virksomhedsstrategi i et netværksperspektiv
#
################

# Pakker og working directory ---------------------------------------------
# Load alle pakker vi har brugt for her og setwd() til din working directory

library(igraph) ; library(ggraph) ; library(tidyverse) ; library(data.table)
setwd("")

# Load data ---------------------------------------------------------------

den <- read_csv("den17-no-nordic-letters.csv")

### SPØRGSMÅL 2 ###
# Hvilke styrelser (affiliation) har de fleste medlemmer? -------------------------------------

#tidyverse 
den %>% 
  group_by(affiliation) %>% 
  summarize(N = n()) %>% 
  arrange(desc(N)) 

# Svaret er H.M. Dronningens 75-aars foedselsdag

# Hvor mange kvinder findes der i datasæt? (i %) ----------------------------------

# Option 1
den %>% 
  count(gender, sort=TRUE) %>% 
  mutate(Percent = round(n/sum(n), 2)*100) #the round() function rounds the number to 2 digits. 

# Option 2
den %>% 
  group_by(gender) %>% 
  summarize(n = n()) %>% 
  mutate(Percent = round(n/sum(n), 2)*100)

# Svaret er 28 %

# Hvem sidder i de fleste styrelser? -------------

# Option 1
den %>% 
  count(name, sort = TRUE)

# Option 2
den %>% 
  group_by(name) %>% 
  summarize(N = n()) %>% 
  arrange(desc(N))

# Svaret er Claus Jensen 6395

### SPØRGSMÅL 3 ###
# Lav et nyt datasæt “den1” hvor I kigger kun på aktørene i sektor “Commissions” -------------------------------------------------

den1 <- den %>% filter(sector == "Commissions")

# Er der flere kvinder i dette dataseæt? ----------------------------------

den1 %>% 
  count(gender, sort=TRUE) %>% 
  mutate(Percent = round(n/sum(n), 2)*100)

den1 %>% 
  group_by(gender) %>% 
  summarize(n = n()) %>% 
  mutate(Percent = round(n/sum(n), 2)*100)

# Svaret er ja, der er 38 percent kvinder i "Commissions" subset

### SPØRGSMÅL 4 ###
# Lav et one-mode netværk af individuer og visualisere dette. -------------

#lav graph
incidence <- xtabs(formula = ~name + affiliation, sparse = TRUE, data = den1)

gr <- graph_from_incidence_matrix(incidence, directed = FALSE)

adj_i <- incidence %*% Matrix::t(incidence)

gr1 <- graph_from_adjacency_matrix(adj_i, mode = "undirected")

#visualize graph
gr1 %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(color = "gray70") +
  geom_node_point(size = 1) +
  theme_graph()

### SPØRGSMÅL 5 ###
# Beskriv netværket -------------------------------------------------------









