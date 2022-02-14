################
#
# Uge 7: lektion01-øvelse
# Virksomhedsstrategi i et netværksperspektiv
#
################

# Pakker og working directory ---------------------------------------------
# Load alle pakker vi har brugt for her og setwd() til din working directory

library(igraph) ; library(ggraph) ; library(tidyverse) ; library(data.table)
setwd("/Users/alexandergamerdinger/Desktop/PhD/website/agamerdinger/content/teaching/virksomhedsstrategi")

# Load data ---------------------------------------------------------------

den <- fread("den17-no-nordic-letters.csv")

### SPØRGSMÅL 2 ###
# Hvilke styrelser (affiliation) har de fleste medlemmer? -------------------------------------

# data.table
affil <- den[,.N,.(affiliation)]
affil[order(-N)]

#tidyverse 
den %>% 
  group_by(affiliation) %>% 
  summarize(N = n()) %>% 
  arrange(desc(N)) %>% 
  slice_max(n=1, order_by = N)

# Hvor mange kvinder findes der i datasæt? (i %) ----------------------------------

# data.table
gender <- den[,.N,.(gender)]
gender$Percent <- round(gender$N/sum(gender$N), 2)*100
gender

#tidyverse 
den %>% 
  group_by(gender) %>% 
  summarize(N = n()) %>% 
  mutate(Percent = round(N/sum(N), 2)*100)

# Hvem sidder i de fleste styrelser? -------------

# data.table
name <- den[,.N,.(name)]
name[order(-N)]

#tidyverse 
den %>% 
  group_by(name) %>% 
  summarize(N = n()) %>% 
  arrange(desc(N))

### SPØRGSMÅL 3 ###
# Lav et nyt datasæt “den1” hvor I kigger kun på aktørene i sektor “Parliament” -------------------------------------------------

# data.table
den1 <- den[sector == "Parliament"]

#tidyverse 
den1 <- den %>% filter(sector == "Parliament")

# Er der flere kvinder i dette dataseæt? ----------------------------------

# data.table
gender1 <- den1[,.N,.(gender)]
gender1$Percent <- round(gender1$N/sum(gender1$N), 2)*100
gender1

#tidyverse 
den1 %>% 
  group_by(gender) %>% 
  summarize(N = n()) %>% 
  mutate(Percent = round(N/sum(N), 2)*100)

### SPØRGSMÅL 4 ###
# Lav et one-mode netværk af individuer og visualisere dette. -------------

#lav graph
incidence <- xtabs(~name + affiliation, sparse = T, data = den1)

graph <- graph_from_incidence_matrix(incidence, directed = FALSE)

graph_projection <- bipartite.projection(graph)

ind_graph <- graph_projection$proj1

#visualize graph
ind_graph %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(color = "gray70") +
  geom_node_point(size = 1.5) +
  theme_graph()

### SPØRGSMÅL 5 ###
# Beskriv netværket -------------------------------------------------------

# Hvem er outsider/insider 







