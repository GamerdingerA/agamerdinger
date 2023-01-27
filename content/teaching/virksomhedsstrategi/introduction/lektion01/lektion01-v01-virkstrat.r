

##############  #afsnit #################
# set working directory og lav folder-struktur
#########################################

# set dit working directory her
setwd('/home/emil/Dropbox/Arbejde/CBS/Virksomhedsstrategy i netværksperspektiv/virkstrat-oevelser')


# kontroller at du har sat den rigtige sti 
#- der burde staa navne paa filer og foldere i den projekt-mappe, I har
# oprettet
list.files('')


##############  #afsnit #################
# installer R pakker
#########################################

install.packages('data.table')
install.packages('igraph')
install.packages('ggraph')
install.packages('readxl')
install.packages('writexl')


##############  #afsnit #################
# load pakker
#########################################

library(data.table)
library(igraph)
library(ggraph)
library(readxl)
library(writexl)


##############  #afsnit #################
# netvaerk: et simpelt eksempel 
#########################################

# skab netvaerksobjektet - deltagere fra Paradise Hotel Season 3
# disclaimer: deres relationer her er ikke baseret paa nogen form for reel analyse.
g1 <- graph( c("Caroline", "Boris", "Boris", "Tanja", "Tanja", "Caroline", "Tanja", "Mark", "Tanja", "Jonas", "Jonas", "Simone"), directed=FALSE) 
# print til skaerm
g1 
# saadan ser det ud som adjacency matrix ('sparse' format)
g1[]
# kan laves om til en rigtig matrix
a1 <- as.matrix(g1[])
View(a1)


# visualisering, hurtig
# ikke super informativt, men virker
autograph(g1, layout = "dh") 


# bygget om op som ggplot-objekt: Her laver vi grafen
# lidt flere oplysninger 
ggraph(g1, layout = "dh") + 
geom_edge_link0(color='black') + 
geom_node_point(color='grey',size=15) + 
geom_node_text(aes(label = name), color='black') + 
theme_graph()

# data indlejret i graph objektet
E(g1) # edges / forbindelser
V(g1) # vertices / noder 
V(g1)$name # navne

# man kan adde oplysninger som attributes - de skal vaere sorteret i samme
# raekkefoelge skal vaere den samme som noderns raekkefoelge, dvs i V(g1)$name
V(g1)$koen <- c("Kvinde", "Mand", "Kvinde", "Mand", "Mand", "Kvinde") 

# se hvilke attributter der findes - baade navn og koen
vertex_attr(g1)

# vi kan farvelaegge efter attributter, her koen
ggraph(g1, layout= "dh") + 
geom_edge_link0() +
geom_node_point( aes(color=koen), size=15) + 
geom_node_text( aes(label=name)) + 
theme_graph() + 
scale_color_manual(values=c('indianred4', 'darkseagreen3'))


# en anden udgave  # husk det med blue)
ggraph(g1, layout= "dh") + 
geom_edge_link0(width=2, color='grey25') +
geom_node_point( aes(color=koen), fill='ghostwhite', size=15, shape=21, stroke=2) + 
geom_node_text( aes(label=name)) + 
theme_graph() + 
scale_color_manual(values=c('red3','royalblue1'))



##############  #afsnit #################
# EliteDB - vores datasaet 
# indlaes data, kig lidt paa det
#########################################


#  Christoph & Anton Graus eliteDB datasaet 
# hent "den17-no-nordic-letters.csv" paa Canvas, i mappen til 1. oevelsestime. placer i input-mappen

# laes data ind -  virker kun hvis data er placeret i input-mappen, og hedder det rigtige
den1 <- fread('input/den17-no-nordic-letters.csv')
# herfra 
# https://github.com/antongrau/eliter/tree/master/raw_data
# forskel fra weblink: alle nordiske bogstaver lavet om, samt smaa bogstaver i variabelnavne.


# jargon: jeg refererer til en konkret data.table som en DT fra nu af. 

# data.table syntaks
# DT1[*i*,  *j*,  *by*]
#     |    |        |   
#     |    |        |  
#     |    |        ----->  By  - Grupppering
#     |    -------------->  j   - Hvilke kolonner/variable
#     ------------------->  i   - Hvilke raekker



# subset/udvaelg de foerste 5 raekker, printes til konsollen
den1[1:5, ]
den1[1:5] # uden komma, som i en vector - virker kun paa data.tables, ikke data.frames


# se indhold i DT i en Viewer (vi indlaer kun de foerste 1000)
View(den1[1:1000,])

# subset/udvaelg to kolonner, printes til konsollen
den1[, .(name, tags)] # nem maade 
den1[, list(name, tags)] # ".(   )" er en nem maade at skrive list(  ) paa
b1 <-  c('name', 'tags')
den1[, b1, with=FALSE] # alternativ
den1[, ..b1] # alternativ

# lad os se paa koensfordelingen med special symbolet .N. 

# først: .N er et special symbol i data.table, og staar for "antal raekker i DT"
# hvis .N bruges i *j*, printer den antal raekker i DT
den1[, .N]
# samme som:
nrow(den1)

# hvis det bruges i *i* tager den den sidste raekke, nr 56.536, og printer til konsollen.
den1[.N]

# .N kombineret med *by* : her bliver det interessant: vi kan lave en ny DT
# med antal i den variabel vi grupperer efter (*by*)

den1[, .N,  by=.(gender)] #  det samme
den1[ , .N, .(gender)] # det behoever ikke vaere et named argument
den1[,.N, by='gender'] # det samme (bedre naar man begynder at loope over DTs)

#  kan ogsaa vaere flere  variable
den1[, .N,  by=.(gender, affiliation)]
den1[ , .N, .(gender, affiliation)]
den1[,.N, by=c('gender', 'affiliation')]


# hvem optraeder flest gange? som ny DF med summeret / aggregeret data
a1 <- den1[ , .N , by=.(name)]
a1
# ordn med hoejeste foerst- det goeres med minus foran den variabel man vil sortere efter. 
a1[order(-N)]

a2 <- a1[order(-N), ]
# bemaerk at output ikke assignes til et objekt, fx tilbage til a1. saa a1 er
# stadig sorteret som da det blev aggregereet fra den-objektet.
a2
a1



##############  #afsnit #################
# netvaerk fra eliteDB
#########################################


# lad os lige tjekke hvilke typer forbindelser, der findes, og hvor mange der
# er til hver
a1 <- den1[,.N,by=.(affiliation)][order(-N)]
View(a1[1:100])

# eller sektor
den1[, .N, by=.(sector)][order(-N)]

# vi udvaelger nogle affiliations
den2 <- den1[sector ==  c('Commissions')] 

# ser paa deres naavne
den2[, .N, by=.(affiliation)][order(affiliation)]


# udvaelger nogle bestemte af interesse for at holde det simpelt
den3 <- den2[affiliation %in% c(
  'Mastergruppen for styrkede laereplaner',
  'Moensterbryderkorps',
  'Dialoggruppen for Ny Nordisk Skole',
  'Inklusionseftersynets referencegruppe'
)]
# inspicer data
View(den3)
nrow(den3)

#######  #subafsnit #######
#  lav graf-objekt: two mode netvaerk

# laver incidence matrix
incidence_matrice <- xtabs(formula = ~ name + affiliation, data = den3, 
    sparse = TRUE)
incidence_matrice
# kigge paa den
a1 <- as.matrix(incidence_matrice)
View(a1)

# skab graf objekt
net1 <- graph_from_incidence_matrix(incidence_matrice, directed=FALSE) 

# see paa indhold
net1

# al information paa vertice niveau - typisk lidt for meget
vertex_attr(net1)

# bedre at faa navne paa attributter, og see paa dem een af gangen 
names(vertex_attr(net1)) # vi vil se navnene paa de to
vertex_attr(net1)$type

# two-mode netvaerk
ggraph(net1, layout = 'stress') + 
geom_edge_link0(color='black') + 
geom_node_point(aes(color=type), size=5) + 
geom_node_label(aes(filter=type==TRUE, label = name), color='black', repel=TRUE) + scale_color_manual(values=
  c('darkseagreen3', 'indianred4')
  , labels=c('person','organisation')) + 
theme_graph() + labs(color='')


#######  #subafsnit #######
#  lav graf-objekt: one-mode netvaerk, organisationer

adjacency_matrice <- Matrix::t(incidence_matrice) %*% incidence_matrice
c1 <- as.matrix(adjacency_matrice)
View(c1)

# saa kan vi lave et one-mode netvaerk i stedet (organisation/organisation)
net_org <- graph_from_adjacency_matrix(adjacency_matrice)

autograph(net_org)

#######  #subafsnit #######
#  lav graf-objekt: one-mode netvaerk, individer

# vi vil gerne se paa individernes indbyrdes forbindelser, gennem organisationerne - det goer vi ved at gange matrixen med dens transposede selv. 

adjacency_matrice <- incidence_matrice %*% Matrix::t(incidence_matrice)
b1 <- as.matrix(adjacency_matrice)
View(b1)

# saa kan vi lave et one-mode netvaerk i stedet (individ/individ)
net_pers <- graph_from_adjacency_matrix(adjacency_matrice) # ikke directed, standard setting i funktionen

autograph(net_pers)

