##########################
#
#  Øvelse 1: Introduktion til netværksanalyse 
#
###########################


# SETTING UP --------------------------------------------------------------

#setting the working directory
setwd("")

# Install packages (only execute once) - if there is an error, check troubleshoot panel here https://agamerdinger.com/teaching/virksomhedsstrategi/
install.packages("data.table")
install.packages("tidyverse")
install.packages("ggraph")
install.packages("igraph")

#loading the libraries
library(tidyverse)
library(data.table)
library(ggraph)
library(igraph)

# loading data | If there is an error, it is probably because of your working directory


#select columns name and affiliation


#count affiliation 

# check that this does the same thing than this: den %>% group_by(affiliation) %>% summarize(n = n())

#filter data set den to only include Corporations, then call it den1


# Making a two-mode network --------------------------------------------------------

# Make an incidence matrix, set sparse to TRUE


# View the incidence matrix 


# make an adjacency matrix for individuals


# make an adjacency matrix for corporations



# make a graph object "gr" from the incidence matrix


# make a graph from adjacency, "gr1" for individuals


# make a graph from adjacency,  "gr2" for corporations 


# Visualize the network ---------------------------------------------------

# Visualize the two mode graph



# Make and visualize the one-mode network ---------------------------------

# Visualize the new graph for corporations
