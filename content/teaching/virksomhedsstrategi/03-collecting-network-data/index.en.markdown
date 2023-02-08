---
title: "Session 3 - Collecting network data"
author: Alexander Gamerdinger
date: '2023-01-08'
slug: /collecting-network-data/
categories:
  - R
  - Teaching
tags:
description: 'Session on how to collecting network data'
summary: "In this session, you will learn how to collect network data and how to construct network objects from new data sources. You will also have time to collect your own data set."
---

## Session 3 - Collecting network data

Content will be shown soon.


```r
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
```



