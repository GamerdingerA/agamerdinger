---
title: Introduction to network analysis in R
author: Alexander Gamerdinger
date: '2023-01-26'
thumbnail: course-picture.jpg
slug: /introduction/
categories:
  - R
  - Teaching
tags:
description: 'Introduction to network analysis'
summary: "In this session, we cover how to install R and RStudio, how to subset and manipulate data sets, and how to visualize simple networks using ggraph and igraph packages."
---

## Session 1 - Introduction to network analysis 

Please find the readings and the materials for this session attached below.

#### Readings

-   [Ellersgaard & Larsen. (2015). The Danish Elite Network](lektion01/Ellersgaard_Larsen_2015.pdf)
-   [Larsen, Ellersgaard & Bernsen (2015). Magteliten: hvordan 423 danskere styrer landet](lektion01/Larsen_Ellersgaard_Bernsen_2015.pdf)
-   [Larsen, Ellersgaard & Steinitz (2016). Magtens Atlas: Et kort over netværk i Danmark](lektion01/Steinitz_Ellersgaard_Larsen_2016.pdf)

#### Materials

-   [data](input/den17-no-nordic-letters.csv)
-   [r-script](lektion01/lektion01-rscript_Alexander.R)

The first thing you need to do is to set a project folder called `virksomhedsstrategi` which you can either place on your desktop, or into a another folder (such as one called `6_semester`).

In this folder, I would like you to create **4 new sub-folders**. Make sure that all of these are spelled with small caps, to make typing easier.

1.  a folder called `input` where you will all the data files (in `csv`, `xlsx`, or other formats)

2.  a folder called `output`where you will save all network figures we will produce

3.  a folder called `r`where you will save all your r-scripts. Make sure to make an r-script for each of the sessions.

After you have created all of the folders, you can open your Rstudio. Create a new r-script, and set your working directory to the file `virksomhedsstrategi`.

For macOs users, click on the folder name, and press `Option + CMD + C`. Paste the path into the function `setwd()`. It should look something similar to this:


```r
setwd("/Users/alexandergamerdinger/Library/CloudStorage/OneDrive-CBS-CopenhagenBusinessSchool/PhD/teaching/virksomhedsstrategi_2023")
```

### 1.1 Installing and loading packages

After you have set your working directory, we will install and load important packages that we will use throughout the course.

1.  The `tidyverse` package, which is basically a collection of a variety of packages that makes it easy and visually pleasing to manipulate and work with data

2.  The `igraph`package, which allows us to construct network objects

3.  The `ggraph`package, which enables us to create beautiful network graphs

4.  The `data.table` package, which we will use occasionally for some data wrangling

How do we install and load packages? Please note that **packages need to only be installed once.** This means that you can simply delete the `install.packages()` function right after its execution.


```r
# install packages (but only run this once)
install.packages("tidyverse")
install.packages("igraph")
install.packages("ggraph")
install.packages("data.table")

# load pckages (run this every time you open/re-open your r-script)
library("tidyverse")
library("igraph")
library("ggraph")
library("data.table")
```



#### 1.1.1 A note on the tidyverse syntax

The `tidyverse` package is great because it has transformed the way of writing R, and has made it much more beautiful. One very important aspect of the package is the use of the pipe `%>%`.

The pipe allows us to write cleaner and more visually pleasing code. Let us take a look at this data set `iris` with the attempt to find the mean petal length and width of all species above a petal width of 0.2.

In `baseR` you would have to write this:


```r
#finding a sub set
subset <- subset(iris, Petal.Width > 0.2)

# selecting only columns on Petals
only_pedal <- subset[,c("Petal.Length", "Petal.Width", "Species")]

# finding mean Petal length and width per species
aggregate(subset[ ,c("Petal.Length", "Petal.Width")],
          by = list(subset$Species), 
          FUN = mean)
```

```
##      Group.1 Petal.Length Petal.Width
## 1     setosa      1.51875       0.375
## 2 versicolor      4.26000       1.326
## 3  virginica      5.55200       2.026
```

In `dplyr` which is a sub-package of `tidyverse`, you would only have to write this:


```r
iris %>% 
  # finding a sub set
  filter(Petal.Width > 0.2) %>% 
  # selecting only three columns
  select(Petal.Length, Petal.Width, Species) %>% 
  #grouping by species
  group_by(Species) %>% 
  # aggregating by mean
  summarize(across(starts_with("Petal"), mean))
```

```
## # A tibble: 3 × 3
##   Species    Petal.Length Petal.Width
##   <fct>             <dbl>       <dbl>
## 1 setosa             1.52       0.375
## 2 versicolor         4.26       1.33 
## 3 virginica          5.55       2.03
```

The pipe operator allows you to simply pass the output of one function as an input to another function that comes afterwards. This makes your code easily readable and efficient.

If you want to know more about the pipe operator, see this website here: <https://uc-r.github.io/pipe>

### 1.2 Loading a data set

After you have set your working directory, and loaded all the required packages, we can now load the data set called `den17-no-nordic-letters.csv`.

Remember to load your data set from your working directory. As your data set is saved in the `input` folder, you have to write this in the command above.


```r
# loading data | If there is an error, 
#it is probably because of your working directory

den <- read_csv("input/den17-no-nordic-letters.csv")
```

```
## Rows: 56849 Columns: 17
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (8): name, affiliation, role, tags, sector, type, description, gender
## dbl  (6): position_id, id, cvr_person, cvr_affiliation, person_id, affiliati...
## dttm (3): created, archived, last_checked
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
# look at the head of the data set
head(den)
```

```
## # A tibble: 6 × 17
##   name             affiliation   role  tags  posit…¹     id sector type  descr…²
##   <chr>            <chr>         <chr> <chr>   <dbl>  <dbl> <chr>  <chr> <chr>  
## 1 Aage Almtoft     Middelfart S… Memb… Corp…       1  95023 Corpo… <NA>  Automa…
## 2 Aage B. Andersen Foreningen O… Memb… Char…       4  67511 NGO    Orga… Direkt…
## 3 Aage Christensen AARHUS SOEMA… Chai… Foun…       6 100903 Found… <NA>  Automa…
## 4 Aage Dam         Brancheforen… Chai… Busi…       8  69156 NGO    Orga… Forman…
## 5 Aage Dam         Dansk Erhver… Memb… Empl…       9  72204 NGO    Stat  Adm. d…
## 6 Aage Frandsen    Dommere valg… Memb… Judg…      15  73158 Parli… <NA>  <NA>   
## # … with 8 more variables: created <dttm>, archived <dttm>,
## #   last_checked <dttm>, cvr_person <dbl>, cvr_affiliation <dbl>,
## #   person_id <dbl>, affiliation_id <dbl>, gender <chr>, and abbreviated
## #   variable names ¹​position_id, ²​description
```

### 1.3 Data manipulation with `dplyr`

#### 1.3.1 Summary statistics

The data set `den17` has 56,849 rows of individuals and 17 columns.


```r
dim(den) # we could also write: den %>% dim()
```

```
## [1] 56849    17
```

We use the following functions in order to gain a better overview of our data set:

1.  the `glimpse()` function to understand the overall structure of the data set

2.  the `count()` function to get summary statistics of one or several variables

3.  the `view()` function to see the whole data set, or a subset, on the big screen.


```r
glimpse(den) # we could also write: den %>% glimpse()
```

```
## Rows: 56,849
## Columns: 17
## $ name            <chr> "Aage Almtoft", "Aage B. Andersen", "Aage Christensen"…
## $ affiliation     <chr> "Middelfart Sparekasse", "Foreningen OEstifterne - Rep…
## $ role            <chr> "Member", "Member", "Chairman", "Chairman", "Member", …
## $ tags            <chr> "Corporation, FINA, Banks, Finance", "Charity, Foundat…
## $ position_id     <dbl> 1, 4, 6, 8, 9, 15, 28, 30, 32, 34, 38, 41, 47, 49, 58,…
## $ id              <dbl> 95023, 67511, 100903, 69156, 72204, 73158, 100249, 316…
## $ sector          <chr> "Corporations", "NGO", "Foundations", "NGO", "NGO", "P…
## $ type            <chr> NA, "Organisation", NA, "Organisation", "Stat", NA, NA…
## $ description     <chr> "Automatisk CVR import at 2016-03-12 18:01:28: BESTYRE…
## $ created         <dttm> 2016-03-12 18:01:28, 2016-02-05 14:45:10, 2016-03-12 …
## $ archived        <dttm> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
## $ last_checked    <dttm> 2017-11-09 15:38:01, 2016-02-12 14:41:09, 2017-11-09 …
## $ cvr_person      <dbl> 4003983591, NA, 4000054465, NA, NA, NA, 4003907021, NA…
## $ cvr_affiliation <dbl> 24744817, NA, 29094411, NA, 43232010, NA, 25952200, NA…
## $ person_id       <dbl> 1, 3, 4, 5, 5, 9, 16, 18, 20, 21, 23, 25, 30, 31, 36, …
## $ affiliation_id  <dbl> 3687, 2528, 237, 469, 1041, 1781, 4878, 1038, 3535, 27…
## $ gender          <chr> "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men"…
```

If we are interested in seeing how many data entries there are per sector, we can write the following.


```r
den %>% # here, we start to use the pipe
  count(sector) # this gives us an unordered summary statistics
```

```
## # A tibble: 13 × 2
##    sector           n
##    <chr>        <int>
##  1 Commissions    795
##  2 Corporations  7989
##  3 Events        1948
##  4 Family         207
##  5 Foundations   6987
##  6 Municipal      320
##  7 NGO          17720
##  8 Organisation     6
##  9 Parliament    1087
## 10 Politics        37
## 11 State        13601
## 12 VL_networks   3803
## 13 <NA>          2349
```

```r
den %>% 
  count(sector, sort = TRUE) # this gives us the same thing, but ordered
```

```
## # A tibble: 13 × 2
##    sector           n
##    <chr>        <int>
##  1 NGO          17720
##  2 State        13601
##  3 Corporations  7989
##  4 Foundations   6987
##  5 VL_networks   3803
##  6 <NA>          2349
##  7 Events        1948
##  8 Parliament    1087
##  9 Commissions    795
## 10 Municipal      320
## 11 Family         207
## 12 Politics        37
## 13 Organisation     6
```

#### 1.3.1 Data subsets

Most of the time, we are interested in looking at subsets of the data set `den`, e.g. to visualize the Danish corporate network, or the ones of political commissions etc.

Here, we make use of the following functions:

1.  we use the `select()` function to select specific columns, keeping all rows constant

2.  we use the `filter()`function to select specific rows, keeping all columns constant

Both of these functions are used to make subsets, that are then assigned to new data objects.


```r
# selecting the name and affiliation of a person
den %>% 
  select(name, affiliation)
```

```
## # A tibble: 56,849 × 2
##    name                 affiliation                                             
##    <chr>                <chr>                                                   
##  1 Aage Almtoft         Middelfart Sparekasse                                   
##  2 Aage B. Andersen     Foreningen OEstifterne - Repraesentantskab (Medlemmer a…
##  3 Aage Christensen     AARHUS SOEMANDSHJEM                                     
##  4 Aage Dam             Brancheforeningen automatik, tryk & transmission (besty…
##  5 Aage Dam             Dansk Erhverv (bestyrelse)                              
##  6 Aage Frandsen        Dommere valgt af Folketinget (Rigsretten)               
##  7 Aage Juhl Joergensen Vestforsyning                                           
##  8 Aage Krogsdam        Danske Rejsejournalister (bestyrelse)                   
##  9 Aage Larsen          Liberalt Oplysnings Forbund (LOF) (Landsstyrelsen)      
## 10 Aage Lauridsen       Halinspektoerforeningen (Bestyrelse)                    
## # … with 56,839 more rows
```


```r
# filtering the data set to only show the sector of corporations
# and assigning it to the object den1

den1 <- 
  den %>% 
  filter(sector == "Corporations")

# look at the object den1 
den1
```

```
## # A tibble: 7,989 × 17
##    name                  affil…¹ role  tags  posit…²     id sector type  descr…³
##    <chr>                 <chr>   <chr> <chr>   <dbl>  <dbl> <chr>  <chr> <chr>  
##  1 Aage Almtoft          Middel… Memb… Corp…       1  95023 Corpo… <NA>  Automa…
##  2 Aage Juhl Joergensen  Vestfo… Memb… Corp…      28 100249 Corpo… <NA>  Automa…
##  3 Adam Christian Harho… IF IT … Exec… Corp…     131  92328 Corpo… <NA>  Automa…
##  4 Adam Troels Bjerg     Ejner … Memb… Corp…     186  87369 Corpo… <NA>  Automa…
##  5 Adam Troels Bjerg     ISS FA… Chai… Corp…     187  92613 Corpo… <NA>  Automa…
##  6 Adine Charlotte Grat… HI3G D… Memb… Corp…     220  91906 Corpo… <NA>  Automa…
##  7 Agnes Marie Therese … Dalum … Chie… Corp…     302  85188 Corpo… <NA>  Automa…
##  8 Agnete Damkjaer Lyng… TRE-FO… Memb… Corp…     321 110225 Corpo… Virk… <NA>   
##  9 Agnete Raaschou-Niel… Broedr… Chai… Corp…     353  84179 Corpo… <NA>  Automa…
## 10 Agnete Raaschou-Niel… Novozy… Vice… Corp…     355 128276 Corpo… <NA>  Automa…
## # … with 7,979 more rows, 8 more variables: created <dttm>, archived <dttm>,
## #   last_checked <dttm>, cvr_person <dbl>, cvr_affiliation <dbl>,
## #   person_id <dbl>, affiliation_id <dbl>, gender <chr>, and abbreviated
## #   variable names ¹​affiliation, ²​position_id, ³​description
```

All of these functions can be combined through the pipe operator. How would you e.g. find out which is the affiliation with the most members in the corporate sector?


```r
den %>% 
  filter(sector == "Corporations") %>% 
  count(affiliation, sort = T)
```

```
## # A tibble: 1,195 × 2
##    affiliation                                        n
##    <chr>                                          <int>
##  1 TRE-FOR (Repraesentantskab)                      119
##  2 Kromann Reumert                                   55
##  3 Bech-Bruun                                        54
##  4 Gorrissen Federspiel                              40
##  5 Plesner                                           40
##  6 EnergiMidt                                        31
##  7 Lett Law Firm                                     27
##  8 Koebenhavns Lufthavns Vaekstkomité (Medlemmer)    24
##  9 Syd Energi (SE)                                   24
## 10 TDC (note)                                        24
## # … with 1,185 more rows
```

### 1.4 Creating matrices
