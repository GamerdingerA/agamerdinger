
##############  #section ################
# load libraries and data
#########################################

## working directory
setwd('/Users/alexandergamerdinger/Desktop/PhD/teaching/virksomhedsstrategi_forår_2022')

# load the packages 
library(tidyverse)
library(readxl)

##############  #section ################
# Load Orbis, first look
#########################################

# read excel sheet into R
orb <- read_xlsx('input/Orbis-Export 27_03_2020 13_37.xlsx', sheet=2)

# character vector with variables interest perhaps these are the wrong names,
# because of what's called "encoding" NOTE - if you get an error, check with
# colnames(orb1) to see if the column names are correct. on some computers,
# because of reasons too complicated to state here, but called "encoding
# issues", these names a have sneaky "/r" inserted somewhere.
orb_selected_vars <- c(
	"Company name Latin alphabet",
	"NACE Rev. 2, core code (4 digits)",                  
	"Operating revenue (Turnover)\nth USD Last avail. yr",
	"Number of employees\nLast avail. yr"                ,
	"DM\nFull name"                                      ,
	"DM\nUCI (Unique Contact Identifier)"                ,
	"DM\nJob title (in English)"                         
)

# rename - first create character vector with new names
orb_new_names = c(
  'firm' = 1,
  'sector' = 2,
  'revenue' = 3,
  'n_employed' = 4,
  'name' = 5,
  'id' = 6,
  'title' = 7
)


# select the variables
orb1 <- orb %>% 
  select(all_of(orb_selected_vars)) %>% 
  rename(all_of(orb_new_names)) 

# remove NA values
orb2 <- orb1 %>% 
  filter(!is.na(title), #get rid of NAs
         title %in% c('Member of the Board', 'Chairman', 'Member of the board')) %>% #filter only some positions
  mutate(across(title, ~ str_replace_all(.,"Member of the board", "Member of the Board"))) # replace all of the former with the latter string
  
# fix problem where missing value (Not Answered = NA) is in the wrong format
orb3 <- orb2 %>% 
  mutate(n_employed = na_if(n_employed, "n.a.")) %>% 
  mutate(revenue = na_if(revenue, "n.a.")) %>% 
  mutate(n_employed = as.numeric(n_employed))

# fix problem with people appearing more than once
# only showing disinct id's per firm. 
orb3 <- orb3 %>% 
  select(firm, title, everything()) %>% 
  distinct(firm, id, .keep_all = TRUE)

# Looking at revenue per firm 
firm <- orb3 %>% 
  select(firm, revenue, sector, n_employed) %>% # select only the relevant cols
  mutate(revenue = as.numeric(revenue)) %>% # change revenue to numeric 
  distinct(firm, .keep_all = TRUE) # get unique firm name

# Calculating percentage of men and women in board 
orb4 <- orb3 %>%
  mutate(gender = case_when(
    # case_when is kind of like if else statements but you can use more than one.
    grepl("^mr", name, ignore.case = TRUE) ~ "male",
    grepl("^ms", name, ignore.case = TRUE) ~ "female")) %>% 
    # now filter out those that have not been recognized. 
  filter(!is.na(gender))
  
gender_tbl <- orb4 %>% 
  count(firm, gender) %>% 
  group_by(firm) %>% 
  mutate(share_gender = n/sum(n)) %>% 
  filter(gender == "male")  %>% # check also if there are only female boards. Here, there are non, but if there are for your subset, make sure to follow the same procedure as in lektion04.
  select(-gender)

# merge the two data.tables
firm1 <- merge(firm, gender_tbl, by='firm', all.x=TRUE)

firm1 <- as_tibble(firm1) %>% 
  rename(n_male_board = n)

