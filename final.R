#########################
######### SETUP #########
#########################

## libraries
library(ggplot2)
library(dplyr)

## set working directory (change it for your computer)
setwd("C:/Users/Fratoi/Documents/Cours/Costa Rica")

## load data
data = read.csv("train.csv/train.csv", sep = ",", header = T)
data = data.frame(data)

#########################
########## EDA ##########
#########################

## to be completed

#########################
## DATA PREPARATION #####
#########################

## recode data
  ## parentesco1-12
data_r = data %>%
  mutate(parentfamily = 
           parentesco1 +
           parentesco2 + 
           parentesco3 + 
           parentesco6 + 
           parentesco7 + 
           parentesco9 + 
           parentesco11,
         parentinlaw = 
           parentesco4 + 
           parentesco5 + 
           parentesco8 + 
           parentesco10,
         parentexte = 
           parentesco12) %>%
  group_by(idhogar) %>%
  summarise(nb_parentfamily = sum(parentfamily),
            nb_parentinlaw = sum(parentinlaw),
            nb_parentexte = sum(parentexte))
  ## same as in SQL : LEFT JOIN t1 ON t1.id = t2.id
  data = left_join(data, data_r, by = "idhogar")
  ## remove unused columns
  ## data = data %>% select(-c(starts_with("parentesco")))


  ## build score : wall / roof /floor
data = data %>%
  mutate(wall_score =
           epared1 * 1 +
           epared2 * 2 +
           epared3 * 3,
         roof_score = 
           etecho1 * 1 +
           etecho2 * 2 +
           etecho3 * 3,
         floor_score = 
           eviv1 * 1 +
           eviv2 * 2 + 
           eviv3 * 3)

  ## instlevel recode 
data = data %>%
  mutate(education_score = 
           instlevel1 * 0 +
           instlevel2 * 1 +
           instlevel3 * 2 +
           instlevel4 * 3 +
           instlevel5 * 4 +
           instlevel6 * 5 +
           instlevel7 * 6 +
           instlevel8 * 7 +
           instlevel9 * 8)

## Score_material ##

data = data %>%
  mutate(score_wall_material = paredblolad*6 
                            + paredzocalo*4 
                            + paredpreb*5 
                            + pareddes*1 
                            + paredmad*4 
                            + paredzinc*4 
                            + paredfibras*2 
                            + paredother*2,
         score_floor_material = pisomoscer*5
                            + pisocemento*4
                            + pisoother*4
                            + pisonatur*1
                            + pisonotiene*1
                            + pisomadera*3,
         score_roof_material = techozinc*2
                            +techoentrepiso*2
                            +techocane*1
                            +techootro*4)
