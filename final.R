#########################
######### SETUP YO #########
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



#########################
## DATA PREPARATION #####
#########################

## recode data

