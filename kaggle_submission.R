## CHANGE WORKING DIRECTORY 
## CHANGE MODEL NAME ON LINE 275

##################################################
################################## SETUP #########
##################################################

## libraries
library(dplyr)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(randomForest)
library(ggplot2)
library(GGally)
library(readr)

## set working directory
setwd("C:/Users/Fratoi/Documents/Cours/Costa Rica")

## load data
data = as_data_frame(read.csv("test.csv/test.csv", sep = ",", header = T))
data$Target = 0
data_test = data

## clean data
data = data %>%
  mutate(dependency = 
           ifelse(dependency == "yes", 1,
                  ifelse(dependency == "no", 0, as.numeric(as.character(dependency)))),
         v18q1 = 
           ifelse(v18q == 0,0,v18q1),
         v2a1 = 
           ifelse(tipovivi1==1,0,v2a1),
         v2a1_missing =
           ifelse(is.na(v2a1),"True","False"),
         rez_esc = 
           ifelse((age<7)|(age>19),0,rez_esc),
         rez_esc_missing = 
           ifelse(is.na(rez_esc),"True","False"),
         edjefi = 
           ifelse(edjefe=='yes',1,
                  ifelse(edjefe=='no',0,as.numeric(as.character(edjefe)))) 
         + ifelse(edjefa=='yes',1,
                  ifelse(edjefa=='no',0,as.numeric(as.character(edjefa)))))

## data : original data
## data_temp : original data + features aggregated at the individual granularity
## data_group : features aggregated at the household granularity
## data_ft : data with every features, one line = one household

################### DATA TEMP ####################
data_temp = data %>%
  mutate(parent_family = 
           parentesco1 +
           parentesco2 + 
           parentesco3 + 
           parentesco6 + 
           parentesco7 + 
           parentesco9 + 
           parentesco11,
         parent_inlaw = 
           parentesco4 + 
           parentesco5 + 
           parentesco8 + 
           parentesco10,
         parent_exte = 
           parentesco12,
         wall_score =
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
           eviv3 * 3,
         score_wall_material = 
           paredblolad*6 
         + paredzocalo*4 
         + paredpreb*5 
         + pareddes*1 
         + paredmad*4 
         + paredzinc*4 
         + paredfibras*2 
         + paredother*2,
         score_floor_material = 
           pisomoscer*5
         + pisocemento*4
         + pisoother*4
         + pisonatur*1
         + pisonotiene*1
         + pisomadera*3,
         score_roof_material = 
           techozinc*2
         +techoentrepiso*2
         +techocane*1
         +techootro*4,
         education_score = 
           instlevel1 * 0 +
           instlevel2 * 1 +
           instlevel3 * 2 +
           instlevel4 * 3 +
           instlevel5 * 4 +
           instlevel6 * 5 +
           instlevel7 * 6 +
           instlevel8 * 7 +
           instlevel9 * 8,
         malus = 
           sanitario1 +
           noelec +
           pisonotiene +
           pisonatur +
           techocane +
           pareddes +
           abastaguano +
           cielorazo +
           elimbasu3 +
           energcocinar4 +
           tipovivi4,
         confort = 
           refrig +
           computer + 
           v18q1 + 
           television,
         children =
           ifelse(age < 8, 1, 0),
         student = 
           ifelse(age >= 8 & age < 20, 1, 0), 
         worker = 
           ifelse(age >=20 & age < 65, 1 , 0), 
         old = 
           ifelse(age >= 64, 1, 0),
         urban =
           area1,
         score_elec = 
           noelec*0 + 
           coopele*1 + 
           public*2 + 
           planpri*3,
         phone_per_capita = 
           qmobilephone/tamviv,
         tablet_per_capital = 
           ifelse(is.na(v18q1/tamviv),0,v18q1/tamviv),
         rooms_per_capital = 
           rooms/tamviv,
         rent_per_capital = 
           ifelse(is.na(v2a1/tamviv),0,v2a1/tamviv),
         meaneduc = 
           ifelse(is.na(meaneduc), 0, meaneduc),
         SQBmeaned = 
           ifelse(is.na(meaneduc), 0, meaneduc)
  )


################### DATA GROUP #################### 
## HOUSEHOLD FEATURES ##

data_group = data_temp %>%
  group_by(idhogar) %>%
  summarise(
    nb_parentfamily = 
      sum(parent_family),
    nb_parentinlaw = 
      sum(parent_inlaw),
    nb_parentexte = 
      sum(parent_exte),
    nb_children = 
      sum(children),
    nb_student = 
      sum(student),
    nb_worker = 
      sum(worker),
    nb_old = 
      sum(old),
    sum_escolari = 
      sum(escolari),
    sum_rez_esc = 
      sum(rez_esc, na.rm = T),
    sum_dis = 
      sum(dis, na.rm = T),
    sum_score_educ = 
      sum(education_score, na.rm = T),
    nb_adult_no_educ = 
      sum(ifelse(education_score <= 3, worker,0)),
    nb_adult_mid_educ = 
      sum(ifelse(education_score >= 4 & education_score <= 7, worker, 0)),
    nb_adult_higher_educ =  
      sum(ifelse(education_score >= 8, worker,0)),
    official_union = 
      max(ifelse(estadocivil3 == 1 | estadocivil4 == 1, 1, 0)),
    age_IQR = 
      ifelse(IQR(age) == 0, median(age)/2, IQR(age))
  )

################### MERGE TEMP & GROUP ####################

data_temp = data_temp %>%
  left_join(data_group, by = "idhogar")

################## FINALIZE DATA_TEMP ####################

data_temp = data_temp %>%
  mutate(avg_score_educ = 
           sum_score_educ / hhsize,
         pct_adult_no_educ = 
           nb_adult_no_educ / hhsize,
         pct_adult_mid_educ = 
           nb_adult_mid_educ / hhsize,
         pct_adult_higher_educ = 
           nb_adult_higher_educ / hhsize,
         pct_late = 
           ifelse(is.na(sum_rez_esc/nb_student),0,sum_rez_esc/nb_student),
         lugar_region = 
           ifelse(lugar1 == 1, "Central", 
                  ifelse(lugar2 == 1, "Chorotega",
                         ifelse(lugar3 == 1, "Pacifico Central",
                                ifelse(lugar4 == 1, "Brunca",
                                       ifelse(lugar5 == 1, "Huerta Atlantica","Huerta Norte"))))),
         pct_male = 
           r4h3 / hhsize,
         house_score = 
           floor_score + 
           wall_score +
           roof_score,
         Target = 
           factor(Target))

##################### DATA FT ################
## FINAL FEATURES ON HOUSEHOLDS ##
col_ft = c("Id","idhogar","Target","overcrowding", "edjefi", "hacdor", "rooms","hacapo","v14a","refrig","r4h3","r4m3","hhsize","dependency",
           "meaneduc","bedrooms","qmobilephone","wall_score","roof_score","floor_score","score_wall_material",
           "score_floor_material", "score_roof_material",  "score_elec","phone_per_capita","tablet_per_capital",
           "rooms_per_capital","rent_per_capital","nb_parentfamily","nb_parentinlaw","nb_parentexte","nb_children",
           "nb_student","nb_worker", "nb_old", "sum_dis","nb_adult_no_educ","nb_adult_mid_educ","nb_adult_higher_educ","official_union",
           "age_IQR","avg_score_educ","pct_adult_no_educ", "pct_adult_mid_educ", "pct_adult_higher_educ", "pct_late", "rez_esc_missing",
           "lugar_region", "score_elec", "pct_male", "male", "house_score")
data_ft = data_temp %>%
  filter(parentesco1 == 1) %>%
  select(col_ft) 
names(data_ft)

## recode factors
col_ft_factors = c("hacapo", "refrig", "v14a", "official_union", "hacdor","lugar_region","rez_esc_missing", "male")
data_ft[col_ft_factors] = lapply(data_ft[col_ft_factors], as.factor)

## if you want to remove some columns : fill the -c() argument like -c(idhogar, Id, hacapo)

data_ft = na.omit(select(data_ft, -Target))
data_ft$Target = 0


## after the Boruta feature selection
col_selected = c("overcrowding", "edjefi","hacdor","rooms","refrig","hhsize",
                 "dependency","meaneduc","bedrooms","score_wall_material",
                 "score_floor_material","phone_per_capita","tablet_per_capital","rent_per_capital",
                 "sum_dis","official_union","age_IQR","pct_adult_no_educ","pct_adult_mid_educ",
                 "pct_adult_higher_educ","pct_late","lugar_region","pct_male","male",
                 "house_score","Target","Id", "idhogar")        

data_ft = select(data_ft, col_selected)









################ CHANGE MODEL HERE !!! #############
data_ft$Target = predict(randomForestFit, data_ft)
################ CHANGE MODEL HERE !!! #############





data_test$id = data_test$Id
data_test = left_join(data_test, data_ft, by ="idhogar")
data_test = subset(data_test, select = c(id, Target))
names(data_test) = c("Id", "Target")

data_test$Target = as.integer(data_test$Target)
write_csv(data_test, "submission2.csv")
