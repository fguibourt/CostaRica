##################################################
################################## SETUP #########
##################################################

## libraries
library(dplyr)
library(magrittr)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(randomForest)
library(ggplot2)

## set working directory
setwd("C:/Users/Fratoi/Documents/Cours/Costa Rica")

## load data
data = as_data_frame(read.csv("train.csv/train.csv", sep = ",", header = T))
data$Target = factor(data$Target)

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

## convert to factors
factors = c('hacdor','hacapo','v14a','refrig','v18q','paredblolad','paredzocalo','paredpreb','pareddes','paredmad',
            'paredzinc','paredfibras','paredother','pisomoscer','pisocemento','pisoother','pisonatur','pisonotiene','pisomadera',
            'techozinc','techoentrepiso','techocane','techootro','cielorazo','abastaguadentro','abastaguafuera','abastaguano','public',
            'planpri','noelec','coopele','sanitario1','sanitario2','sanitario3','sanitario5','sanitario6','energcocinar1','energcocinar2',
            'energcocinar3','energcocinar4','elimbasu1','elimbasu2','elimbasu3','elimbasu4','elimbasu5','elimbasu6','epared1','epared2',
            'epared3','etecho1','etecho2','etecho3','eviv1','eviv2','eviv3','dis','male','female','estadocivil1','estadocivil2','estadocivil3',
            'estadocivil4','estadocivil5','estadocivil6','estadocivil7','parentesco1','parentesco2','parentesco3','parentesco4','parentesco5',
            'parentesco6','parentesco7','parentesco8','parentesco9','parentesco10','parentesco11','parentesco12','instlevel1','instlevel2',
            'instlevel3','instlevel4','instlevel5','instlevel6','instlevel7','instlevel8','instlevel9','tipovivi1','tipovivi2','tipovivi3',
            'tipovivi4','tipovivi5','computer','television','mobilephone','lugar1','lugar2','lugar3','lugar4','lugar5','lugar6','area1','area2')

data %<>%
  mutate_each_(funs(factor(.)),factors)

##################################################
################################### EDA ##########
##################################################
attach(data)

####### TARGET #######
table(Target)

data %>%
  ggplot(aes(x = Target, fill = Target))+
  geom_bar()+
  xlab("Target Classes")+ ylab("Target Classes Count")+
  ggtitle("Target Repartition") +
  scale_fill_brewer(palette="BrBG")


######### MARIAGE ########
t = table(estadocivil3)
round(prop.table(t),2)
barplot(t)

## vs. Target
tt = table(estadocivil3, Target)
round(prop.table(tt),2)
round(prop.table(tt,2),2)
barplot(prop.table(tt,2))
chisq.test(tt)

######### DEPENDENCY ########
hist(dependency)
boxplot(dependency)
summary(dependency)
var(dependency)

## vs. Target
boxplot(dependency~Target)
tapply(dependency, Target, summary)
tapply(dependency, Target, sd)

fligner.test(dependency~Target)
summary(aov(formula = dependency~Target))


########### LUGAR ##########
table_lugar = select(data, c(starts_with("lugar")))
table_lugar$region = names(table_lugar)[max.col(table_lugar)]

t = table(table_lugar$region)
round(prop.table(t),2)
barplot(t)

## vs. Target

tt = table(data$Target,table_lugar$region)
tt
round(prop.table(tt),3)
barplot(prop.table(table(data$Target,table_lugar$region),2))

chisq.test(tt)

################# MULTIVARIATE ANALYSIS #############

## FULL CORRPLOT
data_corr1 = data %>%
  select_if(is.numeric)

corr1 = cor(data_corr1, use = "complete.obs")
corrplot(corr1, order = "hclust")

## CLEAN CORRPLOT
data_corr2 = data %>%
  select_if(is.numeric) %>%
  select(1:3,15:17,22:27,37)

corr2 = cor(data_corr2, use = "complete.obs")
corrplot(corr2, order = "hclust")

## ACP
# data_corr1$Target = data$Target
# data_corr2$Target = data$Target

acp1 = PCA(data_corr1, scale.unit = T, ncp = 6, graph = T)
acp2 = PCA(data_corr2, scale.unit = T, ncp = 6, graph = T)

fviz_eig(acp2)
fviz_pca_var(acp2, col.var = "contrib", select.var = list(contrib = 8))

## to be completed


##################################################
########################### DATA PREPARATION #####
##################################################

## load data
data = as_data_frame(read.csv("train.csv/train.csv", sep = ",", header = T))
data$Target = factor(data$Target)

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
## NEW FEATURES ##

# parent_family : individual from the inner family - 0/1
# parent_inlaw : individual from the step family - 0/1
# parent_exte : individual not in the family - 0/1
# children :  aged from 0 to 7 : 0/1
# student : aged from 7 to 19 : 0/1
# worker : aged from 20 to 65 : 0/1
# old : aged from 65 : 0/1
# education level : level of education : 0:8
# edjefi : education level of the family head 0:8
# wall_score : quality of the wall : 1:3
# floor_score : quality of the floor : 1:3
# roof_score : quality of the roof  :1:3
# score_elec : quality of the infrastr : 0:3
# score_wall_material : wall material ranking : 1:6
# score_floor_material : floor material ranking : 1:5
# score_roof_material : floor material ranking : 1:4
# malus : score with precarious attributes : 0:10
# confort : score with electronic appliances : 0:4
# urban : is in urban area : 0/1

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
      IQR(age)
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
         Target = 
           factor(Target))

##################### DATA FT ################
## FINAL FEATURES ON HOUSEHOLDS ##

data_ft = data_temp %>%
  filter(parentesco1 == 1) %>%
  select(c(1,96,143,4:7, 12, 15, 23,101,104,114,124,134:141,
           147:155,160:173,176,178:186)) 
names(data_ft)

## if you want to remove some columns from data_ft : 
data_ft = subset(data_ft, select = -c(SQBescolari, SQBage, SQBovercrowding))
