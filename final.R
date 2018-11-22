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

######### CLEANING #######

# Suppression des colonnes tam_hog , hogar_total , et les Squares

data_clean <- data[,c(1:7,9:18,20:99,101:133,143)]

##Fill the NA

#Fill the empty 'v18q1'
data = mutate(data,v18q1 = ifelse(v18q==0,0,v18q1))

#Fill the empty 'v2a1'
data = mutate(data,v2a1 = ifelse(tipovivi1==1,0,v2a1))

#Creating a 'v2a1_missing' column indicating there are v2a1 missing info
data = mutate(data,v2a1_missing = ifelse(is.na(v2a1),"True","False"))

#Fill the empty 'rez_esc'
data = mutate(data,rez_esc = ifelse((age<7)|(age>19),0,rez_esc))

#Creating a 'rez_esc_missing' column indicating there are rez_esc missing info
data = mutate(data,rez_esc_missing = ifelse(is.na(rez_esc),"True","False"))


## clean dependency 
 data = data %>%
   mutate(dependency = 
            ifelse(dependency == "yes", 1,
                   ifelse(dependency == "no", 0, as.numeric(as.character(dependency)))))

#clean edjefe/jefa + fusion

data = data %>%
  mutate(edjefe=
           ifelse(edjefe=='yes',1,
                  ifelse(edjefe=='no',0,as.numeric(as.character(edjefe)))))

data = data %>%
  mutate(edjefa=
           ifelse(edjefa=='yes',1,
                  ifelse(edjefa=='no',0,as.numeric(as.character(edjefa)))))

data['edjefi'] = edjefi


######## RECODING ########

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

## Score electricity ###

data = data %>%
  mutate(score_elec = noelec*0 + coopele*1 + public*2 + planpri*3)

## score malus  
# o	'Sanitorio1' qui signifie qu'il n'y a pas de sanitaires dans la maison  : +1
# o	'elec' = 0 qui signifie qu'il n'y pas d'électricité : +1
# o	'pisonotiene' = 0 qui signifie qu'il n'y pas de sol dans la maison : +1
# o 'pisonatur'= 1 qui signifie un sol avec des fibres naturelles
# o 'techocane' = 1 signifie un toit pourri
# o 'pareddes' = 1 signifie des murs de déchets
# o	'abastaguano' =0 qui signifie qu'il n'y pas l'eau courante : +1
# o	'cielorazo' = 0  qui signifie que la maison n'a pas de plafond : +1

data  = data %>%
  mutate(malus = 
           sanitario1 +
           noelec +
           pisonotiene +
           pisonatur +
           techocane +
           pareddes +
           abastaguano +
           cielorazo)

## score bonus confort :
# o	'refrig qui signifie que le foyer a une réfrégirateur : +1
# o	'computer' qui signifie que le foyer a un ordinateur : +1
# o	'v18q1' >0 qui signifie que le foyer a au moins une tablette : +1
# o	'television' qui signifie que le foyer a une television : +1

data = data %>% 
  mutate(confort = 
           refrig +
           computer + 
           v18q1 + 
           television)



## recode age in 4 categories : 
 data = data %>%
   mutate(children =
            ifelse(age < 8, 1, 0),
          student = 
            ifelse(age >= 8 & age < 20, 1, 0), 
          worker = 
            ifelse(age >=20 & age < 65, 1 , 0), 
          old = 
            ifelse(age >= 64, 1, 0))

data_r = data %>%
   group_by(idhogar) %>%
   summarise( nb_children = sum(children),
              nb_student = sum(student),
              nb_worker = sum(worker),
              nb_old = sum(old))
 
data = left_join(data, data_r, by = "idhogar")


## escolari recode : 
data_r = data %>%
  group_by(idhogar) %>%
  summarise(sum_escolari = sum(escolari))

data = left_join(data, data_r, by = "idhogar")

data = data %>% 
  mutate(moy_escolari = 
           sum_escolari / (nb_student+nb_worker+nb_old))


## recode rez_esc in pct_late
data_r = data %>%
  group_by(idhogar) %>%
  summarise(sum_rez_esc = sum(rez_esc, na.rm = T))

data = left_join(data, data_r, by = "idhogar")

data = data %>%
  mutate(pct_late = sum_rez_esc /(nb_student) )

## recode disable (individual) in nb_disable (foyer) 
data_r = data %>%
  group_by(idhogar) %>%
  summarise(sum_dis = sum(dis, na.rm = T))

data = left_join(data, data_r, by = "idhogar")

## recode education_score into 4 categories : avg_score_educ, pct_adult_no_educ, pct_adult_top_educ
data_r = data %>%
  group_by(idhogar) %>%
  summarise(sum_score_educ = sum(education_score, na.rm = T),
            nb_adult_no_educ = sum(ifelse(education_score <= 3, worker,0)),
            nb_adult_mid_educ = sum(ifelse(education_score >= 4 & education_score <= 7, worker, 0)),
            nb_adult_higher_educ =  sum(ifelse(education_score >= 8, worker,0)))

data = left_join(data, data_r, by = "idhogar")

data = data %>%
  mutate(avg_score_educ = sum_score_educ / hhsize,
         pct_adult_no_educ = nb_adult_no_educ / hhsize,
         pct_adult_mid_educ = nb_adult_mid_educ / hhsize,
         pct_adult_higher_educ = nb_adult_higher_educ / hhsize)
  

 ## recode estadocivil in official_union = 1 if married or divorced, 0 else
data_r = data %>%
  group_by(idhogar) %>%
  summarise(official_union = max(ifelse(estadocivil3 == 1 | estadocivil4 == 1, 1, 0)))

data = left_join(data, data_r, by = "idhogar")





##########################
## FEATURE SELECTION #####
##########################

# STEP 0 : SELECT CLEAN DATASET FOR TEST
columns = c("Target", "rooms", "bedrooms", "overcrowding", "malus", "confort", "refrig",
            "moy_escolari", "pct_adult_mid_educ","pct_adult_higher_educ","official_union",
            "roof_score", "wall_score", "nb_children","nb_student","nb_worker", "nb_old",
            "edjefa", "edjefe")

data_test = data[parentesco1 == 1 , columns]
data_test$Target = factor(data_test$Target)
data_test = na.omit(data_test)

# STEP 1: RANK THE VARIABLES BY IMPORTANCE
library(randomForest)

importance = function(S,dataset){
  
  set.seed(0)
  
  #START OF THE LOOP
  for (loop in c(1:S) ){ 
    
    #Random draw with replacement
    draw <- dataset[sample(1:nrow(dataset), nrow(dataset), replace=TRUE), ]
    
    #Importance of each covariate
    rf <- randomForest(Target ~ ., data = draw)
    imp <- rf$importance
    
    #Storing the importance for each draw
    if(loop==1){
      imp_all <- data.frame(imp)
    }
    else{
      imp_all <- cbind(imp_all,data.frame(imp))
    }
    
    #END OF THE LOOP
  }
  
  #Calculate the average importance by variable and the rank
  imp_average <- rowMeans(imp_all,na.rm=TRUE)
  imp_rank <- order(imp_average,decreasing = TRUE)
  
  #RETURN THE RESULTS (the average importance, the rank based of the average importance, and importance by variable for each draw can be seen in output)
  return(list(imp_average=imp_average,imp_rank=imp_rank,imp_all=imp_all))
  
}

#Find the rank of the covariates according to the importance calculated from 50 random draws
imp <- importance(50,data_test)
imp$imp_average
