#library

library('readxl')
library('tidyverse')
library('Hmisc')
library('ggplot2')
library('viridis')
library('gridExtra')
library('dplyr')
library('GGally')
library('grid')
library('ggplot2')
library('gsubfn')
library('tidyr')
library('sas7bdat')


#read data
iarc<-read.sas7bdat("subj_r.sas7bdat")


############################## Prepare data ############################################

# deleting country 6 and extreme energy intake over energy requirements 
iarc<-iarc%>%
  filter(Country!=6)%>%
  filter(Excleier==2)

name<-colnames(iarc) #all column names
kcal_per_species<-name[startsWith(name, "B_KCAL_")] #selecting all column names with species based on kcal
gram_per_species<-name[startsWith(name,"B_QTY_")]   #selecting all column names with species based on gram
id<-name[startsWith(name, "Idepic")]                #selecting the column name  with idnames

# preparing weight-based data-----
kcal<-iarc[,kcal_per_species]  #selecting columns with species based on kcal
rownames(kcal)<-iarc[,id]      # id as row names                
kcal_pi<-data.frame(prop.table(x=as.matrix(kcal),margin=1)) #calculating the proportions
rownames(kcal_pi)<-rownames(kcal)
colnames(kcal_pi)<-colnames(kcal)

# preparing energy-based data-----
gram<-iarc[,gram_per_species]  #selecting columns with species based on gram
rownames(gram)<-iarc[,id]      # id as row names                
gram_pi<-data.frame(prop.table(as.matrix(gram), margin=1)) #calculating the proportions
rownames(gram_pi)<-rownames(gram)
colnames(gram_pi)<-colnames(gram)

################################# calculation Berger-Parker, maximum abundances and species ##################################################################

# calculate Berger-Parker index, what is the relative maximum amount consumed, and which species (both energy- and weight-based)
BP<-data.frame(
  maxg=apply(gram_pi, 1, max),
  Hillinfg=1/apply(gram_pi, 1, max),
  max_species_gram = apply(gram_pi, 1, function(x) colnames(gram_pi)[which.max(x)]),
  maxk=apply(kcal_pi,1,max),
  Hillinfk=1/apply(kcal_pi, 1, max),
  max_species_kcal = apply(kcal_pi, 1, function(x) colnames(kcal_pi)[which.max(x)])
)

#add absolute values and also add the total amount/ energy consumed
BPmethoev<-data.frame(
  maxg=apply(gram_pi, 1, max),
  sumgabsoluut=apply(gram, 1, sum),
  Hillinfg=1/apply(gram_pi, 1, max),
  max_species_gram = apply(gram_pi, 1, function(x) colnames(gram_pi)[which.max(x)]),
  maxk=apply(kcal_pi,1,max),
  sumkabsoluut=apply(kcal, 1, sum),
  Hillinfk=1/apply(kcal_pi, 1, max),
  max_species_kcal = apply(kcal_pi, 1, function(x) colnames(kcal_pi)[which.max(x)])
)

BP$idname<-rownames(BP)

####################################### Quintiles ###########################################

#dividing into quintiles
q_BP<- data.frame(
  idname=ck$idname,
  Q_Hillinf_gram = as.numeric(cut2(ck$Hillinf_g, g=5)),
  Q_Hillinf_kcal = as.numeric(cut2(ck$Hillinf_k, g=5))
)

#join BP and q_BP
samen<-inner_join(q_BP, BP, by="idname")
samenQ1gram<-samen[samen$Q_Hillinf_gram==1 , c(1,2,4,5)] #select the ones for weight-based calculations based on first quintile
samenQ5gram<-samen[samen$Q_Hillinf_gram==5 , c(1,2,4,5)] #select the ones for weight-based calculations based on fifth quintile
samenQ1kcal<-samen[samen$Q_Hillinf_kcal==1 , c(1,3,6,7)] #select the ones for energy-based calculations based on first quintile
samenQ5kcal<-samen[samen$Q_Hillinf_kcal==5 , c(1,3,6,7)] #select the ones for energy-based calculations based on fifth quintile

#determining the cut-off points of the quintiles
bounds_BP<- data.frame(
  Q_Hillinf_gram = levels(cut2(ck$Hillinf_g, g=5)),
  Q_Hillinf_kcal = levels(cut2(ck$Hillinf_k, g=5))) 
#bounds_BP:
#lower boundary for Q5 in gram? 5.16
#upper boundary for Q1 in gram? 2.86
#lower boundary for Q5 in kcal? 5.46
#upper boundary for Q1 in kcal? 3.39

####################################### Weight-based: From first quintile to the fifth quintile #############################################
#how many species have to change in proportion of the species present in the diet
#so they move from Q1 to Q5

goal<-1/5.16 #maximum amount of the most abundant species to categorise in Q5
df_Q1gram<-data.frame()
for (i in (1:nrow(samenQ1gram))){
  #select diet of a person and order abundance consumed species from high to low
  id<-samenQ1gram[i,1]
  person_pi<-as.vector(t(gram_pi[id,]))
  maxindiet<-sort(person_pi, decreasing = TRUE)
  #number of species larger than goal?
  largerthangoal<-maxindiet[maxindiet>goal]
  numberitems<-length(largerthangoal)
  #how much lowering needed so the items reach at least goal?
  loweringneeded<-sum(largerthangoal-goal)
  absloweringneeded<-loweringneeded*BPmethoev[id,"sumgabsoluut"]
  df_Q1gram<-rbind(df_Q1gram,
                   data.frame(
                     name=id,
                     numberhigher=numberitems,
                     sumloweringneeded=loweringneeded,
                     absloweringneeded=absloweringneeded))
}

####################################### Weight-based: From first quintile to the fifth quintile #############################################
#how many species have to change in proportion of the species present in the diet
#so they move from Q5 to Q1
goal<-1/2.86
df_Q5gram<-data.frame()
for (i in (1:nrow(samenQ5gram))){
  #select diet of a person and order abundance consumed species from high to low
  id<-samenQ5gram[i,1]
  person_pi<-as.vector(t(gram_pi[id,]))
  #how much do the species have to increase in abundance to at least reach the goal
  increasingneeded<-sum(goal-max(person_pi))
  absincreasingneeded<-increasingneeded*BPmethoev[id,"sumgabsoluut"]
  df_Q5gram<-rbind(df_Q5gram,
                   data.frame(
                     name=id,
                     sumincreasingneeded=increasingneeded,
                     absincreasingneeded=absincreasingneeded))
}

####################################### Energy-based: From first quintile to the fifth quintile #############################################
#how many species have to change in proportion of the species present in the diet
#so they move from Q1 to Q5

goal<-1/5.46 #maximum amount of the most abundant species to categorise in Q5
df_Q1kcal<-data.frame()
for (i in (1:nrow(samenQ1kcal))){
  #select diet of a person and order abundance consumed species from high to low
  id<-samenQ1kcal[i,1]
  person_pi<-as.vector(t(kcal_pi[id,]))
  maxindiet<-sort(person_pi, decreasing = TRUE)
  #number of species larger than goal?
  largerthangoal<-maxindiet[maxindiet>goal]
  numberitems<-length(largerthangoal)
  #how much lowering needed so the items reach at least goal?
  loweringneeded<-sum(largerthangoal-goal)
  absloweringneeded<-loweringneeded*BPmethoev[id,"sumgabsoluut"]
  df_Q1kcal<-rbind(df_Q1kcal,
                   data.frame(
                     name=id,
                     numberhigher=numberitems,
                     sumloweringneeded=loweringneeded,
                     absloweringneeded=absloweringneeded))
}

####################################### Energy-based: From fifth quintile to the first quintile #############################################
#how many species have to change in proportion of the species present in the diet
#so they move from Q5 to Q1
goal<-1/3.39
df_Q5kcal<-data.frame()
for (i in (1:nrow(samenQ5kcal))){
  #select diet of a person and order abundance consumed species from high to low
  id<-samenQ5kcal[i,1]
  person_pi<-as.vector(t(kcal_pi[id,]))
  #how much do the species have to increase in abundance to at least reach the goal
  increasingneeded<-sum(goal-max(person_pi))
  absincreasingneeded<-increasingneeded*BPmethoev[id,"sumgabsoluut"]
  df_Q5kcal<-rbind(df_Q5kcal,
                   data.frame(
                     name=id,
                     sumincreasingneeded=increasingneeded,
                     absincreasingneeded=absincreasingneeded))
}

####################################### Weight-based: mean and standard deviations calculated #############################################
mean(df_Q1gram$sumloweringneeded)
sd(df_Q1gram$sumloweringneeded)
mean(df_Q1gram$absloweringneeded)
sd(df_Q1gram$absloweringneeded)
mean(df_Q5gram$sumincreasingneeded)
sd(df_Q5gram$sumincreasingneeded)
mean(df_Q5gram$absincreasingneeded)
sd(df_Q5gram$absincreasingneeded)

####################################### Energy-based: mean and standard deviations calculated #############################################
mean(df_Q1kcal$sumloweringneeded)
sd(df_Q1kcal$sumloweringneeded)
mean(df_Q1kcal$absloweringneeded)
sd(df_Q1kcal$absloweringneeded)
mean(df_Q5kcal$sumincreasingneeded)
sd(df_Q5kcal$sumincreasingneeded)
mean(df_Q5kcal$absincreasingneeded)
sd(df_Q5kcal$absincreasingneeded)

####################################### Results #############################################
# > mean(df_Q1gram$sumloweringneeded)
# [1] 0.2210908
# > sd(df_Q1gram$sumloweringneeded)
# [1] 0.06784419
# > mean(df_Q1gram$absloweringneeded)
# [1] 635.8067
# > sd(df_Q1gram$absloweringneeded)
# [1] 288.6917
# > mean(df_Q5gram$sumincreasingneeded)
# [1] 0.1950195
# > sd(df_Q5gram$sumincreasingneeded)
# [1] 0.02758884
# > mean(df_Q5gram$absincreasingneeded)
# [1] 373.7471
# > sd(df_Q5gram$absincreasingneeded)
# [1] 119.6771
# 
# > mean(df_Q1kcal$sumloweringneeded)
# [1] 0.1756201
# > sd(df_Q1kcal$sumloweringneeded)
# [1] 0.05202827
# > mean(df_Q1kcal$absloweringneeded)
# [1] 448.6401
# > sd(df_Q1kcal$absloweringneeded)
# [1] 191.3874
# > mean(df_Q5kcal$sumincreasingneeded)
# [1] 0.1293943
# > sd(df_Q5kcal$sumincreasingneeded)
# [1] 0.0125471
# > mean(df_Q5kcal$absincreasingneeded)
# [1] 332.5658
# > sd(df_Q5kcal$absincreasingneeded)
# [1] 105.9001

######################################## Which species is most abundant? ########################################################

#read data for the names
xl1<-read_excel("resultaten IARC 1/Contents of the database.xlsx", sheet=2)
xl2<-as.data.frame(xl1[4:nrow(xl1),c(2,7)]) #selection of columns
colnames(xl2)<-c("max_species_gram", "namegram") 

## Weight-based
#shorten the names of labels of the species
speciesgram <- xl2 %>%
  filter(startsWith(max_species_gram,"B_QTY_")) %>%
  mutate(namegram = gsub("Quantity for species:", "", namegram))

# join BP and speciesgram
BP2<-inner_join(speciesgram,BP,by="max_species_gram")

## Energy-based
#shorten the names of labels of the species
colnames(xl2)<-c("max_species_kcal", "namekcal")
specieskcal <- xl2 %>%
  filter(startsWith(max_species_kcal,"B_KCAL_")) %>%
  mutate(namekcal = gsub("Energy for species:", "", namekcal))

# join BP and speciesgram
BP3<-inner_join(specieskcal,BP2,by="max_species_kcal")

##################### END ########################
