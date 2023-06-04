
#IARC Berger-Parker extended for two or more maximum species 

#packages
library('sas7bdat')
library('dplyr')
library('readxl')
library('Hmisc')
library('tidyverse')
library('hillR')

# Read data ---------------------------
iarc<-read.sas7bdat("subj_r.sas7bdat")

############################## Prepare data ############################################
# deleting country 6 and extreme energy intake over energy requirements
iarc<-iarc%>%
  filter(Country!=6)%>%
  filter(Excleier==2)

name<-colnames(iarc) #all column names
kcal_per_species<-name[startsWith(name, "B_KCAL_")] #selecting all column names with species based on energy
gram_per_species<-name[startsWith(name,"B_QTY_")]   #selecting all column names with species based on weight
id<-name[startsWith(name, "Idepic")]                #selecting the column name  with idnames


# preparing energy-based data-----
kcal<-iarc[,kcal_per_species]  #selecting columns with species based on kcal
rownames(kcal)<-iarc[,id]      # id as row names       
kcal_pi<-data.frame(prop.table(x=as.matrix(kcal),margin=1)) #calculating the proportions
rownames(kcal_pi)<-rownames(kcal)
colnames(kcal_pi)<-colnames(kcal)

# preparing weight-based data-----
gram<-iarc[,gram_per_species]  #selecting columns with species weight-based 
rownames(gram)<-iarc[,id]      # id as row names                
gram_pi<-data.frame(prop.table(as.matrix(gram), margin=1))  #calculating the proportions
rownames(gram_pi)<-rownames(gram)
colnames(gram_pi)<-colnames(gram)

################################## Jill_x calculation ##############################

#Note that hillinf2=Jill_2 and so on

#Energy-based: Jill_x calculation for several values of x (inverse of the mean of the x most abundant species)
BP_extra_kcal <- data.frame(
  Hill2 = hill_taxa(kcal_pi, q = 2),
  Hillinf = 1/apply(kcal_pi, 1, max),
  Hillinf2 = 1/apply(kcal_pi, 1, function(x) mean(sort(x, decreasing = TRUE)[1:2])),
  Hillinf3 = 1/apply(kcal_pi, 1, function(x) mean(sort(x, decreasing = TRUE)[1:3])),
  Hillinf4 = 1/apply(kcal_pi, 1, function(x) mean(sort(x, decreasing = TRUE)[1:4])),
  Hillinf5 = 1/apply(kcal_pi, 1, function(x) mean(sort(x, decreasing = TRUE)[1:5]))
)

#Weight-based: Jill_x calculation for several values of x (inverse of the mean of the x most abundant species)
BP_extra_gram <- data.frame(
  Hill2 = hill_taxa(gram_pi, q = 2),
  Hillinf = 1/apply(gram_pi, 1, max),
  Hillinf2 = 1/apply(gram_pi, 1, function(x) mean(sort(x, decreasing = TRUE)[1:2])),
  Hillinf3 = 1/apply(gram_pi, 1, function(x) mean(sort(x, decreasing = TRUE)[1:3])),
  Hillinf4 = 1/apply(gram_pi, 1, function(x) mean(sort(x, decreasing = TRUE)[1:4])),
  Hillinf5 = 1/apply(gram_pi, 1, function(x) mean(sort(x, decreasing = TRUE)[1:5]))
)

#save data
write.csv(BP_extra_kcal, file="data/BP_extra_kcal.csv", row.names = TRUE)
write.csv(BP_extra_gram, file="data/BP_extra_gram.csv", row.names = TRUE)


