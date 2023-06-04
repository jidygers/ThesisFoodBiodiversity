#packages
library('sas7bdat')
library('dplyr')
library('hillR')

# upload data
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
gram<-iarc[,gram_per_species]  #selecting columns with species based on gram
rownames(gram)<-iarc[,id]      # id as row names       
gram_pi<-data.frame(prop.table(as.matrix(gram), margin=1)) #calculating the proportions
rownames(gram_pi)<-rownames(gram)
colnames(gram_pi)<-colnames(gram)

############################### Calculate the indices #####################################

#calculating the energy-based indices
indices_kcal<-data.frame(
  Hill0=hill_taxa(kcal_pi, q=0),
  Hill1=hill_taxa(kcal_pi, q=1),
  Hill2=hill_taxa(kcal_pi, q=2),
  Hillinf=1/apply(kcal_pi, 1, max)
)

#calculating the weight-based indices
indices_gram<-data.frame(
  Hill0=hill_taxa(gram_pi, q=0),
  Hill1=hill_taxa(gram_pi, q=1),
  Hill2=hill_taxa(gram_pi, q=2),
  Hillinf=1/apply(gram_pi, 1, max)
)

#Save data
write.csv(Indices_kcal, file="data/Indices_kcal.csv", row.names = TRUE)
write.csv(Indices_gram, file="data/Indices_gram.csv", row.names = TRUE)
selectdata<-cbind(data.frame(Country=iarc$Country), data.frame(idname=iarc$Idepic_Crypt), data.frame(BMI=iarc$Bmi_C), data.frame(Sex=iarc$Sex), data.frame(Age=iarc$D_Birth))
write.csv(selectdata, file="data/Country_idname_BMI_Sex_Age_per_person.csv", row.names=FALSE)
