#percentage of NCLASS groups

#packages
library('dplyr')
library('ggplot2')
library('readxl')
library('sas7bdat')

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

############################## Select the NCLASS Groups and calculate the abundance in the diet ############################################

#select NCLASS groups
kcal_per_species_NCLASS<-name[startsWith(name, "B_KCAL_N")]
gram_per_species_NCLASS<-name[startsWith(name,"B_QTY_N")] 

# calculate the abundance per person
kcal_sum_NCLASS<-rowSums(kcal_NCLASS)/rowSums(kcal)
gram_sum_NCLASS<-rowSums(gram_NCLASS)/rowSums(gram)

# calculate the mean
mean_Nclass_kcal<-mean(kcal_sum_NCLASS)
mean_Nclass_gram<-mean(gram_sum_NCLASS)

# plot the frequency of the abundance of NCLASS groups per person in histograms
plotfreqNclasskcal<-ggplot(data.frame(perc=kcal_sum_NCLASS),aes(perc))+geom_histogram(bins=10)+labs(title="energy-based", x="Percentage of NCLASS")
ggsave(filename = file.path("Clean file/plots","plotfreqNclasskcal.png"),plotfreqNclasskcal, width=8, height=7, dpi=300)
plotfreqNclassgram<-ggplot(data.frame(perc=gram_sum_NCLASS),aes(perc))+geom_histogram(bins=10)+labs(title="weight-based", x="Percentage of NCLASS")
ggsave(filename = file.path("Clean file/plots","plotfreqNclassgram.png"),plotfreqNclassgram, width=8, height=7, dpi=300)
