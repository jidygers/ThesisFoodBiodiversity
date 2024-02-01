#Calculation of the Hill numbers in LICM dataset


#packages
library('dplyr')
library('hillR')
library('data.table')
library('tidyr')


# 1) FULL DATA weight-based
data<-as.data.frame(fread('ExtraData/biodiversity data file clean.csv', stringsAsFactors = FALSE))

A_gram<-data%>%
  select(id, country, MAR, speciesnr, foodweight,childadult)%>%
  filter(speciesnr!="")%>%
  filter(childadult=="adult")%>%
  mutate(idname=paste(country,id,sep=""), .keep="all")%>%
  filter(!is.na(MAR))%>%
  group_by(idname, speciesnr, country,MAR)%>%
  summarise(gramsum=sum(foodweight))%>%
  pivot_wider(names_from="speciesnr", values_from="gramsum")

A_gram[is.na(A_gram)]<-0


#calculating the proportion table
A_gram_pi<-data.frame(prop.table(x=as.matrix(A_gram[, c(-1,-2,-3)]),margin=1)) #calculating the proportions

#calculating the indices
A_indices_gram<-data.frame(
  country=A_gram$country,
  MAR=A_gram$MAR,
  idname=A_gram$idname,
  A_Rich_g=hill_taxa(A_gram_pi, q=0),
  A_Hill1_g=hill_taxa(A_gram_pi, q=1),
  A_Hill2_g=hill_taxa(A_gram_pi, q=2),
  A_Hillinf_g=1/apply(A_gram_pi, 1, max)
)

write.csv(A_indices_gram, "clean scripts/A_indices_gram.csv")


# 2) FULL DATA energy-based
data<-as.data.frame(fread('ExtraData/biodiversity data file clean.csv', stringsAsFactors = FALSE))

#hier aan het werken
A_kcal<-data%>%
  select(id, country, MAR, speciesnr, energy_kcal,childadult)%>%
  filter(speciesnr!="")%>%
  filter(childadult=="adult")%>%
  mutate(idname=paste(country,id,sep=""), .keep="all")%>%
  filter(!is.na(MAR))%>%
  group_by(idname, speciesnr, country,MAR)%>%
  summarise(kcalsum=sum(energy_kcal))%>%
  pivot_wider(names_from="speciesnr", values_from="kcalsum")

A_kcal[is.na(A_kcal)]<-0


#calculating the proportion table
A_kcal_pi<-data.frame(prop.table(x=as.matrix(A_kcal[, c(-1,-2,-3)]),margin=1)) #calculating the proportions

#calculating the indices
A_indices_kcal<-data.frame(
  country=A_gram$country,
  MAR=A_gram$MAR,
  idname=A_gram$idname,
  A_Rich_k=hill_taxa(A_kcal_pi, q=0),
  A_Hill1_k=hill_taxa(A_kcal_pi, q=1),
  A_Hill2_k=hill_taxa(A_kcal_pi, q=2),
  A_Hillinf_k=1/apply(A_kcal_pi, 1, max)
)

write.csv(A_indices_kcal, "clean scripts/A_indices_kcal.csv")


