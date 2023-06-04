#Comments

#This is the original script I have sent to IARC to calculate the indices 
#Note that there are easier calculation methods provided in the file "Indices calculation.R" that would result in the same output
#The visualisations made in this script are just for testing the script, the figures in the thesis are made using the script "Indices visualisation v3"

#library reading
library('sas7bdat')
library('dplyr')
library('tidyr')
library('ggplot2')
library('tictoc')

#read data
iarc<-read.sas7bdat("subj_r.sas7bdat")
namen<-colnames(iarc) #all column names

#select data
kcal_per_species<-namen[startsWith(namen, "B_KCAL_")]
gram_per_species<-namen[startsWith(namen,"B_QTY_")]
id<-namen[startsWith(namen, "Idepic")]

# preparing data kcal--------------------------
kcal<-iarc[c(id, kcal_per_species)]

datakcal<-pivot_longer(kcal, cols=(2:ncol(kcal)), names_to="speciesnr", values_to="kcalsum")
colnames(datakcal)[1]<-c("idname")

lang<-datakcal%>%
  pivot_wider(names_from = "idname",values_from = "kcalsum")%>%
  group_by(speciesnr)%>%
  summarise_all(sum, na.rm=TRUE)


#quicker results: test
tic()
rownames(kcal)<-kcal[1:nrow(kcal),1]
lang_df <- as.data.frame(t(kcal[, 2:nrow(kcal)]))
toc()

# !! data screening: did everybody consume something
test<-sort(colSums(lang[2:ncol(lang)])) 
lang<-lang%>%select(-EPC____9FECD54624)
pi<-prop.table(as.matrix(lang[,-1]),2)
pi<-cbind(data.frame(speciesnr=lang$speciesnr),data.frame(pi))

# preparing data gram -------------------------------
gram<-iarc[c(id, gram_per_species)]
datagram<-pivot_longer(gram, cols=(2:ncol(gram)), names_to="speciesnr", values_to="gramsum")
colnames(datagram)[1]<-c("idname")

langgram<-datagram%>%
  pivot_wider(names_from = "idname",values_from = "gramsum")%>%
  group_by(speciesnr)%>%
  summarise_all(sum, na.rm=TRUE)

# !! data screening: did everybody consume something
test<-sort(colSums(langgram[2:ncol(langgram)])) 
langgram<-langgram%>%select(-EPC____9FECD54624)

pigram<-prop.table(as.matrix(langgram[,-1]),2)
pigram<-cbind(data.frame(speciesnr=langgram$speciesnr),data.frame(pigram))

# calculations ------------------------
#DEEL 1 Richness
#DEEL 1.1 Richness in kcal--------------
#1.1.1 determining N and S
S<-data.frame()
N<-data.frame()
for (j in 2:ncol(lang)){
  nonzero<-lang[lang[,j]!=0,j]
  S[1,j-1]<-nrow(nonzero)
  N[1,j-1]<-sum(nonzero)
}
S_kcal<-S
N_kcal<-N

#1.1.2 calculation
uitkomst<-data.frame()
uitkomst<-S                #Richness
uitkomst[2,]<-(S-1)/log(N) #margalef
uitkomst[3,]<-S/log10(N)   #odum
uitkomst[4,]<-S/sqrt(N)    #menhinick
uitkomst[5,]<-N            #Nkcal

#1.1.3 figures
richnesskcal<-pivot_longer(data=uitkomst[1,], cols=everything(), names_to="person", values_to="index")
ggplot(data=richnesskcal, aes(x=index))+geom_bar()+labs(title="Richness kcal")

margalefkcal<-pivot_longer(data=uitkomst[2,], cols=everything(), names_to="person", values_to="index")
ggplot(data=margalefkcal, aes(x=index))+geom_histogram(bins=30)+labs(title="Margalef kcal")

odumkcal<-pivot_longer(data=uitkomst[3,], cols=everything(), names_to="person", values_to="index")
ggplot(data=odumkcal, aes(x=index))+geom_histogram(bins=30)+labs(title="Odum kcal")

menhinickkcal<-pivot_longer(data=uitkomst[4,], cols=everything(), names_to="person", values_to="index")
ggplot(data=menhinickkcal, aes(x=index))+geom_histogram(bins=30)+labs(title="Menhinick kcal")

Nkcal<-pivot_longer(data=uitkomst[5,], cols=everything(), names_to="person", values_to="Nkcal")
ggplot(data=left_join(richnesskcal, Nkcal, by="person"), aes(x=index, y=Nkcal))+geom_point()+labs(title="Total energy intake in function of the number of species consumed")


## DEEL 1.2 Richness in gram--------------------
# 1.2.1 determining N and S
S<-data.frame()
N<-data.frame()
for (j in 2:ncol(langgram)){
  nonzero<-langgram[langgram[,j]!=0,j] #deze zijn niet allemaal even lang...
  S[1,j-1]<-nrow(nonzero)
  N[1,j-1]<-sum(nonzero)
}
S_gram<-S
N_gram<-N

# 1.2.2 calculation
uitkomst<-data.frame()
uitkomst<-S                #Richness
uitkomst[2,]<-(S-1)/log(N) #margalef
uitkomst[3,]<-S/log10(N)   #odum
uitkomst[4,]<-S/sqrt(N)    #menhinick
uitkomst[5,]<-N            #Ngram


# 1.2.4 figures
richnessgram<-pivot_longer(data=uitkomst[1,], cols=everything(), names_to="person", values_to="index")
ggplot(data=richnessgram, aes(x=index))+geom_bar()+labs(title="Richness gram")

margalefgram<-pivot_longer(data=uitkomst[2,], cols=everything(), names_to="person", values_to="index")
ggplot(data=margalefgram, aes(x=index))+geom_histogram(bins=30)+labs(title="Margalef gram")

odumgram<-pivot_longer(data=uitkomst[3,], cols=everything(), names_to="person", values_to="index")
ggplot(data=odumgram, aes(x=index))+geom_histogram(bins=30)+labs(title="Odum gram")

menhinickgram<-pivot_longer(data=uitkomst[4,], cols=everything(), names_to="person", values_to="index")
ggplot(data=menhinickgram, aes(x=index))+geom_histogram(bins=30)+labs(title="Menhinick gram")

Ngram<-pivot_longer(data=uitkomst[5,], cols=everything(), names_to="person", values_to="Ngram")
ggplot(data=left_join(richnessgram, Ngram, by="person"), aes(x=index, y=Ngram))+geom_point()+labs(title="Total quantity in function of number of species")


# DEEL 1.3 compare kcal and gram ------------
figuredata<-left_join(x=richnesskcal, y=richnessgram, by="person", suffix=c(x=".kcal", y=".gram"))
ggplot(figuredata, aes(x=index.kcal, y=index.gram))+geom_point()+labs(title="Richness in gram in function of richness in kcal")

figuredata<-left_join(x=margalefkcal, y=margalefgram, by="person", suffix=c(x=".kcal", y=".gram"))
ggplot(figuredata, aes(x=index.kcal, y=index.gram))+geom_point()+labs(title="Margalef in gram in function of margalef in kcal")

figuredata<-left_join(x=odumkcal, y=odumgram, by="person", suffix=c(x=".kcal", y=".gram"))
ggplot(figuredata, aes(x=index.kcal, y=index.gram))+geom_point()+labs(title="Odum in gram in function of odum in kcal")

figuredata<-left_join(x=menhinickkcal, y=menhinickgram, by="person", suffix=c(x=".kcal", y=".gram"))
ggplot(figuredata, aes(x=index.kcal, y=index.gram))+geom_point()+labs(title="Menhinick in gram in function of menhinick in kcal")

# DEEL 2 EQUITABILITY INDICES--------------------------

# DEEL 2.1 Equitabilityindices in kcal
# 2.1.1 Calculation --------------
uitkomst<-data.frame()
for (j in 2: ncol(pi)){
  nonzero<-pi[pi[,j]!=0,j]
  S<-S_kcal[j-1]
  uitkomst[1,j-1]<- -1*sum(nonzero*log(nonzero)) #Shannon
  uitkomst[2,j-1]<- sum(nonzero*nonzero)   #Simpson
  uitkomst[3,j-1]<- 1-sum(nonzero*nonzero) #Simpsoncomplement
  uitkomst[4,j-1]<- 1/sum(nonzero*nonzero) #Simpsonreciprocal = Hill2
  uitkomst[5,j-1]<- max(nonzero)           #DBergerParker
  uitkomst[6,j-1]<- 1-max(nonzero)         #DBergerParkercomplement
  uitkomst[7,j-1]<- 1/max(nonzero)         #DBergerParkerreciprocal
  uitkomst[8,j-1]<-exp(-1*sum(nonzero*log(nonzero))) #Hill1
}

for (j in 2:ncol(lang)){
  nonzero<-lang[lang[,j]!=0,j]
  S<-S_kcal[j-1]
  N<-N_kcal[j-1]
  U<-sqrt(sum(nonzero*nonzero))
  uitkomst[9,j-1]<- (N-U)/(N-sqrt(N)) #DmcIntosh, weglaten??
}

colnames(uitkomst)<-colnames(lang[,-1])
uitkomstkcal<-uitkomst

# 2.1.2 figures ----------------
shannonkcal<-pivot_longer(data=uitkomst[1,], cols=everything(), names_to="person", values_to="index")
ggplot(data=shannonkcal, aes(x=index))+geom_histogram(bins=30)+labs(title="Shannon kcal")

simpsonkcal<-pivot_longer(data=uitkomst[2,], cols=everything(), names_to="person", values_to="index")
ggplot(data=simpsonkcal, aes(x=index))+geom_histogram(bins=30)+labs(title="Simpson kcal")

simpsoncomplementkcal<-pivot_longer(data=uitkomst[3,], cols=everything(), names_to="person", values_to="index")
ggplot(data=simpsoncomplementkcal, aes(x=index))+geom_histogram(bins=30)+labs(title="Simpsoncomplement kcal")

simpsonreciprocalkcal<-pivot_longer(data=uitkomst[4,], cols=everything(), names_to="person", values_to="index")
ggplot(data=simpsonreciprocalkcal, aes(x=index))+geom_histogram(bins=30)+labs(title="Simpson reciprocal kcal")

DBparkerkcal<-pivot_longer(data=uitkomst[5,], cols=everything(), names_to="person", values_to="index")
ggplot(data=DBparkerkcal, aes(x=index))+geom_histogram(bins=30)+labs(title="Berger-Parker kcal")

Bparkercomplementkcal<-pivot_longer(data=uitkomst[6,], cols=everything(), names_to="person", values_to="index")
ggplot(data=Bparkercomplementkcal, aes(x=index))+geom_histogram(bins=30)+labs(title="Berger-Parker complement kcal")

Bparkerreciprocalkcal<-pivot_longer(data=uitkomst[7,], cols=everything(), names_to="person", values_to="index")
ggplot(data=Bparkerreciprocalkcal, aes(x=index))+geom_histogram(bins=30)+labs(title="Berger Parker reciprocal kcal")

Hill1kcal<-pivot_longer(data=uitkomst[8,], cols=everything(), names_to="person", values_to="index")
ggplot(data=Hill1kcal, aes(x=index))+geom_histogram(bins=30)+labs(title="Hill 1 kcal")

Dmcintoshkcal<-pivot_longer(data=uitkomst[9,], cols=everything(), names_to="person", values_to="index")
ggplot(data=Dmcintoshkcal, aes(x=index))+geom_histogram(bins=30)+labs(title="McIntosh kcal")



# DEEL 2.2 indices in gram-------------------------
# 2.2.1 Calculations ---------------
uitkomst<-data.frame()
for (j in 2:ncol(pigram)){  
  S<-S_gram[j-1]
  nonzero<-pigram[pigram[,j]!=0,j]
  uitkomst[1,j-1]<- -1*sum(nonzero*log(nonzero)) #Shannon
  uitkomst[2,j-1]<- sum(nonzero*nonzero)   #Simpson
  uitkomst[3,j-1]<- 1-sum(nonzero*nonzero) #Simpsoncomplement
  uitkomst[4,j-1]<- 1/sum(nonzero*nonzero) #Simpsonreciprocal = Hill2
  uitkomst[5,j-1]<- max(nonzero)           #DBergerParker
  uitkomst[6,j-1]<- 1-max(nonzero)         #DBergerParkercomplement
  uitkomst[7,j-1]<- 1/max(nonzero)         #DBergerParkerreciprocal
  uitkomst[8,j-1]<-exp(-1*sum(nonzero*log(nonzero))) #Hill1
}

for (j in 2:ncol(langgram)){
  nonzero<-langgram[langgram[,j]!=0,j]
  S<-length(nonzero)
  N<-sum(nonzero)
  U<-sqrt(sum(nonzero*nonzero))
  uitkomst[9,j-1]<- (N-U)/(N-sqrt(N)) #DmcIntosh, weglaten??
}

colnames(uitkomst)<-colnames(lang[,-1])
uitkomstgram<-uitkomst

# 2.2.2 figures
shannongram<-pivot_longer(data=uitkomst[1,], cols=everything(), names_to="person", values_to="index")
ggplot(data=shannongram, aes(x=index))+geom_histogram(bins=30)+labs(title="Shannon gram")

simpsongram<-pivot_longer(data=uitkomst[2,], cols=everything(), names_to="person", values_to="index")
ggplot(data=simpsongram, aes(x=index))+geom_histogram(bins=30)+labs(title="Simpson gram")

simpsoncomplementgram<-pivot_longer(data=uitkomst[3,], cols=everything(), names_to="person", values_to="index")
ggplot(data=simpsoncomplementgram, aes(x=index))+geom_histogram(bins=30)+labs(title="Simpson complement gram")

simpsonreciprocalgram<-pivot_longer(data=uitkomst[4,], cols=everything(), names_to="person", values_to="index")
ggplot(data=simpsonreciprocalgram, aes(x=index))+geom_histogram(bins=30)+labs(title="Simpson reciprocal gram")

DBparkergram<-pivot_longer(data=uitkomst[5,], cols=everything(), names_to="person", values_to="index")
ggplot(data=DBparkergram, aes(x=index))+geom_histogram(bins=30)+labs(title="Berger-Parker gram")

Bparkercomplementgram<-pivot_longer(data=uitkomst[6,], cols=everything(), names_to="person", values_to="index")
ggplot(data=Bparkercomplementgram, aes(x=index))+geom_histogram(bins=30)+labs(title="Berger-Parker complement gram")

Bparkerreciprocalgram<-pivot_longer(data=uitkomst[7,], cols=everything(), names_to="person", values_to="index")
ggplot(data=Bparkerreciprocalgram, aes(x=index))+geom_histogram(bins=30)+labs(title="Berger-Parker reciprocal gram")

Hill1gram<-pivot_longer(data=uitkomst[8,], cols=everything(), names_to="person", values_to="index")
ggplot(data=Hill1gram, aes(x=index))+geom_histogram(bins=30)+labs(title="Hill1 gram")

Dmcintoshgram<-pivot_longer(data=uitkomst[9,], cols=everything(), names_to="person", values_to="index")
ggplot(data=Dmcintoshgram, aes(x=index))+geom_histogram(bins=30)+labs(title="Mc Intosh gram")


# DEEL 2.3 compare kcal and gram ------------
figuredata<-left_join(x=shannonkcal, y=shannongram, by="person", suffix=c(x=".kcal", y=".gram"))
ggplot(figuredata, aes(x=index.kcal, y=index.gram))+geom_point()+labs(title="Shannon in gram in function of Shannon in kcal")

figuredata<-left_join(x=simpsonkcal, y=simpsongram, by="person", suffix=c(x=".kcal", y=".gram"))
ggplot(figuredata, aes(x=index.kcal, y=index.gram))+geom_point()+labs(title="Simpson in gram in function of Simpson in kcal")

figuredata<-left_join(x=simpsoncomplementkcal, y=simpsoncomplementgram, by="person", suffix=c(x=".kcal", y=".gram"))
ggplot(figuredata, aes(x=index.kcal, y=index.gram))+geom_point()+labs(title="Simpsoncomplement in gram in function of Simpsoncomplement in kcal")

figuredata<-left_join(x=simpsonreciprocalkcal, y=simpsonreciprocalgram, by="person", suffix=c(x=".kcal", y=".gram"))
ggplot(figuredata, aes(x=index.kcal, y=index.gram))+geom_point()+labs(title="Simpsonreciprocal in gram in function of Simpsonreciprocal in kcal")

figuredata<-left_join(x=DBparkerkcal, y=DBparkergram, by="person", suffix=c(x=".kcal", y=".gram"))
ggplot(figuredata, aes(x=index.kcal, y=index.gram))+geom_point()+labs(title="DBparker in gram in function of DBparkerkcal in kcal")

figuredata<-left_join(x=Bparkercomplementkcal, y=Bparkercomplementgram, by="person", suffix=c(x=".kcal", y=".gram"))
ggplot(figuredata, aes(x=index.kcal, y=index.gram))+geom_point()+labs(title="Bparkercomplement in gram in function of Bparkercomplement in kcal")

figuredata<-left_join(x=Bparkerreciprocalkcal, y=Bparkerreciprocalgram, by="person", suffix=c(x=".kcal", y=".gram"))
ggplot(figuredata, aes(x=index.kcal, y=index.gram))+geom_point()+labs(title="Bparkerreciprocal in gram in function of Bparkerreciprocal in kcal")

figuredata<-left_join(x=Hill1kcal, y=Hill1gram, by="person", suffix=c(x=".kcal", y=".gram"))
ggplot(figuredata, aes(x=index.kcal, y=index.gram))+geom_point()+labs(title="Hill1 in gram in function of Hill1 in kcal")

figuredata<-left_join(x=Dmcintoshkcal, y=Dmcintoshgram, by="person", suffix=c(x=".kcal", y=".gram"))
ggplot(figuredata, aes(x=index.kcal, y=index.gram))+geom_point()+labs(title="Dmcintosh in gram in function of Dmcintosh in kcal")


# DEEL 3: K-dominance curves ---------------------------------------------------

# DEEL 3.1 K-dominance curves in kcal
geordend<-data.frame(apply(pi[,2:ncol(pi)], 2, sort, decreasing=TRUE))
geordend2<-data.frame(geordend[rowSums((geordend)==0) != ncol(geordend),])
x<-1:nrow(geordend2)
df2<-cbind(x,geordend2)
selectcountry<-cbind(data.frame(country=iarc$Country), data.frame(idname=iarc$Idepic_Crypt))

kdominancecurveskcal<-df2%>%
  pivot_longer(cols=c(2:ncol(df2)), names_to="idname", values_to="value")
inner_join(selectcountry, by="idname")%>%
  group_by(country, x)%>%
  summarise(meanpercountry=mean(value), .groups="keep")

ggplot(plotdata, aes(x=x, y=meanpercountry))+geom_line(aes(color=country))+labs(title='kdominancecurves kcal per country')

#to save the data
selectdata<-cbind(data.frame(Country=iarc$Country), data.frame(idname=iarc$Idepic_Crypt), data.frame(BMI=iarc$Bmi_C), data.frame(Sex=iarc$Sex), data.frame(Age=iarc$D_Birth))

selectdata_classes<-selectdata%>%
  mutate(BMIclass=cut(BMI, breaks=c(1,18.5, 24.9, 29.9, 34.9, 39.9,100)))%>%
  mutate(Sex=as.character(Sex))%>%
  mutate(Date=as.numeric(format(as.Date(Age, origin="1960-01-01"), "%Y")))%>%
  mutate(Dateclass=cut(Date, breaks=c(1900,1920,1940,1960,2000)))

kdominancecurveskcal<-df2%>%
  pivot_longer(cols=c(2:ncol(df2)), names_to="idname", values_to="value")%>%
  inner_join(selectdata_classes, by="idname")

kdominancecurvekcal_country<-kdominancecurveskcal%>%  
  group_by(Country, x)%>%
  summarise(meanpercountry=mean(value), .groups="keep")
ggplot(kdominancecurvekcal_country, aes(x=x, y=meanpercountry))+geom_line(aes(color=country))+labs(title='kdominancecurves kcal per country')

kdominancecurvekcal_BMI<-kdominancecurveskcal%>%
  group_by(BMIclass,x)%>%
  summarise(meanperBMI=mean(value), .groups="keep")
ggplot(kdominancecurvekcal_BMI, aes(x=x, y=meanperBMI))+geom_line(aes(color=BMIclass))+labs(title='kdominancecurves kcal per BMI')

kdominancecurvekcal_Sex<-kdominancecurveskcal%>%
  group_by(Sex,x)%>%
  summarise(meanperSex=mean(value), .groups="keep")
ggplot(kdominancecurvekcal_Sex, aes(x=x, y=meanperSex))+geom_line(aes(color=Sex))+labs(title='kdominancecurves kcal per sex')

kdominancecurvekcal_Birthyear<-kdominancecurveskcal%>%
  group_by(Dateclass,x)%>%
  summarise(meanperBirthyear=mean(value), .groups="keep")
ggplot(kdominancecurvekcal_Sex, aes(x=x, y=meanperBirthyear))+geom_line(aes(color=Sex))+labs(title='kdominancecurves kcal per year')

#DEEL 3.2 K-dominance curves in gram
geordend<-data.frame(apply(pigram[,2:ncol(pigram)], 2, sort, decreasing=TRUE))
geordend2<-data.frame(geordend[rowSums((geordend)==0) != ncol(geordend),])
x<-1:nrow(geordend2)
df2<-cbind(x,geordend2)

plotdata<-df2%>%
  pivot_longer(cols=c(2:ncol(df2)), names_to="idname", values_to="value")%>%
  inner_join(selectcountry, by="idname")%>%
  group_by(country, x)%>%
  summarise(meanpercountry=mean(value), .groups="keep")

ggplot(plotdata, aes(x=x, y=meanpercountry))+geom_line(aes(color=country))+labs(title='kdominancecurves gram per BMI')


#To save the data analysed
write.csv(lang, file="data/kcal_per_person.csv", row.names = FALSE)
write.csv(pi, file="data/kcal_proportion_per_peron.csv", row.names=FALSE)
write.csv(langgram, file="data/gram_per_person.csv", row.names = FALSE)
write.csv(pigram, file="data/gram_proportion_per_person.csv", row.names=FALSE)

Indices_kcal<-data.frame(richnesskcal[,2], margalefkcal[, 2], odumkcal[,2], menhinickkcal[,2], Nkcal[,2],
                         shannonkcal[,2], Hill1kcal[,2], 
                         simpsonkcal[,2], simpsoncomplementkcal[,2], simpsonreciprocalkcal[,2], 
                         DBparkerkcal[,2], Bparkercomplementkcal[,2], Bparkerreciprocalkcal[,2],
                         Dmcintoshkcal[,2])
rownames(Indices_kcal)<-colnames(lang)[2:ncol(lang)]
colnames(Indices_kcal)<-c("Richness_kcal", "Margalef_kcal", "Odum_kcal", "Menhinick_kcal", "Total_kcal",
                          "Shannon_kcal", "Hill1_kcal", 
                          "Simpson_kcal", "Simpson_complement_kcal", "Simpson_reciprocal_kcal",
                          "Berger-Parker_kcal","Berger-Parker_complement_kcal","Berger-Parker_reciprocal_kcal",
                          "McIntosh_kcal")

Indices_gram<-data.frame(richnessgram[,2], margalefgram[, 2], odumgram[,2], menhinickgram[,2], Ngram[,2],
                         shannongram[,2], Hill1gram[,2], 
                         simpsongram[,2], simpsoncomplementgram[,2], simpsonreciprocalgram[,2], 
                         DBparkergram[,2], Bparkercomplementgram[,2], Bparkerreciprocalgram[,2],
                         Dmcintoshgram[,2])
rownames(Indices_gram)<-colnames(lang)[2:ncol(lang)]
colnames(Indices_gram)<-c("Richness_gram", "Margalef_gram", "Odum_gram", "Menhinick_gram", "Total_gram",
                          "Shannon_gram", "Hill1_gram", 
                          "Simpson_gram", "Simpson_complement_gram", "Simpson_reciprocal_gram",
                          "Berger-Parker_gram","Berger-Parker_complement_gram","Berger-Parker_reciprocal_gram",
                          "McIntosh_gram")

write.csv(Indices_kcal, file="data/Indices_kcal.csv", row.names = TRUE)
write.csv(Indices_gram, file="data/Indices_gram.csv", row.names = TRUE)

selectdata<-cbind(data.frame(Country=iarc$Country), data.frame(idname=iarc$Idepic_Crypt), data.frame(BMI=iarc$Bmi_C), data.frame(Sex=iarc$Sex), data.frame(Age=iarc$D_Birth))
write.csv(selectdata, file="data/Country_idname_BMI_Sex_Age_per_person.csv", row.names=FALSE)

selectdata_classes<-selectdata%>%
  mutate(BMIclass=cut(BMI, breaks=c(1,18.5, 24.9, 29.9, 34.9, 39.9,100)))%>%
  mutate(Sex=as.character(Sex))%>%
  mutate(Date=as.numeric(format(as.Date(Age, origin="1960-01-01"), "%Y")))%>%
  mutate(Dateclass=cut(Date, breaks=c(1900,1920,1940,1960,2000)))

write.csv(selectdata_classes, file="data/classes_per_person.csv", row.names=FALSE)
