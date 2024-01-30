#library
library('data.table')
library('readxl')
library('tidyverse')
library('Hmisc')
library('ggplot2')
library('viridis')
library('gridExtra')
library('dplyr')
library('GGally')
library('grid')
library('cowplot')
library('gsubfn')
library('sas7bdat')
############################ Data preparation ################################################
#read data
#ik<-read.csv("scripts/resultaten IARC 1/Indices_kcal.csv")[,c(2,6,8,11,14)] #full data 
ik2<-fread("scripts/resultaten IARC 1/Indices_kcal.csv")[,c(1,2,6,8,11,14)] #full data 
#ig<-read.csv("scripts/resultaten IARC 1/Indices_gram.csv")[,c(2,6,8,11,14)] #full data
ig2<-fread("scripts/resultaten IARC 1/Indices_gram.csv")[,c(1,2,6,8,11,14)] #full data
eigb<-fread("scripts/resultaten IARC 1/Country_idname_BMI_Sex_Age_per_person.csv") #full data
eig2<-fread("scripts/resultaten IARC 1/Age_Date_Recruitment.csv")[, c(1,3)] #full data
eig3<-fread("scripts/resultaten IARC 1/Country_idname_FBDIARC_Excleier_per_person.csv")[, c(2,4)] #full data
colnames(eig3)[1]<-"Idepic_Crypt"

#combine data
list_df<- list(ig2, ik2, eigb, eig2,eig3)
combi<- list_df %>% reduce(inner_join,  by="Idepic_Crypt")

# deleting country 6 and extreme energy intake over energy requirements and put BMI, Sex, Age and Age_recr  in classes
combi<-combi%>%
  filter(Excleier==2)%>% # delete extremes of energy intake/energy needs
  filter(Country!="6")%>% # delete Greece due to administrative constraints
  mutate(BMIclass=cut(BMI, breaks=c(1,18.5, 24.9, 29.9,100), labels = c("<18.5", "18.5-24.9", ">25", ">30")))%>%
  mutate(Sexclass=as.character(Sex))%>%
  mutate(Age_now=format(as.Date(Age, format = "%d/%m/%Y"),"%Y"))%>%
  mutate(Agerecr_class=cut(Age_Recr, breaks=c(1,40,50,60,100), labels = c("<40", "40-50", "50-60", ">60")))

#view data
summary(combi)

#shorten the names of the data
ck<-combi
colnames(ck)<-c("idname",
                "Rich_g","Tot_g", "Hill1_g", "Hill2_g", "Hillinf_g", 
                "Rich_k","Tot_k", "Hill1_k", "Hill2_k", "Hillinf_k",
                "Country","BMI", "Sex","Age","Age_Recr","Excleier", "BMIc","Sexc","Age_now", "Age_Recr_c")

write.csv(ck, file="scripts/ck.csv")

###########################################################################
ck<-as.data.frame(fread("scripts/ck.csv"))

############################# Figure 1: ggpairs weight ########################

plotdata<-ck[, c("Rich_g", "Hill1_g", "Hill2_g", "Hillinf_g")] #select data

#write function to create the graphs in lower part
my_ggally <- function(data, mapping){
  ggplot(data,mapping) +
    geom_bin2d(bins=150)+
    scale_fill_gradientn(
      colours= c("#F4EBE1", "#D2B7B1","#009238","#003916", "black"),
      values= c(0,0.035,0.17,0.38,0.6,1),     
      limits=c(0,3000))+
    theme_bw()}
#"#D2B7B1"

my_diag <-function(data, mapping){
  ggplot(data,mapping) +
    geom_density(color="#003916")+
    theme(panel.background = element_rect(fill = "#F8F3F2"))
}


# plot the data
plotwithoutlegend<-ggpairs(
  plotdata[],
  aes(),
  lower = "blank",
  upper = list(continuous = my_ggally),
  diag = list(continuous = my_diag),
  columnLabels = c('Hill["0"]', 'Hill["1"]', 'Hill["2"]', 'Hill["\U221E"]'),
  labeller = label_parsed
) 

plotwithoutlegend<-plotwithoutlegend+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(panel.border = element_blank())+ 
  theme(axis.text= element_blank(), axis.ticks = element_blank(), strip.background = element_rect(fill = "#003916"), strip.text=element_text(color="white"))
#theme(strip.text.x = element_text(size = 14), strip.text.y = element_text(size = 14)) #remove borders and grid

plotwithoutlegend[1,2]<-plotwithoutlegend[1,2]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.22   ")), size = 2.7, color = "black")
plotwithoutlegend[1,3]<-plotwithoutlegend[1,3]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.06   ")), size = 2.7, color = "black")
plotwithoutlegend[1,4]<-plotwithoutlegend[1,4]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.04   ")), size = 2.7, color = "black")
plotwithoutlegend[2,3]<-plotwithoutlegend[2,3]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.93   ")), size = 2.7, color = "black")
plotwithoutlegend[2,4]<-plotwithoutlegend[2,4]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.79   ")), size = 2.7, color = "black")
plotwithoutlegend[3,4]<-plotwithoutlegend[3,4]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.93   ")), size = 2.7, color = "black")

plotwithoutlegend

#"#656839", "black", "#514B23", "#BDDBD0", "#B6CB9E", "#A8763E", "#F1E8B8", "#009238", "#337CA0", "#D4D6B9", "#CBC9AD", "#1E555C", "#FDC149", "#ECC3C1"

#add legend by making one graph to grab legend
auxplot<-ggplot(plotdata, aes(x=Hill2_g, Hillinf_g))+geom_bin2d(bins=150)+
  scale_fill_gradientn(
    colours= c("#F4EBE1", "#D2B7B1","#009238","#003916", "black"),
    values= c(0,0.035,0.17,0.38,0.6,1),     
    limits=c(0,3000))+
  theme_bw() + labs(fill = "Number of \npeople")

mylegend<-grab_legend(auxplot)
g <- grid.grabExpr(print(plotwithoutlegend))

#combine legend and the graphs
ga<-grid.arrange(g, mylegend, widths=c(0.9,0.1))
ggsave(ga,filename = file.path("plots","pairsWeight.png"), width=10, height=7, dpi=450)

############################# Figure 1: ggpairs energy ########################

plotdata<-ck[, c("Rich_k", "Hill1_k", "Hill2_k", "Hillinf_k")] #select data

#write function to create the graphs in lower part
my_ggally <- function(data, mapping){
  ggplot(data,mapping) +
    geom_bin2d(bins=150)+
    scale_fill_gradientn(
      colours= c("#F4EBE1", "#D2B7B1","#009238","#003916", "black"),
      values= c(0,0.035,0.17,0.38,0.6,1),     
      limits=c(0,3000))+
    theme_bw()}
#"#D2B7B1"

my_diag <-function(data, mapping){
  ggplot(data,mapping) +
    geom_density(color="#003916")+
    theme(panel.background = element_rect(fill = "#F8F3F2"))
}


# plot the data
plotwithoutlegend<-ggpairs(
  plotdata[],
  aes(),
  lower = "blank",
  upper = list(continuous = my_ggally),
  diag = list(continuous = my_diag),
  #lower = list(continuous = wrap("cor", method = "spearman")),
  columnLabels = c('Hill["0,e"]', 'Hill["1,e"]', 'Hill["2,e"]', 'Hill["\U221E,e"]'),
  labeller = label_parsed
) 

plotwithoutlegend<-plotwithoutlegend+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(panel.border = element_blank())+ 
  theme(axis.text= element_blank(), axis.ticks = element_blank(), strip.background = element_rect(fill = "#003916"), strip.text=element_text(color="white"))
#theme(strip.text.x = element_text(size = 14), strip.text.y = element_text(size = 14)) #remove borders and grid

plotwithoutlegend[1,2]<-plotwithoutlegend[1,2]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.40   ")), size = 2.7, color = "black")
plotwithoutlegend[1,3]<-plotwithoutlegend[1,3]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.20   ")), size = 2.7, color = "black")
plotwithoutlegend[1,4]<-plotwithoutlegend[1,4]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.15   ")), size = 2.7, color = "black")
plotwithoutlegend[2,3]<-plotwithoutlegend[2,3]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.89   ")), size = 2.7, color = "black")
plotwithoutlegend[2,4]<-plotwithoutlegend[2,4]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.68   ")), size = 2.7, color = "black")
plotwithoutlegend[3,4]<-plotwithoutlegend[3,4]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.90   ")), size = 2.7, color = "black")

plotwithoutlegend

#"#656839", "black", "#514B23", "#BDDBD0", "#B6CB9E", "#A8763E", "#F1E8B8", "#009238", "#337CA0", "#D4D6B9", "#CBC9AD", "#1E555C", "#FDC149", "#ECC3C1"

#add legend by making one graph to grab legend
auxplot<-ggplot(plotdata, aes(x=Hill2_k, Hillinf_k))+geom_bin2d(bins=150)+
  scale_fill_gradientn(
    colours= c("#F4EBE1", "#D2B7B1","#009238","#003916", "black"),
    values= c(0,0.035,0.17,0.38,0.6,1),     
    limits=c(0,3000))+
  theme_bw() + labs(fill = "Number of \npeople")

mylegend<-grab_legend(auxplot)
g <- grid.grabExpr(print(plotwithoutlegend))

#combine legend and the graphs
ga<-grid.arrange(g, mylegend, widths=c(0.9,0.1))
ggsave(ga,filename = file.path("plots","pairsEnergy.png"), width=10, height=7, dpi=450)


################################# FIGURE 2:country weight #####################################

cbPalette <- c("#70FFA7","#E3D7FF","#C9FBFF","#003916","#5BC0EB", "#009238", "#E06C9F","#FCCA46", "#EDAB98")
#"#656839", "black", "#514B23", "#BDDBD0", "#B6CB9E", "#A8763E", "#F1E8B8", "#009238", "#337CA0", "#D4D6B9", "#CBC9AD", "#1E555C", "#FDC149", "#ECC3C1", "#009238","#003916",

#cbPalette <- c("black","#D2B7B1","#ECC3C1","lightblue2","slateblue", "#009238", "darkblue","deeppink2", "indianred3")

lab1 <- bquote(Hill["0"])
lab2 <- bquote(Hill["1"])
lab3 <- bquote(Hill["2"])
lab4 <- bquote(Hill["\U221E"])

#creating new function for graph lay-out
#stat_density(aes(x=Sepal.Width, colour=Species),
#             geom="line",position="identity")
#ggplot(df,aes(x=Income))+ 
#  geom_density(aes(colour=Type), show.legend = FALSE)+
#  stat_density(aes(x=Income, colour=Type), 
#               geom="line",position="identity")

my_ggplot2<- function(data, mapping,xlabel){
  ggplot(data,mapping) +
    geom_density(alpha=0, linewidth=0.75)+
    scale_colour_manual(values=cbPalette)+
    labs(x=xlabel, y="Density (%)")+
    theme_bw()+
    theme(axis.title = element_text(size = 15))+
    theme(axis.text = element_text(size = 12), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line.x.bottom = element_line(colour = "black"),
          axis.line.y.left = element_line(colour = "black"),
          legend.key.height= unit(0.75, 'cm'), 
          legend.key.width= unit(0.75, 'cm'), 
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 15))
}

#Labelling the countries 
options<-c(unique(ck$Country)) #y-axis
ck$Countrylab <- factor(ck$Country,
                        levels = options, 
                        labels = c("Fr", "It", "Sp", "UK", "Ne", "Ge", "Sw", "De", "No"))

# --- testing with countries gram NEW----

#create new dataframe
datacountrylong<-ck%>%select(c(Country,Countrylab, Rich_g, Hill1_g, Hill2_g, Hillinf_g))%>%
  mutate(Hill0_g=Rich_g, .keep = "unused")%>%
  pivot_longer(
    cols = ends_with("_g"),
    names_to = "Index",
    values_to = "Value"
  )


#my_labeller <- as_labeller(c(setosa="A[0]", versicolor="B^1", virginica="Gamma"),
#                           default = label_parsed)

#ggplot(iris) +
#  geom_bar(aes(x=Sepal.Length)) +
#  facet_wrap(~Species, labeller = my_labeller)

mylab <- as_labeller(c(
  Hill0_g = 'Hill["0"]',
  Hill1_g = 'Hill["1"]',
  Hill2_g = 'Hill["2"]',
  Hillinf_g = paste('Hill["\U221E"]')
),   default = label_parsed)

psave<-my_ggplot2(data=datacountrylong, mapping=aes(x=Value, colour=Countrylab), xlabel="Number of effective species")+facet_wrap(~Index, scales= "free_x", labeller= mylab)+
  labs(fill="Country", colour= "Country")+
  theme(strip.background = element_rect(fill = "#003916", color = "#003916"), strip.text = element_text(size= 14, color="white"))+
  theme(panel.border = element_blank(), axis.line.x.bottom = element_line(colour = "white"), axis.line.y.left = element_line(colour="white"))

ggsave(filename = file.path("plots","ploteigCountry_g.png"), psave,width=12, height=7, dpi=300)

psave

################################# FIGURE 2:country energy #####################################

#Labelling the countries 
options<-c(unique(ck$Country)) #y-axis
ck$Countrylab <- factor(ck$Country,
                        levels = options, 
                        labels = c("Fr", "It", "Sp", "UK", "Ne", "Ge", "Sw", "De", "No"))

# --- testing with countries gram NEW----

#create new dataframe
datacountrylong<-ck%>%select(c(Country,Countrylab, Rich_k, Hill1_k, Hill2_k, Hillinf_k))%>%
  mutate(Hill0_k=Rich_k, .keep = "unused")%>%
  pivot_longer(
    cols = ends_with("_k"),
    names_to = "Index",
    values_to = "Value"
  )

mylab <- as_labeller(c(
  Hill0_k = 'Hill["0,e"]',
  Hill1_k = 'Hill["1,e"]',
  Hill2_k = 'Hill["2,e"]',
  Hillinf_k = paste('Hill["\U221E,e"]')
),   default = label_parsed)

psave<-my_ggplot2(data=datacountrylong, mapping=aes(x=Value, colour=Countrylab), xlabel="Number of effective species")+facet_wrap(~Index, scales= "free_x", labeller= mylab)+
  labs(fill="Country", colour= "Country")+
  theme(strip.background = element_rect(fill = "#003916", color = "#003916"), strip.text = element_text(size= 14, color="white"))+
  theme(panel.border = element_blank(), axis.line.x.bottom = element_line(colour = "white"), axis.line.y.left = element_line(colour="white"))

ggsave(filename = file.path("plots","ploteigCountry_g.png"), psave,width=12, height=7, dpi=300)

psave

#FIGURE 1, attempt 2

#create new dataframe
datalong<-ck%>%select(c(Rich_g, Hill1_g, Hill2_g, Hillinf_g))%>%
  mutate(Hill0_g=Rich_g, .keep = "unused")%>%
  pivot_longer(
    cols = ends_with("_g"),
    names_to = "Index",
    values_to = "Value"
  )


psave<-my_ggplot2(data=datalong, mapping=aes(x=Value), xlabel="Number of effective species")+facet_wrap(~Index, scales= "free", labeller= mylab, ncol=4)+
  theme(strip.background = element_rect(fill = "#D2B7B1", color = "#D2B7B1"), strip.text = element_text(size= 14))+
  theme(panel.border = element_blank(), axis.line.x.bottom = element_line(colour = "white"), axis.line.y.left = element_line(colour="white"), panel.background = element_rect(fill = "#F4EBE1"))



###################### Make graph for comparing the Hill numbers : Hazard ratios ########################

# read data provided by IARC
d<-read_xlsx('scripts/resultaten IARC 1/ModelDeath.xlsx', sheet=3)
dtest<-d%>%
  filter(Parameter %in% (c("Hill0_g", "Hill1_g", "Hill2_g","Hillinf_g")))
dtest$`BI (ondergrens)`<-as.numeric(dtest$`BI (ondergrens)`)

# for lay-out graph
mylab <- as_labeller(c(
  Hill0_g = 'Hill["0,g/day"]',
  Hill1_g = 'Hill["1,g/day"]',
  Hill2_g = 'Hill["2,g/day"]',
  Hillinf_g= 'Hill["\U221E,g/day"]'),   default = label_parsed)

  

# plot the hazard ratio's per quintile and add horizontal errorbars

g1<-subset(dtest, Quintiles==1)
# plot the hazard ratio's per quintile and add horizontal errorbars
ggplot(dtest, aes(x = `Hazard ratio`, y = Quintiles)) +
  geom_hline(aes(yintercept = 2), linewidth=11, color="#F8F3F2", alpha=1)+
  geom_hline(aes(yintercept = 4), linewidth=11, color="#F8F3F2", alpha=1)+
  geom_errorbarh(aes(xmax=`BI(bovengrens`, xmin=`BI (ondergrens)`, height=0.000001), color="#003916")+
  geom_point(colour="#003916", shape=15, size=2)+
  geom_point(data=g1, colour="#003916", shape=15, size=2)+
  geom_vline(xintercept = 1, linewidth=0.3, linetype = "dashed")+
  facet_wrap(vars(Parameter), ncol = 2, labeller = mylab) +
  theme_bw() +
  scale_y_reverse()+
  xlim(0.6, 1.002)+
  labs(y="Quintile")+
  theme(strip.text.x = element_text(size = 11), 
        strip.text.y = element_text(size = 11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x.bottom = element_line(colour = "black"), 
        strip.background = element_rect(fill = "#003916", color = "#003916"), 
        strip.text = element_text(size= 14, color="white"))
#save graph
ggsave(filename = file.path("plots/hazard ratios weight.png"), width=6, height=4, dpi=300)

# read data provided by IARC
d<-read_xlsx('scripts/resultaten IARC 1/ModelDeath.xlsx', sheet=3)
dtest<-d%>%
  filter(Parameter %in% (c("Hill0_k", "Hill1_k", "Hill2_k","Hillinf_k")))
dtest$`BI (ondergrens)`<-as.numeric(dtest$`BI (ondergrens)`)

# for lay-out graph
mylab <- as_labeller(c(
  Hill0_k = 'Hill["0,kcal/day"]',
  Hill1_k = 'Hill["1,kcal/day"]',
  Hill2_k = 'Hill["2,kcal/day"]',
  Hillinf_k = 'Hill["\U221E,kcal/day"]'),   default = label_parsed)



# plot the hazard ratio's per quintile and add horizontal errorbars

g1<-subset(dtest, Quintiles==1)
# plot the hazard ratio's per quintile and add horizontal errorbars
ggplot(dtest, aes(x = `Hazard ratio`, y = Quintiles)) +
  geom_hline(aes(yintercept = 2), linewidth=11, color="#F8F3F2", alpha=1)+
  geom_hline(aes(yintercept = 4), linewidth=11, color="#F8F3F2", alpha=1)+
  geom_errorbarh(aes(xmax=`BI(bovengrens`, xmin=`BI (ondergrens)`, height=0.000001), color="#003916")+
  geom_point(colour="#003916", shape=15, size=2)+
  geom_point(data=g1, colour="#003916", shape=15, size=2)+
  geom_vline(xintercept = 1, linewidth=0.3, linetype = "dashed")+
  facet_wrap(vars(Parameter), ncol = 2, labeller = mylab) +
  theme_bw() +
  scale_y_reverse()+
  labs(y="Quintile")+
  xlim(0.6, 1.002)+
  theme(strip.text.x = element_text(size = 11), 
        strip.text.y = element_text(size = 11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x.bottom = element_line(colour = "black"), 
        strip.background = element_rect(fill = "#003916", color = "#003916"), 
        strip.text = element_text(size= 14, color="white"))
#save graph
ggsave(filename = file.path("plots/hazard ratios energy.png"), width=6, height=4, dpi=300)



###################### Figure 4 ############################ Indices visualisation all indices ################"


#ik<-read.csv("scripts/resultaten IARC 1/Indices_kcal.csv")[,c(2,6,8,11,14)] #full data 
ik2<-fread("scripts/resultaten IARC 1/Indices_kcal.csv") #full data 
#ig<-read.csv("scripts/resultaten IARC 1/Indices_gram.csv")[,c(2,6,8,11,14)] #full data
ig2<-fread("scripts/resultaten IARC 1/Indices_gram.csv") #full data
eigb<-fread("scripts/resultaten IARC 1/Country_idname_BMI_Sex_Age_per_person.csv") #full data
eig2<-fread("scripts/resultaten IARC 1/Age_Date_Recruitment.csv")[, c(1,3)] #full data
eig3<-fread("scripts/resultaten IARC 1/Country_idname_FBDIARC_Excleier_per_person.csv")[, c(2,4)] #full data
colnames(eig3)[1]<-"Idepic_Crypt"

#combine data
list_df<- list(ig2, ik2, eigb, eig2,eig3)
combi<- list_df %>% reduce(inner_join,  by="Idepic_Crypt")

# deleting country 6 and extreme energy intake over energy requirements and put BMI, Sex, Age and Age_recr  in classes
combi<-combi%>%
  filter(Excleier==2)%>% # delete extremes of energy intake/energy needs
  filter(Country!="6")%>% # delete Greece due to administrative constraints
  mutate(BMIclass=cut(BMI, breaks=c(1,18.5, 24.9, 29.9,100), labels = c("<18.5", "18.5-24.9", ">25", ">30")))%>%
  mutate(Sexclass=as.character(Sex))%>%
  mutate(Age_now=format(as.Date(Age, format = "%d/%m/%Y"),"%Y"))%>%
  mutate(Agerecr_class=cut(Age_Recr, breaks=c(1,40,50,60,100), labels = c("<40", "40-50", "50-60", ">60")))

#view data
summary(combi)

#shorten the names of the data
ckFull<-combi
colnames(ckFull)<-c("idname",
                "Rich_g","Marg_g","Odum_g","Menh_g","Tot_g", "Shan_g", "Hill1_g", "Simp_g", "Simpson_complement_gram", "Hill2_g", "BergerParker_gram", "BergerParker_complement_gram", "Hillinf_g", "McIn_g", 
                "Rich_k","Marg_k","Odum_k","Menh_k","Tot_k", "Shan_k", "Hill1_k", "Simp_k", "Simpson_complement_kcal", "Hill2_k", "BergerParker_kcal", "BergerParker_complement_kcal", "Hillinf_k", "McIn_k",
                "Country","BMI", "Sex","Age","Age_Recr","Excleier", "BMIc","Sexc","Age_now", "Age_Recr_c")


write.csv(ckFull, file="scripts/ckFull.csv")

#########################################################

ckFull<-as.data.frame(fread("scripts/ckFull.csv"))

datalongFull<-ckFull%>%
  select(ends_with("_k") | ends_with("_g"))%>%
  mutate(Hill0_g=Rich_g, Hill0_k=Rich_k, .keep = "unused")%>%
  select(-c("Tot_g", "Tot_k"))%>%
  pivot_longer(
    cols = matches(".*(_g|_k)$"),
    names_to = "Index",
    values_to = "Value"
  )


mylab <- as_labeller(c(
  Hill0_g = 'Hill["0,w"]',
  Hill1_g = 'Hill["1,w"]',
  Hill2_g = 'Hill["2,w"]',
  Hillinf_g= 'Hill["\U221E,w"]',
  Hill0_k = 'Hill["0,e"]',
  Hill1_k = 'Hill["1,e"]',
  Hill2_k = 'Hill["2,e"]',
  Hillinf_k= 'Hill["\U221E,e"]',
  Marg_g= 'Margalef["w"]',
  McIn_g= 'McIntosh["w"]',
  Menh_g= 'Menhinick["w"]',
  Odum_g= 'OdumCantlonKornicker["w"]',
  Simp_g= 'Simpson["w"]',
  Shan_g= 'Shannon["g"]',
  Marg_k= 'Margalef["e"]',
  McIn_k= 'McIntosh["e"]',
  Menh_k= 'Menhinick["e"]',
  Odum_k= 'OdumCantlonKornicker["e"]',
  Simp_k= 'Simpson["e"]',
  Shan_k= 'Shannon["e"]'
  ),   default = label_parsed)



psave<-ggplot(data=datalongFull, mapping=aes(x=Value))+geom_density(color="#003916")+facet_wrap(~Index, scales= "free", ncol=4, labeller= mylab)+theme_bw()+
  theme(strip.text.x = element_text(size = 11), strip.text.y = element_text(size = 11),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        axis.line.x.bottom = element_line(colour = "black"), 
        strip.background = element_rect(fill = "#003916", color = "#003916"), 
        strip.text = element_text(size= 14, color="white")) 
        
                                                                                                                                                   

psave
ggsave(psave, filename=file.path("plots/allIndices.png"), width=5, height=6, dpi=450)
#ggsave(filename = file.path("plots/hazard ratios hill0 and hillinf.png"), width=6, height=4, dpi=300)


  theme(strip.background = element_rect(fill = "#D2B7B1", color = "#D2B7B1"), strip.text = element_text(size= 14))+
  theme(panel.border = element_blank(), axis.line.x.bottom = element_line(colour = "white"), axis.line.y.left = element_line(colour="white"), panel.background = element_rect(fill = "#F4EBE1"))

unique(datalongFull[,1])


###################### Make graph for comparing the Hill numbers : in function of total consumption ########################

lab1 <- bquote(Hill["0,w"])
lab2 <- bquote(Hill["1,w"])
lab3 <- bquote(Hill["2,w"])
lab4 <- bquote(Hill["\U221E,w"])
lab5 <- bquote(Hill["0,e"])
lab6 <- bquote(Hill["1,e"])
lab7 <- bquote(Hill["2,e"])
lab8 <- bquote(Hill["\U221E,e"])

dataTotal_g<-ckFull%>%
  select(ends_with("_k") | ends_with("_g"))%>%
  mutate(Hill0_g=Rich_g, Hill0_k=Rich_k, .keep = "unused")%>%
  select(-c("Tot_g", "Tot_k"))%>%
  pivot_longer(
    cols = matches(".*(_g|_k)$"),
    names_to = "Index",
    values_to = "Value"
  )


#creating function for graph lay-out
my_scatter<-function(data, mapping,endxlim, labstitle, labsx, labsy){
  ggplot(data, mapping)+
    geom_bin2d(bins=150)+
    xlim(0,endxlim)+
    scale_fill_gradientn(colours=c("#F4EBE1", "#D2B7B1","#009238","#003916", "black"),
                         values=c(0,0.035,0.17,0.38,0.6,1),
                         limits=c(0,450))+
    theme_bw()+
    labs(x=labsx,y= labsy)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(panel.border = element_blank())+ #remove borders and grid
    theme(legend.text = element_text(size = 11),
          axis.line.x.bottom = element_line(colour = "black"),
          axis.line.y.left = element_line(colour = "black"),
          axis.title.x = element_text(size = 11),
          axis.title.y = element_text(size = 13),
          legend.title = element_text(size=11)
    )
}


# Hill numbers in function of total consumption in gram
p_hill0_scatter_gram<-my_scatter(data=ck[,c("Tot_g","Rich_g")], mapping=aes(x=Tot_g, y=Rich_g), endxlim = 8000, labsx="Total consumption (gram/day)", labsy=lab1)
p_hill1_scatter_gram<-my_scatter(data=ck[,c("Tot_g","Hill1_g")], mapping=aes(x=Tot_g, y=Hill1_g), endxlim = 8000, labsx="Total consumption (gram/day)", labsy=lab2)
p_hill2_scatter_gram<-my_scatter(data=ck[,c("Tot_g","Hill2_g")], mapping=aes(x=Tot_g, y=Hill2_g), endxlim = 8000, labsx="Total consumption (gram/day)", labsy=lab3)
p_hillinf_scatter_gram<-my_scatter(data=ck[,c("Tot_g","Hillinf_g")], mapping=aes(x=Tot_g, y=Hillinf_g), endxlim = 8000, labsx="Total consumption (gram/day)", labsy=lab4)
# Hill numbers in function of total consumption in kcal
p_hill0_scatter_kcal<-my_scatter(data=ck[,c("Tot_k","Rich_k")], mapping=aes(x=Tot_k, y=Rich_k), endxlim = 5000, labsx="Total consumption (kcal/day)", labsy=lab5)
p_hill1_scatter_kcal<-my_scatter(data=ck[,c("Tot_k","Hill1_k")], mapping=aes(x=Tot_k, y=Hill1_k), endxlim = 5000, labsx="Total consumption (kcal/day)", labsy=lab6)
p_hill2_scatter_kcal<-my_scatter(data=ck[,c("Tot_k","Hill2_k")], mapping=aes(x=Tot_k, y=Hill2_k), endxlim = 5000, labsx="Total consumption (kcal/day)", labsy=lab7)
p_hillinf_scatter_kcal<-my_scatter(data=ck[,c("Tot_k","Hillinf_k")], mapping=aes(x=Tot_k, y=Hillinf_k), endxlim = 5000, labsx="Total consumption (kcal/day)", labsy=lab8)


psave<-grid.arrange(p_hill0_scatter_gram, p_hill0_scatter_kcal, p_hill1_scatter_gram, p_hill1_scatter_kcal, p_hill2_scatter_gram, p_hill2_scatter_kcal, p_hillinf_scatter_gram, p_hillinf_scatter_kcal,ncol=2)
ggsave(filename = file.path("plots","plottotalallind.png"), psave, width=10, height=12, dpi=350)


############################ Hill numbers energy versus weight ###########################################

my_scatter2<-function(data, mapping,labstitle, labsx, labsy){
  ggplot(data, mapping)+
    geom_bin2d(bins=150)+
    theme_bw()+
    labs(x=labsx,y= labsy)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(panel.border = element_blank())+ #remove borders and grid
    theme(legend.text = element_text(size = 11),
          axis.line.x.bottom = element_line(colour = "black"),
          axis.line.y.left = element_line(colour = "black"),
          axis.title.x = element_text(size = 11),
          axis.title.y = element_text(size = 13),
          legend.title = element_text(size=11)
    )
}

p_hill0_scatter<-my_scatter2(data=ck[,c("Rich_k","Rich_g")], mapping=aes(x=Rich_k, y=Rich_g), labsx=lab5, labsy=lab1) +  scale_fill_gradientn(colours=c("#F4EBE1", "#D2B7B1","#009238","#003916", "black"),
                                                                                                                                              values=c(0,0.035,0.17,0.38,0.6,1),
                                                                                                                                              limits=c(0,10000))
p_hill1_scatter<-my_scatter2(data=ck[,c("Hill1_k","Hill1_g")], mapping=aes(x=Hill1_k, y=Hill1_g), labsx=lab6, labsy=lab2) + scale_fill_gradientn(colours=c("#F4EBE1", "#D2B7B1","#009238","#003916", "black"),
                                                                                                                                                values=c(0,0.035,0.17,0.38,0.6,1),
                                                                                                                                                limits=c(0,1000))
p_hill2_scatter<-my_scatter2(data=ck[,c("Hill2_k","Hill2_g")], mapping=aes(x=Hill2_k, y=Hill2_g), labsx=lab7, labsy=lab3) +  scale_fill_gradientn(colours=c("#F4EBE1", "#D2B7B1","#009238","#003916", "black"),
                                                                                                                                                 values=c(0,0.035,0.17,0.38,0.6,1),
                                                                                                                                                 limits=c(0,1000))
p_hillinf_scatter<-my_scatter2(data=ck[,c("Hillinf_k","Hillinf_g")], mapping=aes(x=Hillinf_k, y=Hillinf_g), labsx=lab8, labsy=lab4)  + scale_fill_gradientn(colours=c("#F4EBE1", "#D2B7B1","#009238","#003916", "black"),
                                                                                                                                                           values=c(0,0.035,0.17,0.38,0.6,1),
                                                                                                                                                           limits=c(0,1000))

psave<-grid.arrange(p_hill0_scatter, p_hill1_scatter, p_hill2_scatter, p_hillinf_scatter,ncol=2)
ggsave(filename = file.path("plots","plotcompare.png"), psave, width=10, height=12, dpi=350)
