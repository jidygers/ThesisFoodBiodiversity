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
library('cowplot')
library('gsubfn')
library('sas7bdat')
############################ Data preparation ################################################
#read data
ik<-read.csv("resultaten IARC 1/Indices_kcal.csv") #full data 
ig<-read.csv("resultaten IARC 1/Indices_gram.csv") #full data
eigb<-read.csv("resultaten IARC 1/Country_idname_BMI_Sex_Age_per_person.csv") #full data
eig2<-read.csv("resultaten IARC 1/Age_Date_Recruitment.csv")[, c(1,3)] #full data
eig3<-read.csv("resultaten IARC 1//Country_idname_FBDIARC_Excleier_per_person.csv")[, c(2,4)] #full data
colnames(eig3)[1]<-"Idepic_Crypt"

#combine data
list_df<- list(ig, ik, eigb, eig2,eig3)
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
                "Rich_g","Marg_g","Odum_g","Menh_g","Tot_g", "Shan_g", "Hill1_g", "Simp_g", "Simpson_complement_gram", "Hill2_g", "BergerParker_gram", "BergerParker_complement_gram", "Hillinf_g", "McIn_g", 
                "Rich_k","Marg_k","Odum_k","Menh_k","Tot_k", "Shan_k", "Hill1_k", "Simp_k", "Simpson_complement_kcal", "Hill2_k", "BergerParker_kcal", "BergerParker_complement_kcal", "Hillinf_k", "McIn_k",
                "Country","BMI", "Sex","Age","Age_Recr","Excleier", "BMIc","Sexc","Age_now", "Age_Recr_c")

#Note: focus is on Hill numbers

# Colour palette (colourblindness OK):
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "black")

################################ Correlations and standard deviations ###########################################
# correlations based on the index chosen:
# Pearson
cor(ck$Rich_g, ck$Rich_k)
cor(ck$Hill1_g, ck$Hill1_k)
cor(ck$Hill2_g, ck$Hill2_k)
cor(ck$Hillinf_g, ck$Hillinf_k)

# Spearman
cor(ck$Rich_g, ck$Rich_k, method="spearman")
cor(ck$Hill1_g, ck$Hill1_k, method="spearman")
cor(ck$Hill2_g, ck$Hill2_k, method="spearman")
cor(ck$Hillinf_g, ck$Hillinf_k, method="spearman")

#mean and standard deviation of the Hill numbers
mean(ck$Rich_g)
mean(ck$Rich_k)
mean(ck$Hill1_g)
mean(ck$Hill1_k)
mean(ck$Hill2_g)
mean(ck$Hill2_k)
mean(ck$Hillinf_g)
mean(ck$Hillinf_k)

sd(ck$Rich_g)
sd(ck$Rich_k)
sd(ck$Hill1_g)
sd(ck$Hill1_k)
sd(ck$Hill2_g)
sd(ck$Hill2_k)
sd(ck$Hillinf_g)
sd(ck$Hillinf_k)

mean(1/ck$Hillinf_g)
sd(1/ck$Hillinf_g)

#mean of tot_energy and mean of tot_weight
mean(ck$Tot_g)
mean(ck$Tot_k)

###################### Make graph for overview the Hill numbers ########################
p1<-ggplot(data.frame(ck$Rich_g, ck$Rich_k)) +
  geom_histogram(aes(x = ck$Rich_g, fill = "grey"), alpha = 0.5, binwidth = 0.5) +
  geom_histogram(aes(x = ck$Rich_k, fill = "seagreen3"), alpha = 0.5, binwidth = 0.5) +
  labs(x = bquote(Hill["0"]) , y = "Frequency") +
  scale_fill_manual(values = c("grey", "seagreen3"), name = "Index", labels = c(bquote(Hill["0,gram"]), bquote(Hill["0,kcal"])))+
  theme_bw()+
  theme(axis.title = element_text(size = 15))+
  theme(legend.text = element_text(size = 11))+
  theme(axis.text = element_text(size = 12), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x.bottom = element_line(colour = "black"),
        axis.line.y.left = element_line(colour = "black"))
p2<-ggplot(data.frame(ck$Hill1_g, ck$Hill1_k)) +
  geom_histogram(aes(x = ck$Hill1_g, fill = "grey"), alpha = 0.5, binwidth = 0.5) +
  geom_histogram(aes(x = ck$Hill1_k, fill = "seagreen3"), alpha = 0.5, binwidth = 0.5) +
  labs(x = bquote(Hill["1"]) , y = "Frequency") +
  scale_fill_manual(values = c("grey", "seagreen3"), name = "Index", labels = c(bquote(Hill["1,gram"]), bquote(Hill["1,kcal"])))+
  theme_bw()+
  theme(axis.title = element_text(size = 15))+
  theme(legend.text = element_text(size = 11))+
  theme(axis.text = element_text(size = 12), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x.bottom = element_line(colour = "black"),
        axis.line.y.left = element_line(colour = "black"))
p3<-ggplot(data.frame(ck$Hill2_g, ck$Hill2_k)) +
  geom_histogram(aes(x = ck$Hill2_g, fill = "grey"), alpha = 0.5, binwidth = 0.5) +
  geom_histogram(aes(x = ck$Hill2_k, fill = "seagreen3"), alpha = 0.5, binwidth = 0.5) +
  labs(x = bquote(Hill["2"]) , y = "Frequency") +
  scale_fill_manual(values = c("grey", "seagreen3"), name = "Index", labels = c(bquote(Hill["2,gram"]), bquote(Hill["2,kcal"])))+
  theme_bw()+
  theme(axis.title = element_text(size = 15))+
  theme(legend.text = element_text(size = 11))+
  theme(axis.text = element_text(size = 12), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x.bottom = element_line(colour = "black"),
        axis.line.y.left = element_line(colour = "black"))
p4<-ggplot(data.frame(ck$Hillinf_g, ck$Hillinf_k)) +
  geom_histogram(aes(x = ck$Hillinf_g, fill = "grey"), alpha = 0.5, binwidth = 0.5) +
  geom_histogram(aes(x = ck$Hillinf_k, fill = "seagreen3"), alpha = 0.5, binwidth = 0.5) +
  labs(x = bquote(Hill["\U221E"]) , y = "Frequency") +
  scale_fill_manual(values = c("grey", "seagreen3"), name = "Index", labels = c(bquote(Hill["\U221E,gram"]), bquote(Hill["\U221E,kcal"])))+
  theme_bw()+
  theme(axis.title = element_text(size = 15))+
  theme(legend.text = element_text(size = 11))+
  theme(axis.text = element_text(size = 12), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x.bottom = element_line(colour = "black"),
        axis.line.y.left = element_line(colour = "black"))
psave<-do.call(grid.arrange,list(p1,p2,p3,p4))
ggsave(filename = file.path("Clean file/plots","histograms_overview.png"),psave, width=10, height=5, dpi=300)

###################### Make graph for comparing the Hill numbers ########################
# ggpairs gram -----
plotdata<-ck[, c("Rich_g", "Hill1_g", "Hill2_g", "Hillinf_g")] #select data

#write function to create the graphs in lower part
my_ggally <- function(data, mapping){
  ggplot(data,mapping) +
    geom_bin2d(bins=150)+scale_fill_gradientn(colours=c("grey","grey","khaki","seagreen3","skyblue2","slateblue3"),
                                              values=c(0,0.02,0.1,0.30,0.4,1),limits=c(0,1250))+theme_bw()
}

# plot the data
plotwithoutlegend<-ggpairs(
  plotdata[],
  aes(),
  lower = list(continuous = my_ggally),
  #upper = list(continuous = wrap("cor", method = "spearman")),
  title = "Quantity-based Hill numbers",
  columnLabels = c('Hill["0,gram"]', 'Hill["1,gram"]', 'Hill["2,gram"]', 'Hill["\U221E,gram"]'),
  labeller = label_parsed,
  xlab = "Number of effective species",
  ylab = "Number of effective species"
) 

plotwithoutlegend[1,1] <- plotwithoutlegend[1,1] + theme(axis.text.y = element_blank(), 
                                                         axis.ticks = element_blank()) #remove text of axis of the first graph

plotwithoutlegend<-plotwithoutlegend+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(panel.border = element_blank())+ 
  theme(strip.text.x = element_text(size = 14), strip.text.y = element_text(size = 14)) #remove borders and grid

#add legend by making one graph to grab legend
auxplot<-ggplot(plotdata, aes(x=Hill2_g, Hill1_g))+geom_bin2d(bins=150)+scale_fill_gradientn(colours=c("grey","grey","khaki","seagreen3","skyblue2","slateblue3"),      
                                                                                             values=c(0,0.02,0.1,0.30,0.4,1),limits=c(0,1250))+theme_bw()        
mylegend<-grab_legend(auxplot)
g <- grid.grabExpr(print(plotwithoutlegend))

#combine legend and the graphs
grid.arrange(g, mylegend, widths=c(0.9,0.1))
ggsave(filename = file.path("Clean file/plots","pairsgram.png"), width=8, height=7, dpi=450)

# ggpairs in kcal--------

plotdata<-ck[, c("Rich_k", "Hill1_k", "Hill2_k", "Hillinf_k")] #select data

#plot the data
plotwithoutlegend<-ggpairs(
  plotdata[],
  aes(),
  lower = list(continuous = my_ggally),
  #upper = list(continuous = wrap("cor", method = "spearman")),
  title = "Energy-based Hill numbers",
  columnLabels = c('Hill["0,kcal"]', 'Hill["1,kcal"]', 'Hill["2,kcal"]', 'Hill["\U221E,kcal"]'),
  labeller= label_parsed,
  xlab = "Number of effective species",
  ylab = "Number of effective species"
)

plotwithoutlegend[1,1] <- plotwithoutlegend[1,1] + theme(axis.text.y = element_blank(), 
                                                         axis.ticks = element_blank())

plotwithoutlegend<-plotwithoutlegend+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(panel.border = element_blank())+ 
  theme(strip.text.x = element_text(size=14), strip.text.y = element_text(size = 14))#remove borders and grid

#make one graph to grab legend
auxplot<-ggplot(plotdata, aes(x=Hill2_k, Hill1_k))+geom_bin2d(bins=150)+scale_fill_gradientn(colours=c("grey","grey","khaki","seagreen3","skyblue2","slateblue3"),       
                                                                                                                values=c(0,0.02,0.1,0.30,0.4,1),limits=c(0,1250))
mylegend<-grab_legend(auxplot)

g <- grid.grabExpr(print(plotwithoutlegend))

#combine legend and the graphs
grid.arrange(g, mylegend, widths=c(0.9,0.1))
ggsave(filename = file.path("Clean file/plots","pairskcal.png"), width=8, height=7, dpi=450)

###################### Make graph for comparing the Hill numbers : quintiles and deciles ########################
lab1 <- bquote(Hill["0,gram"])
lab2 <- bquote(Hill["1,gram"])
lab3 <- bquote(Hill["2,gram"])
lab4 <- bquote(Hill["\U221E,gram"])
lab5 <- bquote(Hill["0,kcal"])
lab6 <- bquote(Hill["1,kcal"])
lab7 <- bquote(Hill["2,kcal"])
lab8 <- bquote(Hill["\U221E,kcal"])
# --- testing with Q5  ----

# calculate quintiles
q <- data.frame(
  Richness_gram = as.numeric(cut2(ck$Rich_g, g=5)),
  Hill1_gram = as.numeric(cut2(ck$Hill1_g, g=5)),
  Hill2_gram = as.numeric(cut2(ck$Hill2_g, g=5)),
  Hillinf_gram = as.numeric(cut2(ck$Hillinf_g, g=5)),
  Richness_kcal = as.numeric(cut2(ck$Rich_k, g=5)),
  Hill1_kcal = as.numeric(cut2(ck$Hill1_k, g=5)),
  Hill2_kcal = as.numeric(cut2(ck$Hill2_k, g=5)),
  Hillinf_kcal = as.numeric(cut2(ck$Hillinf_k, g=5))
)


# 1. weight-based versus energy-based

p<-list() #create empty list
col_names <- colnames(q) #select colnames
pairs <- combn(col_names, 2)[, c(4,11,17,22)] # generate all possible pairs of column names and selecting only the columns wanted for comparison
labels <- list(lab1, lab2, lab3, lab4, lab5, lab6, lab7, lab8) # combine the labels for the graph lay-out
tel<-c(1,5,2,6,3,7,4,8) #for graph lay-out

mask <- matrix(FALSE, nrow=5, ncol=5) #for calculating the number of mismatches 
mask[row(mask) < col(mask) - 1 | row(mask) > col(mask) + 1] <- TRUE # number of mismatches= people classified into quintiles that differ more than one quintile for two different indices

# Loop through the pairs and calculate the frequency table for each pair
for (i in 1:ncol(pairs)) {
  col1 <- pairs[,i][1]
  col2 <- pairs[,i][2]
  tbl <- table(q[,col1], q[,col2]) #first variable as rows, second variable as columns
  tbl_pct <- round(prop.table(tbl) * 100, 2)
  som<-sum(tbl_pct[mask]) # percentage of mismatches
  print(paste(col1, col2, som))
  table_df <- data.frame(tbl_pct) #first variable : rows, second variable: columns
 
  p[[i]]<- 
    ggplot(table_df, aes(x=Var1, y=Var2, fill=Freq)) + 
    geom_tile() + 
    scale_fill_gradientn(colours=c("white","white","khaki","seagreen3","skyblue2","slateblue3"),
                         values=c(0,0.02,0.1,0.30,0.4,1),limits=c(0,20))+
    theme(plot.title = element_text(size=10))+
    labs(x =labels[[tel[2*i-1]]], 
         y =labels[[tel[2*i]]], 
         fill = "  %")+
    theme_bw()+
    theme(axis.title = element_text(size = 15))+
    theme(legend.text = element_text(size = 11))+
    theme(axis.text = element_text(size = 12), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line.x.bottom = element_line(colour = "black"),
          axis.line.y.left = element_line(colour = "black"))
}
do.call(grid.arrange,p)
psave<-do.call(grid.arrange,c(p, ncol=4))
ggsave(filename = file.path("Clean file/plots","quintilesunit.png"),psave, width=10, height=2, dpi=300)

# 2. comparing indices based on same unit
p<-list() #create empty list
col_names <- colnames(q) #collect column names
pairs <- combn(col_names, 2)[, c(1:3,8:9,14,23:28)] #generate all possible pairs of column names and selecting only the columns wanted for comparison
labels <- list(lab1, lab2, lab3, lab4, lab5, lab6, lab7, lab8) #create list for graph lay-out
tel<-c(1,2,1,3,1,4,2,3,2,4,3,4,5,6,5,7,5,8,6,7,6,8,7,8) #for graph layout

mask <- matrix(FALSE, nrow=5, ncol=5) # calculating the number of mismatches
mask[row(mask) < col(mask) - 1 | row(mask) > col(mask) + 1] <- TRUE  # number of mismatches= people classified into quintiles that differ more than one quintile for two different indices

# Loop through the pairs and calculate the frequency table for each pair
for (i in 1:ncol(pairs)) {
  col1 <- pairs[,i][1]
  col2 <- pairs[,i][2]
  tbl <- table(q[,col1], q[,col2]) #first variable as rows, second variable as colnumns
  tbl_pct <- round(prop.table(tbl) * 100, 2)
  som<-sum(tbl_pct[mask]) # total percentage of mismatches
  print(paste(col1, col2, som))
  table_df <- data.frame(tbl_pct) #first variable : rows, second variable: columns
  
  p[[i]]<- 
    ggplot(table_df, aes(x=Var1, y=Var2, fill=Freq)) + 
    geom_tile() + 
    scale_fill_gradientn(colours=c("white","white","khaki","seagreen3","skyblue2","slateblue3"),
                         values=c(0,0.02,0.1,0.30,0.4,1),limits=c(0,20))+
    theme(plot.title = element_text(size=10))+
    labs(x =labels[[tel[2*i-1]]], 
         y =labels[[tel[2*i]]], 
         fill = "  %")+
    theme_bw()+
    theme(axis.title = element_text(size = 15))+
    theme(legend.text = element_text(size = 9))+
    theme(axis.text = element_text(size = 12), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line.x.bottom = element_line(colour = "black"),
          axis.line.y.left = element_line(colour = "black"),
          legend.key.height = unit(0.5, 'cm'))
}
do.call(grid.arrange,p)
psave<-do.call(grid.arrange,c(p, ncol=4))
ggsave(filename = file.path("Clean file/plots","quintilesindices.png"),psave,width=10, height=5.5, dpi=300)


# --- testing with Q10----
q3 <- data.frame(
  Richness_gram = as.numeric(cut2(ck$Rich_g, g=10)),
  Hill1_gram = as.numeric(cut2(ck$Hill1_g, g=10)),
  Hill2_gram = as.numeric(cut2(ck$Hill2_g, g=10)),
  Hillinf_gram = as.numeric(cut2(ck$Hillinf_g, g=10)),
  Richness_kcal = as.numeric(cut2(ck$Rich_k, g=10)),
  Hill1_kcal = as.numeric(cut2(ck$Hill1_k, g=10)),
  Hill2_kcal = as.numeric(cut2(ck$Hill2_k, g=10)),
  Hillinf_kcal = as.numeric(cut2(ck$Hillinf_k, g=10))
)

p3<-list() #create empty list
col_names <- colnames(q3)
pairs <- combn(col_names, 2)[, c(4,11,17,22,1:3,8:9,14,23:28)] #generate all possible pairs of column names and selecting only the columns wanted for comparison

mask <- matrix(FALSE, nrow=10, ncol=10) #for calculating number of mismatches
mask[row(mask) < col(mask) - 1 | row(mask) > col(mask) + 1] <- TRUE  # number of mismatches= people classified into quintiles that differ more than one quintile for two different indices


# Loop through the pairs and calculate the frequency table for each pair
for (i in 1:ncol(pairs)) {
  col1 <- pairs[,i][1]
  col2 <- pairs[,i][2]
  tbl <- table(q3[,col1], q3[,col2])
  tbl_pct <- round(prop.table(tbl) * 100, 2)
  som<-sum(tbl_pct[mask]) #percentage of mismatches
  print(paste(col1, col2, som))
  table_df <- data.frame(tbl_pct)
  
  p3[[i]]<- ggplot(table_df, aes(x=Var1, y=Var2, fill=Freq)) + 
    geom_tile() + 
    scale_fill_gradientn(colours=c("white","white","khaki","seagreen3","skyblue2","slateblue3"),
                         values=c(0,0.02,0.1,0.30,0.4,1),limits=c(0,10))+
    labs(x =labels[[tel[2*i-1]]], 
         y =labels[[tel[2*i]]], 
         fill = "%") +
    theme_bw()+
    theme(plot.title = element_text(size=9), axis.text.x = element_text(size=8), axis.text.y = element_text(size=8), axis.title.x=element_text(size=8),axis.title.y=element_text(size=8))
  }
psave<-do.call(grid.arrange,p3)
ggsave(filename = file.path("Clean file/plots","deciles.png"),psave, width=10, height=5, dpi=300)

###################### Make graph for comparing the Hill numbers : Associations with characteristics ########################

#creating new function for graph lay-out
my_ggplot<- function(data, mapping,xlabel){
    ggplot(data,mapping) +
    geom_density(alpha=0.2)+
    scale_fill_manual(values=cbPalette)+
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

# --- testing with countries kcal----
ploteigCountry_k<-list()
ploteigCountry_k[[1]]<-my_ggplot(data=ck, mapping=aes(x=Rich_k, fill=Countrylab, colour=Countrylab), xlabel=lab5)+labs(colour="Country", fill="Country")
ploteigCountry_k[[2]]<-my_ggplot(data=ck, mapping=aes(x=Hill1_k, fill=Countrylab, colour=Countrylab), xlabel=lab6)+labs(colour="Country", fill="Country")
ploteigCountry_k[[3]]<-my_ggplot(data=ck, mapping=aes(x=Hill2_k, fill=Countrylab, colour=Countrylab), xlabel=lab7)+labs(colour="Country", fill="Country")
ploteigCountry_k[[4]]<-my_ggplot(data=ck, mapping=aes(x=Hillinf_k, fill=Countrylab, colour=Countrylab), xlabel=lab8)+labs(colour="Country", fill="Country")
psave<-do.call(grid.arrange,ploteigCountry_k)
ggsave(filename = file.path("Clean file/plots","ploteigCountry_k.png"),psave,width=12, height=7, dpi=300)

# --- testing with sex kcal----
ploteigSexc_k<-list()
ploteigSexc_k[[1]]<-my_ggplot(data=ck, mapping=aes(x=Rich_k, fill=Sexc, colour=Sexc), xlabel=lab5)+labs(colour="Sex", fill="Sex")
ploteigSexc_k[[2]]<-my_ggplot(data=ck, mapping=aes(x=Hill1_k, fill=Sexc, colour=Sexc), xlabel=lab6)+labs(colour="Sex", fill="Sex")
ploteigSexc_k[[3]]<-my_ggplot(data=ck, mapping=aes(x=Hill2_k, fill=Sexc, colour=Sexc), xlabel=lab7)+labs(colour="Sex", fill="Sex")
ploteigSexc_k[[4]]<-my_ggplot(data=ck, mapping=aes(x=Hillinf_k, fill=Sexc, colour=Sexc), xlabel=lab8)+labs(colour="Sex", fill="Sex")
psave<-do.call(grid.arrange,ploteigSexc_k)
ggsave(filename = file.path("Clean file/plots","ploteigSexc_k.png"),psave, width=10, height=5, dpi=300)
# --- testing with age at recruitment kcal----
ploteigAge_Recr_c_k<-list()
ploteigAge_Recr_c_k[[1]]<-my_ggplot(data=ck, mapping=aes(x=Rich_k, fill=Age_Recr_c, colour=Age_Recr_c), xlabel=lab5)+labs(colour="Age", fill="Age")
ploteigAge_Recr_c_k[[2]]<-my_ggplot(data=ck, mapping=aes(x=Hill1_k, fill=Age_Recr_c, colour=Age_Recr_c), xlabel=lab6)+labs(colour="Age", fill="Age")
ploteigAge_Recr_c_k[[3]]<-my_ggplot(data=ck, mapping=aes(x=Hill2_k, fill=Age_Recr_c, colour=Age_Recr_c), xlabel=lab7)+labs(colour="Age", fill="Age")
ploteigAge_Recr_c_k[[4]]<-my_ggplot(data=ck, mapping=aes(x=Hillinf_k, fill=Age_Recr_c, colour=Age_Recr_c), xlabel=lab8)+labs(colour="Age", fill="Age")
psave<-do.call(grid.arrange,ploteigAge_Recr_c_k)
ggsave(filename = file.path("Clean file/plots","ploteigAge_Recr_c_k.png"), psave,width=10, height=5, dpi=300)
# --- testing with BMI kcal----
ploteigBMIc_k<-list()
ploteigBMIc_k[[1]]<-my_ggplot(data=ck, mapping=aes(x=Rich_k, fill=BMIc, colour=BMIc), xlabel=lab5)+labs(colour="BMI", fill="BMI")
ploteigBMIc_k[[2]]<-my_ggplot(data=ck, mapping=aes(x=Hill1_k, fill=BMIc, colour=BMIc), xlabel=lab6)+labs(colour="BMI", fill="BMI")
ploteigBMIc_k[[3]]<-my_ggplot(data=ck, mapping=aes(x=Hill2_k, fill=BMIc, colour=BMIc), xlabel=lab7)+labs(colour="BMI", fill="BMI")
ploteigBMIc_k[[4]]<-my_ggplot(data=ck, mapping=aes(x=Hillinf_k, fill=BMIc, colour=BMIc), xlabel=lab8)+labs(colour="BMI", fill="BMI")
psave<-do.call(grid.arrange,ploteigBMIc_k)
ggsave(filename = file.path("Clean file/plots","ploteigBMIc_k.png"), psave,width=10, height=5, dpi=300)

# --- testing with countries gram----
ploteigCountry_g<-list()
ploteigCountry_g[[1]]<-my_ggplot(data=ck, mapping=aes(x=Rich_g, fill=Countrylab, colour=Countrylab), xlabel=lab1)+labs(colour="Country", fill="Country")
ploteigCountry_g[[2]]<-my_ggplot(data=ck, mapping=aes(x=Hill1_g, fill=Countrylab, colour=Countrylab), xlabel=lab2)+labs(colour="Country", fill="Country")
ploteigCountry_g[[3]]<-my_ggplot(data=ck, mapping=aes(x=Hill2_g, fill=Countrylab, colour=Countrylab), xlabel=lab3)+labs(colour="Country", fill="Country")
ploteigCountry_g[[4]]<-my_ggplot(data=ck, mapping=aes(x=Hillinf_g, fill=Countrylab, colour=Countrylab), xlabel=lab4)+labs(colour="Country", fill="Country")
psave<-do.call(grid.arrange,ploteigCountry_g)
ggsave(filename = file.path("Clean file/plots","ploteigCountry_g.png"), psave,width=12, height=7, dpi=300)

# --- testing with sex gram----
ploteigSexc_g<-list()
ploteigSexc_g[[1]]<-my_ggplot(data=ck, mapping=aes(x=Rich_g, fill=Sexc, colour=Sexc), xlabel=lab1)+labs(colour="Sex", fill="Sex")
ploteigSexc_g[[2]]<-my_ggplot(data=ck, mapping=aes(x=Hill1_g, fill=Sexc, colour=Sexc), xlabel=lab2)+labs(colour="Sex", fill="Sex")
ploteigSexc_g[[3]]<-my_ggplot(data=ck, mapping=aes(x=Hill2_g, fill=Sexc, colour=Sexc), xlabel=lab3)+labs(colour="Sex", fill="Sex")
ploteigSexc_g[[4]]<-my_ggplot(data=ck, mapping=aes(x=Hillinf_g, fill=Sexc, colour=Sexc), xlabel=lab4)+labs(colour="Sex", fill="Sex")
psave<-do.call(grid.arrange,ploteigSexc_g)
ggsave(filename = file.path("Clean file/plots","ploteigSexc_g.png"), psave,width=10, height=5, dpi=300)
# --- testing with age at recruitment gram----
ploteigAge_Recr_c_g<-list()
ploteigAge_Recr_c_g[[1]]<-my_ggplot(data=ck, mapping=aes(x=Rich_g, fill=Age_Recr_c, colour=Age_Recr_c), xlabel=lab1)+labs(colour="Age", fill="Age")
ploteigAge_Recr_c_g[[2]]<-my_ggplot(data=ck, mapping=aes(x=Hill1_g, fill=Age_Recr_c, colour=Age_Recr_c), xlabel=lab2)+labs(colour="Age", fill="Age")
ploteigAge_Recr_c_g[[3]]<-my_ggplot(data=ck, mapping=aes(x=Hill2_g, fill=Age_Recr_c, colour=Age_Recr_c), xlabel=lab3)+labs(colour="Age", fill="Age")
ploteigAge_Recr_c_g[[4]]<-my_ggplot(data=ck, mapping=aes(x=Hillinf_g, fill=Age_Recr_c, colour=Age_Recr_c), xlabel=lab4)+labs(colour="Age", fill="Age")
psave<-do.call(grid.arrange,ploteigAge_Recr_c_g)
ggsave(filename = file.path("Clean file/plots","ploteigAge_Recr_c_g.png"), psave,width=10, height=5, dpi=300)

# --- testing with BMI gram----
ploteigBMIc_g<-list()
ploteigBMIc_g[[1]]<-my_ggplot(data=ck, mapping=aes(x=Rich_g, fill=BMIc, colour=BMIc), xlabel=lab1)+labs(colour="BMI", fill="BMI")
ploteigBMIc_g[[2]]<-my_ggplot(data=ck, mapping=aes(x=Hill1_g, fill=BMIc, colour=BMIc), xlabel=lab2)+labs(colour="BMI", fill="BMI")
ploteigBMIc_g[[3]]<-my_ggplot(data=ck, mapping=aes(x=Hill2_g, fill=BMIc, colour=BMIc), xlabel=lab3)+labs(colour="BMI", fill="BMI")
ploteigBMIc_g[[4]]<-my_ggplot(data=ck, mapping=aes(x=Hillinf_g, fill=BMIc, colour=BMIc), xlabel=lab4)+labs(colour="BMI", fill="BMI")
psave<-do.call(grid.arrange,ploteigBMIc_g)
ggsave(filename = file.path("Clean file/plots","ploteigBMIc_g.png"), psave,width=10, height=5, dpi=300)


###################### Make graph for comparing the Hill numbers : in function of total consumption ########################

#creating function for graph lay-out
my_scatter<-function(data, mapping,endxlim, labstitle, labsx, labsy){
  ggplot(data, mapping)+
    geom_bin2d(bins=150)+
    xlim(0,endxlim)+
    scale_fill_gradientn(colours=c("lightgrey","lightgrey","khaki","seagreen3","skyblue2","slateblue3"),
                         values=c(0,0.02,0.1,0.30,0.4,1),
                         limits=c(0,350))+
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
p_hill0_scatter_gram<-my_scatter(data=ck[,c("Tot_g","Rich_g")], mapping=aes(x=Tot_g, y=Rich_g), endxlim = 7000, labsx="Total consumption (gram/day)", labsy=lab1)
p_hill1_scatter_gram<-my_scatter(data=ck[,c("Tot_g","Hill1_g")], mapping=aes(x=Tot_g, y=Hill1_g), endxlim = 7000, labsx="Total consumption (gram/day)", labsy=lab2)
p_hill2_scatter_gram<-my_scatter(data=ck[,c("Tot_g","Hill2_g")], mapping=aes(x=Tot_g, y=Hill2_g), endxlim = 7000, labsx="Total consumption (gram/day)", labsy=lab3)
p_hillinf_scatter_gram<-my_scatter(data=ck[,c("Tot_g","Hillinf_g")], mapping=aes(x=Tot_g, y=Hillinf_g), endxlim = 7000, labsx="Total consumption (gram/day)", labsy=lab4)
# Hill numbers in function of total consumption in gram
p_hill0_scatter_kcal<-my_scatter(data=ck[,c("Tot_k","Rich_k")], mapping=aes(x=Tot_k, y=Rich_k), endxlim = 4000, labsx="Total consumption (kcal/day)", labsy=lab5)
p_hill1_scatter_kcal<-my_scatter(data=ck[,c("Tot_k","Hill1_k")], mapping=aes(x=Tot_k, y=Hill1_k), endxlim = 4000, labsx="Total consumption (kcal/day)", labsy=lab6)
p_hill2_scatter_kcal<-my_scatter(data=ck[,c("Tot_k","Hill2_k")], mapping=aes(x=Tot_k, y=Hill2_k), endxlim = 4000, labsx="Total consumption (kcal/day)", labsy=lab7)
p_hillinf_scatter_kcal<-my_scatter(data=ck[,c("Tot_k","Hillinf_k")], mapping=aes(x=Tot_k, y=Hillinf_k), endxlim = 4000, labsx="Total consumption (kcal/day)", labsy=lab8)

psave<-grid.arrange(p_hill0_scatter_gram, p_hill0_scatter_kcal, ncol=2)
ggsave(filename = file.path("Clean file/plots","plottotalhill0.png"), psave, width=10, height=2, dpi=300)

psave<-grid.arrange(p_hill0_scatter_gram, p_hill0_scatter_kcal, p_hill1_scatter_gram, p_hill1_scatter_kcal, p_hill2_scatter_gram, p_hill2_scatter_kcal, p_hillinf_scatter_gram, p_hillinf_scatter_kcal,ncol=2)
ggsave(filename = file.path("Clean file/plots","plottotalallind.png"), psave, width=10, height=12, dpi=300)

###################### Make graph for comparing the Hill numbers : Quintiles per country ########################

options<-unique(ck$Country) #countries in the dataset
numbercountries<-length(options) #number of counties
q_country<-vector(mode = "list", length = numbercountries+1) #also an extra for "all" countries

# In the first list collect the data for the countries combined
q_country[[1]]<- data.frame(
  land = rep("All", times=5), 
  Richness_gram = levels(cut2(ck$Rich_g, g=5)),
  Hill1_gram = levels(cut2(ck$Hill1_g, g=5)),
  Hill2_gram = levels(cut2(ck$Hill2_g, g=5)),
  Hillinf_gram = levels(cut2(ck$Hillinf_g, g=5)),
  Richness_kcal = levels(cut2(ck$Rich_k, g=5)),
  Hill1_kcal = levels(cut2(ck$Hill1_k, g=5)),
  Hill2_kcal = levels(cut2(ck$Hill2_k, g=5)),
  Hillinf_kcal = levels(cut2(ck$Hillinf_k, g=5))) 

# In the next lists collect the data for each index and each country, store per country in the list
for (i in (1:length(options))){
  plot_data<-ck%>%
    filter(Country==options[i])
  q_country[[i+1]]<- data.frame(
    land = rep(options[i], times=5),
    Richness_gram = levels(cut2(plot_data$Rich_g, g=5)),
    Hill1_gram = levels(cut2(plot_data$Hill1_g, g=5)),
    Hill2_gram = levels(cut2(plot_data$Hill2_g, g=5)),
    Hillinf_gram = levels(cut2(plot_data$Hillinf_g, g=5)),
    Richness_kcal = levels(cut2(plot_data$Rich_k, g=5)),
    Hill1_kcal = levels(cut2(plot_data$Hill1_k, g=5)),
    Hill2_kcal = levels(cut2(plot_data$Hill2_k, g=5)),
    Hillinf_kcal = levels(cut2(plot_data$Hillinf_k, g=5)))
}

options<-c("All", unique(ck$Country)) # labels for the y-axis
options2<-colnames(q_country[[1]])[2:9] #names of every index, so for every figure
p_quintiles<-vector(mode = "list", length = length(options2)) #preamble a list for the figures
l<-(5+1)*(length(options)) # 5 quintiles, but a starting and endpoint and times the number of countries

for (j in 2:9){ #number of figures
  start<-1 #make a counting variable
  df <- data.frame(x = rep(NA,l),y = rep(NA,l)) #preamble of dataframes
  
  #for each country:
  for (i in 1:length(options)){ 
    breaks<-data.frame(unique(as.vector(strapply(unique(q_country[[i]][j]), "[.0-9]+", as.numeric, simplify= rbind)))) #collect the data of the breaks for the j-th index
    einde<-start+nrow(breaks)-1 
    df[start:einde,1]<-breaks #sometimes begin and end of breakpoint is the same
    df[start:einde,2]<-options[i]
    start<-einde+1
  }
  
  df<-na.omit(df) #because the dataframe data.frame(x = rep(NA,l),y = rep(NA,l)) can be longer, because 2 breakpoints can be the same number
  df$y <- factor(df$y,
    levels = options, 
    labels = c("All", "Fr", "It", "Sp", "UK", "Ne", "Ge", "Sw", "De", "No"))
  
  #make the plot for the j-th index
  p_quintiles[[j-1]]<-ggplot(df, aes(x=x, y=y))+
    geom_point()+
    labs(title=labels[[j-1]])+
    xlab("Number of effective species")+ 
    ylab("")+
    theme_bw()+
    theme(plot.title = element_text(size = 20),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14), 
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line.x.bottom = element_line(colour = "black"),
          axis.line.y.left = element_line(colour = "black"))
}

#combining the graphs in the right order
p_quintiles2<-list()
p_quintiles2<-p_quintiles[c(1,5,2,6,3,7,4,8)]
psave<-do.call(grid.arrange,c(p_quintiles2, ncol=2))

#save graph
ggsave(filename = file.path("Clean file/plots","quintilespercountry.png"), psave, width=10, height=12, dpi=300)

###################### Make graph for comparing the Hill numbers : Hazard ratios ########################
## only hill0 and hillinf

# read data provided by IARC
d<-read_xlsx('resultaten IARC 1/ModelDeath.xlsx', sheet=3)
dtest<-d%>%
  filter(Parameter %in% (c("Hill0_g", "Hillinf_g", "Hill0_k","Hillinf_k")))
dtest$`BI (ondergrens)`<-as.numeric(dtest$`BI (ondergrens)`)

# for lay-out graph
mylab <- as_labeller(c(
  Hill0_g = 'Hill["0,gram"]',
  Hillinf_g = paste('Hill["\U221E,gram"]'),
  Hill0_k = 'Hill["0,kcal"]',
  Hillinf_k = paste('Hill["\U221E,kcal"]')
),   default = label_parsed)

# plot the hazard ratio's per quintile and add horizontal errorbars

g1<-subset(dtest, Quintiles==1)
# plot the hazard ratio's per quintile and add horizontal errorbars
ggplot(dtest, aes(x = `Hazard ratio`, y = Quintiles)) +
  geom_point() +
  geom_errorbarh(aes(xmax=`BI(bovengrens`, xmin=`BI (ondergrens)`, height=0.2))+
  geom_point(data=g1, colour="white")+
  geom_vline(xintercept = 1, linewidth=0.6, linetype = "dashed")+
  facet_wrap(vars(Parameter), ncol = 2, labeller = mylab) +
  theme_bw() +
  scale_y_reverse()+
  theme(strip.text.x = element_text(size = 11), 
        strip.text.y = element_text(size = 11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x.bottom = element_line(colour = "black"))
#save graph
ggsave(filename = file.path("Clean file/plots","hazard ratios hill0 and hillinf.png"), width=6, height=4, dpi=300)

#all indices
d$`BI (ondergrens)`<-as.numeric(d$`BI (ondergrens)`)

# for lay-out
mylab <- as_labeller(c(
  Hill0_g = 'Hill["0,gram"]',
  Hill1_g = 'Hill["1,gram"]',
  Hill2_g = 'Hill["2,gram"]',
  Hillinf_g = paste('Hill["\U221E,gram"]'),
  Hill0_k = 'Hill["0,kcal"]',
  Hill1_k = 'Hill["1,kcal"]',
  Hill2_k = 'Hill["2,kcal"]',
  Hillinf_k = paste('Hill["\U221E,kcal"]')
),   default = label_parsed)

# plot the hazard ratio's per quintile and add horizontal errorbars
g1<-subset(d, Quintiles==1)
# plot the hazard ratio's per quintile and add horizontal errorbars
ggplot(d, aes(x = `Hazard ratio`, y = Quintiles)) +
  geom_point() +
  geom_errorbarh(aes(xmax=`BI(bovengrens`, xmin=`BI (ondergrens)`, height=0.2))+
  geom_point(data=g1, colour="white")+
  geom_vline(xintercept = 1, linewidth=0.6, linetype = "dashed")+
  facet_wrap(vars(Parameter), ncol = 2, labeller = mylab) +
  theme_bw() +
  scale_y_reverse()+
  theme(strip.text.x = element_text(size = 11), 
        strip.text.y = element_text(size = 11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x.bottom = element_line(colour = "black"))

#save plot
ggsave(filename = file.path("Clean file/plots","hazard ratios all indices.png"), width=6, height=8, dpi=300)
       
############################ Jill_x: correlation ####################################

#read data
indices_kcal<-read.csv("resultaten IARC 1/BP_extra_kcal.csv")
indices_gram<-read.csv("resultaten IARC 1/BP_extra_gram.csv")

# combine indices_kcal, eigb, eig3
colnames(indices_kcal)[1]<-"Idepic_Crypt" #give corresponding name for combining 
list_df<- list(indices_kcal, eigb, eig3)
combine_kcal<- list_df %>% reduce(inner_join,  by="Idepic_Crypt")

#indices_gram, eigb, eig3
colnames(indices_gram)[1]<-"Idepic_Crypt"
list_df<- list(indices_gram, eigb, eig3)
combine_gram<- list_df %>% reduce(inner_join,  by="Idepic_Crypt")

# deleting country 6 and extreme energy intake over energy requirements
indices_kcal<-combine_kcal%>%
  filter(Country!=6)%>%
  filter(Excleier==2)

indices_gram<-combine_gram%>%
  filter(Country!=6)%>%
  filter(Excleier==2)

# energy-based
# calculate correlation with hill2 for Jill_x with varying x
indices<-c("Hillinf", "Hillinf2", "Hillinf3","Hillinf4", "Hillinf5")
plotdata<-data.frame(x=indices, cor=c(NA,NA,NA,NA,NA), corSpearman= c(NA,NA,NA,NA,NA)) 
for(i in 1:length(indices)){
  plotdata[[i,2]]<-cor(indices_kcal$Hill2, indices_kcal[[indices[[i]]]])
  plotdata[[i,3]]<-cor(indices_kcal$Hill2, indices_kcal[[indices[[i]]]], method = "spearman")
}

# plot the correlations
plotcorkcal <- ggplot(plotdata, aes(x=indices, y=cor)) +
  geom_col() +
  labs(x="", y="Pearson correlation coefficient", 
       title= expression(paste("Correlation between ", Hill["2,kcal"], " and ..." ))) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=c(bquote(Hill["\U221E,kcal"]), bquote(Jill["2,kcal"]), 
                            bquote(Jill["3,kcal"]), bquote(Jill["4,kcal"]), 
                            bquote(Jill["5,kcal"]))) +
  coord_cartesian(ylim=c(0.75, 1))+
  theme(axis.text.x = element_text(size=15))+
  theme(axis.title = element_text(size = 15))+
  theme(axis.text.y = element_text(size = 13), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x.bottom = element_line(colour = "black"),
        axis.line.y.left = element_line(colour = "black"), 
        plot.title = element_text(size=15))


ggsave(filename = file.path("Clean file/plots","plotcorkcal.png"),plotcorkcal, width=5, height=7, dpi=300)

# weight-based
# calculate correlation with hill2 for Jill_x with varying x
indices<-c("Hillinf", "Hillinf2", "Hillinf3","Hillinf4", "Hillinf5")
plotdata<-data.frame(x=indices, cor=c(NA,NA,NA,NA,NA), corSpearman= c(NA,NA,NA,NA,NA)) 
for(i in 1:length(indices)){
  plotdata[[i,2]]<-cor(indices_gram$Hill2, indices_gram[[indices[[i]]]])
  plotdata[[i,3]]<-cor(indices_gram$Hill2, indices_gram[[indices[[i]]]], method = "spearman")
}

#plot the correlations
plotcorgram<-ggplot(plotdata, aes(x=indices, y=cor))+geom_col()+
  coord_cartesian(ylim=c(0.75, 1))+
  labs(x="", 
       y="Pearson correlation coefficient", 
       title= expression(paste("Correlation between ", Hill["2,gram"], " and ..." )))+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  scale_x_discrete(labels=c(bquote(Hill["\U221E,gram"]), bquote(Jill["2,gram"]), bquote(Jill["3,gram"]), bquote(Jill["4,gram"]), bquote(Jill["5,gram"])))+
  theme(axis.text.x = element_text(size=15))+
  theme(axis.title = element_text(size = 15))+
  theme(axis.text.y = element_text(size = 13), 
        panel.border = element_blank(),
        axis.line.x.bottom = element_line(colour = "black"),
        axis.line.y.left = element_line(colour = "black"), 
        plot.title = element_text(size=15))
ggsave(filename = file.path("Clean file/plots","plotcorgram.png"),plotcorgram, width=5, height=7, dpi=300)

#combine the graphs
psave<-do.call(grid.arrange,c(list(plotcorgram, plotcorkcal), ncol=2))
ggsave(filename = file.path("Clean file/plots","plotcor_gram_kcal.png"), psave, width=13, height=7, dpi=600)


############################ Jill_x: Proportions per person #####################################
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



# energy-based: calculate the 10 most abundant species per person

#create empty plotdata and an column x for numbering the 10 most abundant species per person
plotdata<-data.frame(name=rep(NA,10*nrow(kcal_pi)), 
                     y=rep(NA,10*nrow(kcal_pi)),
                     x=rep(1:10, times=nrow(kcal_pi)))

#selecting the 10 most abundant species
for (i in 1:nrow(kcal_pi)){
  plotdata[((i-1)*10+1):(i*10),1]<-rep(rownames(kcal_pi)[i],10)
  plotdata[((i-1)*10+1):(i*10),2]<-t(kcal_pi[i,order(kcal_pi[i,], decreasing = TRUE)[1:10]])
}

# plot where one line represents one person
ab_pp_kcal<-ggplot(plotdata, aes(x, y, group=name))+
  geom_line(linewidth=0.1)+
  scale_x_continuous(breaks = seq(0, 10, by = 1))+
  labs(title="Energy-based", x="10 most abundant species", y="Proportion")+
  ylim(0,0.6)+
  theme_bw()+
  theme(axis.text = element_text(size = 14), 
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x.bottom = element_line(colour = "black"),
        axis.line.y.left = element_line(colour = "black"), 
        axis.title = element_text(size=15)
       )
ggsave(filename = file.path("Clean file/plots","ab_pp_kcal.png"), ab_pp_kcal, width=4, height=3.5, dpi=300)

# weight-based: calculate the 10 most abundant species per person

#create empty plotdata and an column x for numbering the 10 most abundant species per person
plotdata<-data.frame(name=rep(NA,10*nrow(gram_pi)), 
                     y=rep(NA,10*nrow(gram_pi)),
                     x=rep(1:10, times=nrow(gram_pi)))

#selecting the 10 most abundant species
for (i in 1:nrow(gram_pi)){
  plotdata[((i-1)*10+1):(i*10),1]<-rep(rownames(gram_pi)[i],10)
  plotdata[((i-1)*10+1):(i*10),2]<-t(gram_pi[i,order(gram_pi[i,], decreasing = TRUE)[1:10]])
}

# plot where one line represents one person
ab_pp_gram<-ggplot(plotdata, aes(x, y, group=name))+
  geom_line(linewidth=0.1)+
  scale_x_continuous(breaks = seq(0, 10, by = 1))+
  labs(title="Weight-based", x="10 most abundant species", y="Proportion")+
  ylim(0,0.6)+
  theme_bw()+
  theme(axis.text = element_text(size = 14), 
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x.bottom = element_line(colour = "black"),
        axis.line.y.left = element_line(colour = "black"),
        axis.title = element_text(size=15)
        )
  
ggsave(filename = file.path("Clean file/plots","ab_pp_gram.png"), ab_pp_gram, width=4, height=3.5, dpi=300)

#combine the graphs
psave<-do.call(grid.arrange,c(list(ab_pp_gram, ab_pp_kcal), ncol=2))
ggsave(filename = file.path("Clean file/plots","ab_pp_gram_kcal.png"), psave, width=10, height=7, dpi=300)


############################ END #####################################
