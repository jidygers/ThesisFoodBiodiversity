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
library('hillR')

################ PLOT COUNTRY KCAL ##################

#deel 1
ck<-as.data.frame(fread("scripts/ck.csv"))

#Labelling the countries 
options<-c(unique(ck$Country)) #y-axis
ck$Countrylab <- factor(ck$Country,
                        levels = options, 
                        labels = c("FR", "IT", "ES", "GB", "NL", "DE", "SE", "DK", "NO"))

#put in long format for plotting
datacountrylong<-ck%>%select(c(Country,Countrylab, Rich_k, Hill1_k, Hill2_k, Hillinf_k))%>%
  mutate(Hill0_k=Rich_k, .keep = "unused")%>%
  pivot_longer(
    cols = ends_with("_k"),
    names_to = "Index",
    values_to = "Value"
  )%>%
  mutate(Dataset="EPIC")

#deel2

A_indices_kcal<-read.csv("clean scripts/A_indices_kcal.csv", row.names=1)

A_indices_kcal
#make labels
options<-c(unique(A_indices_kcal$country)) #y-axis
A_indices_kcal$Countrylab <- factor(A_indices_kcal$country,
                                        levels = options, 
                                        labels = c("DR", "EC","KE","LK", "VN"))
#put in long format
A_datacountrylong<-A_indices_kcal%>%
  mutate(Hill0_k=A_Rich_k, .keep = "unused")%>%
  mutate(Hill1_k=A_Hill1_k, .keep= "unused")%>%
  mutate(Hill2_k=A_Hill2_k, .keep= "unused")%>%
  mutate(Hillinf_k=A_Hillinf_k, .keep= "unused")%>%
  mutate(Country=country, .keep= "unused")%>%
  pivot_longer(
    cols = ends_with("_k"),
    names_to = "Index",
    values_to = "Value"
  )%>%
  mutate(Dataset="LMIC")

#combine the two datasets
common_columns <- c("Countrylab", "Index", "Value", "Dataset")
U_datacountrylong<-rbind(datacountrylong[,common_columns], A_datacountrylong[,common_columns])

#visualisation
result <- A_datacountrylong %>%
  group_by(Countrylab) %>%
  summarise(Count = n())

cbPalette <- c("#70FFA7","#E3D7FF","#C9FBFF","#003916","#5BC0EB", "#009238","#E06C9F", "red", "blue", "yellow", "green", "grey", "orange","purple", "pink", "lightblue")

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

mylab <- as_labeller(c(
  Hill0_k = 'Hill["0,kcal/day"]',
  Hill1_k = 'Hill["1,kcal/day"]',
  Hill2_k = 'Hill["2,kcal/day"]',
  Hillinf_k = paste('Hill["\U221E,kcal/day"]'),
  LMIC = "LMIC",
  EPIC= "EPIC"
),   default = label_parsed)

psave<-my_ggplot2(data=U_datacountrylong, mapping=aes(x=Value, colour=Countrylab), xlabel="Number of effective species")+facet_grid(Dataset~Index, scales= "free_x", labeller= mylab)+
  labs(fill="Country", colour= "Country")+
  theme(strip.background = element_rect(fill = "#003916", color = "#003916"), strip.text = element_text(size= 14, color="white"))+
  theme(panel.border = element_blank(), axis.line.x.bottom = element_line(colour = "white"), axis.line.y.left = element_line(colour="white"))

ggsave(filename = file.path("ExtraData/EDplots","U_ploteigCountry_k.png"), psave,width=12, height=7, dpi=300)

################ PLOT COUNTRY GRAM ##################

#deel 1  partly the same as kcal

#put in long format for plotting
datacountrylong<-ck%>%select(c(Country,Countrylab, Rich_g, Hill1_g, Hill2_g, Hillinf_g))%>%
  mutate(Hill0_g=Rich_g, .keep = "unused")%>%
  pivot_longer(
    cols = ends_with("_g"),
    names_to = "Index",
    values_to = "Value"
  )%>%
  mutate(Dataset="EPIC")

#deel2  partly the same as kcal
A_indices_gram<-read.csv("clean scripts/A_indices_gram.csv", row.names=1)

#make labels
options<-c(unique(A_indices_gram$country)) #y-axis
A_indices_gram$Countrylab <- factor(A_indices_gram$country,
                                    levels = options, 
                                    labels = c("DR", "EC","KE","LK", "VN"))

#put in long format
A_datacountrylong<-A_indices_gram%>%
  mutate(Hill0_g=A_Rich_g, .keep = "unused")%>%
  mutate(Country=country, .keep= "unused")%>%
  mutate(Hill1_g=A_Hill1_g, .keep= "unused")%>%
  mutate(Hill2_g=A_Hill2_g, .keep= "unused")%>%
  mutate(Hillinf_g=A_Hillinf_g, .keep= "unused")%>%
  pivot_longer(
    cols = ends_with("_g"),
    names_to = "Index",
    values_to = "Value"
  )%>%
  mutate(Dataset="LMIC")

#combine the two datasets
U_datacountrylong<-rbind(datacountrylong[, common_columns], A_datacountrylong[,common_columns])

#visualisation
cbPalette <- c("#70FFA7","#E3D7FF","#C9FBFF","#003916","#5BC0EB", "#009238","#E06C9F", "red", "blue", "yellow", "green", "grey", "orange","purple", "pink", "lightblue")

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

mylab <- as_labeller(c(
  Hill0_g = 'Hill["0,g/day"]',
  Hill1_g = 'Hill["1,g/day"]',
  Hill2_g= 'Hill["2,g/day"]',
  Hillinf_g = paste('Hill["\U221E,g/day"]'),
  EPIC="EPIC", 
  LMIC="LMIC"
),   default = label_parsed)

psave<-my_ggplot2(data=U_datacountrylong, mapping=aes(x=Value, colour=Countrylab), xlabel="Number of effective species")+facet_grid(Dataset~Index, scales= "free_x", labeller= mylab)+
  labs(fill="Country", colour= "Country")+
  theme(strip.background = element_rect(fill = "#003916", color = "#003916"), strip.text = element_text(size= 14, color="white"))+
  theme(panel.border = element_blank(), axis.line.x.bottom = element_line(colour = "white"), axis.line.y.left = element_line(colour="white"))

ggsave(filename = file.path("ExtraData/EDplots","U_ploteigCountry_w.png"), psave,width=12, height=7, dpi=300)

################ PLOT CORRELATION KCAL 

############################# Figure 1: ggpairs energy ########################

plotdata<-ck[,c("Rich_k","Hill1_k","Hill2_k","Hillinf_k")]
A_plotdata<-A_indices_kcal[, c("A_Rich_k", "A_Hill1_k", "A_Hill2_k", "A_Hillinf_k")] #select data
colnames(A_plotdata)<-c("Rich_k", "Hill1_k", "Hill2_k", "Hillinf_k")

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

my_diag <-function(data, mapping, color){
  ggplot(data,mapping) +
    geom_density(color=color)+
    theme(panel.background = element_rect(fill = "#F8F3F2"))
}

A_my_ggally <- function(data, mapping){
  ggplot(data,mapping) +
    geom_bin2d(bins=150)+
    scale_fill_gradientn(
      colours= c("#F4EBE1", "#D2B7B1","slateblue1", "slateblue4", "black"),
      values= c(0,0.035,0.17,0.38,0.6,1),     
      limits=c(0,50))+
    theme_bw()}
#"#D2B7B1"


# plot the data
plotwithoutlegend<-ggpairs(
  plotdata[],
  aes(),
  #lower = "blank",
  upper = list(continuous = my_ggally),
  #diag = list(continuous = my_diag),
  diag = "blank",
  lower = list(continuous = wrap("cor", method = "spearman")),
  columnLabels = c('Hill["0,kcal/day"]', 'Hill["1,kcal/day"]', 'Hill["2,kcal/day"]', 'Hill["\U221E,kcal/day"]'),
  labeller = label_parsed
) 

A_plotwithoutlegend<-ggpairs(
  A_plotdata[],
  aes(),
  #lower = "blank",
  lower = list(continuous = A_my_ggally),
  diag= "blank",
  #diag = list(continuous = my_diag),
  upper = list(continuous = wrap("cor", method = "spearman")),
  columnLabels = c('Hill["0,kcal/day"]', 'Hill["1,kcal/day"]', 'Hill["2,kcal/day"]', 'Hill["\U221E,kcal/day"]'),
  labeller = label_parsed
) 

plotwithoutlegend<-plotwithoutlegend+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(panel.border = element_blank())+ 
  theme(axis.text= element_blank(), axis.ticks = element_blank(), strip.background = element_rect(fill = "#003916"), strip.text=element_text(color="white"))
#theme(strip.text.x = element_text(size = 14), strip.text.y = element_text(size = 14)) #remove borders and grid

plotwithoutlegend[1,2]<-plotwithoutlegend[1,2]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.39   ")), size = 2.7, color = "black")
plotwithoutlegend[1,3]<-plotwithoutlegend[1,3]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.19   ")), size = 2.7, color = "black")
plotwithoutlegend[1,4]<-plotwithoutlegend[1,4]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.15   ")), size = 2.7, color = "black")
plotwithoutlegend[2,3]<-plotwithoutlegend[2,3]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.88   ")), size = 2.7, color = "black")
plotwithoutlegend[2,4]<-plotwithoutlegend[2,4]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.68   ")), size = 2.7, color = "black")
plotwithoutlegend[3,4]<-plotwithoutlegend[3,4]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.90   ")), size = 2.7, color = "black")

plotwithoutlegend[2,1]<-A_plotwithoutlegend[2,1]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.63   ")), size = 2.7, color = "black")
plotwithoutlegend[3,1]<-A_plotwithoutlegend[3,1]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.55   ")), size = 2.7, color = "black")
plotwithoutlegend[3,2]<-A_plotwithoutlegend[3,2]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.98   ")), size = 2.7, color = "black")
plotwithoutlegend[4,1]<-A_plotwithoutlegend[4,1]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.49   ")), size = 2.7, color = "black")
plotwithoutlegend[4,2]<-A_plotwithoutlegend[4,2]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.95   ")), size = 2.7, color = "black")
plotwithoutlegend[4,3]<-A_plotwithoutlegend[4,3]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.99   ")), size = 2.7, color = "black")


plotwithoutlegend[1,1]<-ggplot() +geom_density(data=plotdata, aes(x=Rich_k, color = "IARC"), linewidth=0.8) +
  geom_density(data=A_plotdata, aes(x=Rich_k, color = "LMIC"), linewidth = 0.8) +theme(panel.background = element_rect(fill = "#F8F3F2"))+
  scale_color_manual(name=NULL,
                     values = c( "IARC" = "#003916", "LMIC" = "slateblue4"),
                     labels = c("EPIC", "LMIC"))+theme(legend.position= c(0.85, 0.87), legend.key.size=unit(0.4, "cm"), legend.text=element_text(size=8))

plotwithoutlegend[2,2]<-ggplot() +geom_density(data=plotdata, aes(x=Hill1_k, color = "IARC"),  linewidth=0.8) +
  geom_density(data=A_plotdata, aes(x=Hill1_k, color = "LMIC"), linewidth=0.8) +theme(panel.background = element_rect(fill = "#F8F3F2"))+
  scale_color_manual(name=NULL,
                     values = c( "IARC" = "#003916", "LMIC" = "slateblue4"),
                     labels = c("EPIC", "LMIC"))+theme(legend.position= c(0.85, 0.87), legend.key.size=unit(0.4, "cm"), legend.text=element_text(size=8))

plotwithoutlegend[3,3]<-ggplot() +geom_density(data=plotdata, aes(x=Hill2_k, color = "IARC"), linewidth=0.8) +
  geom_density(data=A_plotdata, aes(x=Hill2_k, color = "LMIC"), linewidth=0.8) +theme(panel.background = element_rect(fill = "#F8F3F2"))+
  scale_color_manual(name = NULL,
                     values = c( "IARC" = "#003916", "LMIC" = "slateblue4"),
                     labels = c("EPIC", "LMIC"))+theme(legend.position= c(0.85, 0.87), legend.key.size=unit(0.4, "cm"), legend.text=element_text(size=8))

plotwithoutlegend[4,4]<-ggplot() +geom_density(data=plotdata, aes(x=Hillinf_k, color = "IARC"), linewidth=0.8) +
  geom_density(data=A_plotdata, aes(x=Hillinf_k, color = "LMIC"), linewidth=0.8) +theme(panel.background = element_rect(fill = "#F8F3F2"))+
  scale_color_manual(name = NULL,
                     values = c( "IARC" = "#003916", "LMIC" = "slateblue4"),
                     labels = c("EPIC", "LMIC"))+theme(legend.position= c(0.85, 0.87), legend.key.size=unit(0.4, "cm"), legend.text=element_text(size=8))

#"#656839", "black", "#514B23", "#BDDBD0", "#B6CB9E", "#A8763E", "#F1E8B8", "#009238", "#337CA0", "#D4D6B9", "#CBC9AD", "#1E555C", "#FDC149", "#ECC3C1"

#add legend by making one graph to grab legend
auxplot<-ggplot(plotdata, aes(x=Hill2_k, Hillinf_k))+geom_bin2d(bins=150)+
  scale_fill_gradientn(
    colours= c("#F4EBE1", "#D2B7B1","#009238","#003916", "black"),
    values= c(0,0.035,0.17,0.38,0.6,1),     
    limits=c(0,3000))+
  theme_bw() + labs(fill = "Number of \npeople")

A_auxplot<-ggplot(A_plotdata, aes(x=Hill2_k, Hillinf_k))+geom_bin2d(bins=150)+
  scale_fill_gradientn(
    colours= c("#F4EBE1", "#D2B7B1","slateblue1","slateblue4", "black"),
    values= c(0,0.035,0.17,0.38,0.6,1),     
    limits=c(0,50))+
  theme_bw() + labs(fill = "Number of \npeople")


mylegend<-grab_legend(auxplot)
A_mylegend<-grab_legend(A_auxplot)
g <- grid.grabExpr(print(plotwithoutlegend))


#combine legend and the graphs
ga<-grid.arrange(A_mylegend,g, mylegend, widths=c(0.08,0.84,0.08))
ggsave(ga,filename = file.path("ExtraData/EDplots/U_pairsEnergy.png"), width=10, height=7, dpi=450)

############################# Figure 2: ggpairs weight ########################

plotdata<-ck[,c("Rich_g","Hill1_g","Hill2_g","Hillinf_g")]
A_plotdata<-A_indices_gram[, c("A_Rich_g", "A_Hill1_g", "A_Hill2_g", "A_Hillinf_g")] #select data
colnames(A_plotdata)<-c("Rich_g", "Hill1_g", "Hill2_g", "Hillinf_g")
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


A_my_ggally <- function(data, mapping){
  ggplot(data,mapping) +
    geom_bin2d(bins=150)+
    scale_fill_gradientn(
      colours= c("#F4EBE1", "#D2B7B1","slateblue1", "slateblue4", "black"),
      values= c(0,0.035,0.17,0.38,0.6,1),     
      limits=c(0,50))+
    theme_bw()}
#"#D2B7B1"


# plot the data
plotwithoutlegend<-ggpairs(
  plotdata[],
  aes(),
  #lower = "blank",
  upper = list(continuous = my_ggally),
  #diag = list(continuous = my_diag),
  diag = "blank",
  lower = list(continuous = wrap("cor", method = "spearman")),
  columnLabels = c('Hill["0,g/day"]', 'Hill["1,g/day"]', 'Hill["2,g/day"]', 'Hill["\U221E,g/day"]'),
  labeller = label_parsed
) 

A_plotwithoutlegend<-ggpairs(
  A_plotdata[],
  aes(),
  #lower = "blank",
  lower = list(continuous = A_my_ggally),
  diag= "blank",
  #diag = list(continuous = my_diag),
  upper = list(continuous = wrap("cor", method = "spearman")),
  columnLabels = c('Hill["0,g/day"]', 'Hill["1,g/day"]', 'Hill["2,g/day"]', 'Hill["\U221E,g/day"]'),
  labeller = label_parsed
) 

plotwithoutlegend<-plotwithoutlegend+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(panel.border = element_blank())+ 
  theme(axis.text= element_blank(), axis.ticks = element_blank(), strip.background = element_rect(fill = "#003916"), strip.text=element_text(color="white"))
#theme(strip.text.x = element_text(size = 14), strip.text.y = element_text(size = 14)) #remove borders and grid

plotwithoutlegend[1,1]<-ggplot() +geom_density(data=plotdata, aes(x=Rich_g, color = "IARC"), linewidth=0.8) +
  geom_density(data=A_plotdata, aes(x=Rich_g, color = "LMIC"), linewidth = 0.8) +theme(panel.background = element_rect(fill = "#F8F3F2"))+
  scale_color_manual(name=NULL,
                     values = c( "IARC" = "#003916", "LMIC" = "slateblue4"),
                     labels = c("EPIC", "LMIC"))+theme(legend.position= c(0.85, 0.87), legend.key.size=unit(0.4, "cm"), legend.text=element_text(size=8))

plotwithoutlegend[2,2]<-ggplot() +geom_density(data=plotdata, aes(x=Hill1_g, color = "IARC"),  linewidth=0.8) +
  geom_density(data=A_plotdata, aes(x=Hill1_g, color = "LMIC"), linewidth=0.8) +theme(panel.background = element_rect(fill = "#F8F3F2"))+
  scale_color_manual(name=NULL,
                     values = c( "IARC" = "#003916", "LMIC" = "slateblue4"),
                     labels = c("EPIC", "LMIC"))+theme(legend.position= c(0.85, 0.87), legend.key.size=unit(0.4, "cm"), legend.text=element_text(size=8))

plotwithoutlegend[3,3]<-ggplot() +geom_density(data=plotdata, aes(x=Hill2_g, color = "IARC"), linewidth=0.8) +
  geom_density(data=A_plotdata, aes(x=Hill2_g, color = "LMIC"), linewidth=0.8) +theme(panel.background = element_rect(fill = "#F8F3F2"))+
  scale_color_manual(name=NULL,
                     values = c( "IARC" = "#003916", "LMIC" = "slateblue4"),
                     labels = c("EPIC", "LMIC"))+theme(legend.position= c(0.85, 0.87), legend.key.size=unit(0.4, "cm"), legend.text=element_text(size=8))

plotwithoutlegend[4,4]<-ggplot() +geom_density(data=plotdata, aes(x=Hillinf_g, color = "IARC"), linewidth=0.8) +
  geom_density(data=A_plotdata, aes(x=Hillinf_g, color = "LMIC"), linewidth=0.8) +theme(panel.background = element_rect(fill = "#F8F3F2"))+
  scale_color_manual(name = NULL,
                     values = c( "IARC" = "#003916", "LMIC" = "slateblue4"),
                     labels = c("EPIC", "LMIC"))+theme(legend.position= c(0.85, 0.87), legend.key.size=unit(0.4, "cm"), legend.text=element_text(size=8))


plotwithoutlegend[1,2]<-plotwithoutlegend[1,2]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.22   ")), size = 2.7, color = "black")
plotwithoutlegend[1,3]<-plotwithoutlegend[1,3]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.06   ")), size = 2.7, color = "black")
plotwithoutlegend[1,4]<-plotwithoutlegend[1,4]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.04   ")), size = 2.7, color = "black")
plotwithoutlegend[2,3]<-plotwithoutlegend[2,3]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.93   ")), size = 2.7, color = "black")
plotwithoutlegend[2,4]<-plotwithoutlegend[2,4]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.79   ")), size = 2.7, color = "black")
plotwithoutlegend[3,4]<-plotwithoutlegend[3,4]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.93   ")), size = 2.7, color = "black")

plotwithoutlegend[2,1]<-A_plotwithoutlegend[2,1]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.75   ")), size = 2.7, color = "black")
plotwithoutlegend[3,1]<-A_plotwithoutlegend[3,1]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.62   ")), size = 2.7, color = "black")
plotwithoutlegend[3,2]<-A_plotwithoutlegend[3,2]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.96   ")), size = 2.7, color = "black")
plotwithoutlegend[4,1]<-A_plotwithoutlegend[4,1]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.51   ")), size = 2.7, color = "black")
plotwithoutlegend[4,2]<-A_plotwithoutlegend[4,2]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.88   ")), size = 2.7, color = "black")
plotwithoutlegend[4,3]<-A_plotwithoutlegend[4,3]+annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = expression(paste(italic("\u03C1"), "=0.97   ")), size = 2.7, color = "black")

#"#656839", "black", "#514B23", "#BDDBD0", "#B6CB9E", "#A8763E", "#F1E8B8", "#009238", "#337CA0", "#D4D6B9", "#CBC9AD", "#1E555C", "#FDC149", "#ECC3C1"

#add legend by making one graph to grab legend
auxplot<-ggplot(plotdata, aes(x=Hill2_g, Hillinf_g))+geom_bin2d(bins=150)+
  scale_fill_gradientn(
    colours= c("#F4EBE1", "#D2B7B1","#009238","#003916", "black"),
    values= c(0,0.035,0.17,0.38,0.6,1),     
    limits=c(0,3000))+
  theme_bw() + labs(fill = "Number of \npeople")

A_auxplot<-ggplot(A_plotdata, aes(x=Hill2_g, Hillinf_g))+geom_bin2d(bins=150)+
  scale_fill_gradientn(
    colours= c("#F4EBE1", "#D2B7B1","slateblue1","slateblue4", "black"),
    values= c(0,0.035,0.17,0.38,0.6,1),     
    limits=c(0,50))+
  theme_bw() + labs(fill = "Number of \npeople")


mylegend<-grab_legend(auxplot)
A_mylegend<-grab_legend(A_auxplot)
g <- grid.grabExpr(print(plotwithoutlegend))


#combine legend and the graphs
ga<-grid.arrange(A_mylegend,g, mylegend, widths=c(0.08,0.84,0.08))
ggsave(ga,filename = file.path("ExtraData/EDplots/U_pairsWeight.png"), width=10, height=7, dpi=450)




####################### in function of total consumption ############################

#EPIC
dataTOTlong_g<-ck%>%select(c(Tot_g, Rich_g, Hill1_g, Hill2_g, Hillinf_g))%>%
  mutate(Hill0_g=Rich_g, .keep = "unused")%>%
  pivot_longer(
    cols = contains("i"),
    names_to = "Index",
    values_to = "Value"
  )

mylab <- as_labeller(c(
  Hill0_g = 'Hill["0,g/day"]',
  Hill1_g = 'Hill["1,g/day"]',
  Hill2_g = 'Hill["2,g/day"]',
  Hillinf_g = paste('Hill["\U221E,g/day"]')
),   default = label_parsed)

pTOTg<-ggplot(dataTOTlong_g, aes(Tot_g, Value))+geom_bin2d(bins=150)+facet_wrap(~Index, scales="free", ncol=1, labeller=mylab)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(panel.border = element_blank())+ #remove borders and grid
  theme(legend.text = element_text(size = 11),
        axis.line.x.bottom = element_line(colour = "black"),
        axis.line.y.left = element_line(colour = "black"),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 13),
        legend.title = element_text(size=11),
        strip.background = element_rect(fill = "#003916"), 
        strip.text=element_text(color="white"))+
  scale_fill_gradientn(
    colours = c("#F4EBE1", "#D2B7B1", "#009238", "#003916", "black"),
    values = c(0, 0.035, 0.17, 0.38, 0.6, 1),
    limits = c(0, 1500)
  ) +labs(x="Total cons (g/day)", y=NULL, title="EPIC", fill = "Number of \npeople") 

pTOTgwithoutlegend<-ggplot(dataTOTlong_g, aes(Tot_g, Value))+geom_bin2d(bins=150)+facet_wrap(~Index, scales="free", ncol=1, labeller=mylab)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(panel.border = element_blank())+ #remove borders and grid
  theme(legend.position = "none",
        axis.line.x.bottom = element_line(colour = "black"),
        axis.line.y.left = element_line(colour = "black"),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 13),
        strip.background = element_rect(fill = "#003916"), 
        strip.text=element_text(color="white"))+
  scale_fill_gradientn(
    colours = c("#F4EBE1", "#D2B7B1", "#009238", "#003916", "black"),
    values = c(0, 0.035, 0.17, 0.38, 0.6, 1),
    limits = c(0, 1500)
  ) +labs(x="Weight intake (g/day)", y=NULL, title="EPIC") 

dataTOTlong_k<-ck%>%select(c(Tot_k, Rich_k, Hill1_k, Hill2_k, Hillinf_k))%>%
  mutate(Hill0_k=Rich_k, .keep = "unused")%>%
  pivot_longer(
    cols = contains("i"),
    names_to = "Index",
    values_to = "Value"
  )

mylab <- as_labeller(c(
  Hill0_k = 'Hill["0,kcal/day"]',
  Hill1_k = 'Hill["1,kcal/day"]',
  Hill2_k = 'Hill["2,kcal/day"]',
  Hillinf_k = paste('Hill["\U221E,kcal/day"]')
),   default = label_parsed)

pTOTk_withoutlegend<-ggplot(dataTOTlong_k, aes(Tot_k, Value))+geom_bin2d(bins=150)+facet_wrap(~Index, scales="free", ncol=1, labeller=mylab)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(panel.border = element_blank())+ #remove borders and grid
  theme(legend.position = "none",
        axis.line.x.bottom = element_line(colour = "black"),
        axis.line.y.left = element_line(colour = "black"),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 13),
        strip.background = element_rect(fill = "#003916"), 
        strip.text=element_text(color="white"))+
  scale_fill_gradientn(
    colours = c("#F4EBE1", "#D2B7B1", "#009238", "#003916", "black"),
    values = c(0, 0.035, 0.17, 0.38, 0.6, 1),
    limits = c(0, 1500)
  ) +labs(x="Energy intake (kcal/day)", y=NULL, title="EPIC") 


mylegend<-grab_legend(pTOTg)
gg <- grid.grabExpr(print(pTOTgwithoutlegend))
gk <- grid.grabExpr(print(pTOTk_withoutlegend))

#LMIC

data<-as.data.frame(fread('ExtraData/biodiversity data file clean.csv', stringsAsFactors = FALSE))

A_totg<-data%>%
  select(id, speciesnr, country, MAR,foodweight,childadult)%>%
  filter(speciesnr!="")%>%
  filter(childadult=="adult")%>%
  mutate(idname=paste(country,id,sep=""), .keep="all")%>%
  filter(!is.na(MAR))%>%
  group_by(idname)%>%
  summarise(Tot_g=sum(foodweight))

A_dataTOTlong_g<-inner_join(A_indices_gram%>%select(c(idname,A_Rich_g, A_Hill1_g, A_Hill2_g, A_Hillinf_g))%>%
                              mutate(A_Hill0_g=A_Rich_g, .keep = "unused")%>%
                              pivot_longer(
                                cols = contains("_g"),
                                names_to = "Index",
                                values_to = "Value"
                              ), A_totg, by="idname")

mylab <- as_labeller(c(
  A_Hill0_g = 'Hill["0,g/day"]',
  A_Hill1_g = 'Hill["1,g/day"]',
  A_Hill2_g = 'Hill["2,g/day"]',
  A_Hillinf_g = paste('Hill["\U221E,g/day"]')
),   default = label_parsed)

pTOTg<-ggplot(A_dataTOTlong_g, aes(Tot_g, Value))+geom_bin2d(bins=150)+facet_wrap(~Index, scales="free", ncol=1, labeller=mylab)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(panel.border = element_blank())+ #remove borders and grid
  theme(legend.text = element_text(size = 11),
        axis.line.x.bottom = element_line(colour = "black"),
        axis.line.y.left = element_line(colour = "black"),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 13),
        legend.title = element_text(size=11),
        strip.background = element_rect(fill = "slateblue4"), 
        strip.text=element_text(color="white"))+
  scale_fill_gradientn(
    colours = c("#F4EBE1", "#D2B7B1", "slateblue1", "slateblue4", "black"),
    values = c(0, 0.035, 0.17, 0.38, 0.6, 1),
    limits = c(0, 30)
  ) +labs(fill = "Number of \npeople")

pTOTgwithoutlegend<-ggplot(A_dataTOTlong_g, aes(Tot_g, Value))+geom_bin2d(bins=150)+facet_wrap(~Index, scales="free", ncol=1, labeller=mylab)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(panel.border = element_blank())+ #remove borders and grid
  theme(legend.position = "none",
        axis.line.x.bottom = element_line(colour = "black"),
        axis.line.y.left = element_line(colour = "black"),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 13),
        strip.background = element_rect(fill = "slateblue4"), 
        strip.text=element_text(color="white"))+
  scale_fill_gradientn(
    colours = c("#F4EBE1", "slateblue1", "slateblue4", "#003916", "black"),
    values = c(0, 0.035, 0.17, 0.38, 0.6, 1),
    limits = c(0, 30)
  ) +labs(x="Weight intake (g/day)", y="Number of effective species", title="LMIC", fill = "Number of \npeople") 


A_totk<-data%>%
  select(id, country, MAR, speciesnr, energy_kcal,childadult)%>%
  filter(speciesnr!="")%>%
  filter(childadult=="adult")%>%
  mutate(idname=paste(country,id,sep=""), .keep="all")%>%
  filter(!is.na(MAR))%>%
  group_by(idname)%>%
  summarise(Tot_k=sum(energy_kcal))

A_dataTOTlong_k<-inner_join(A_indices_kcal%>%select(c(idname,A_Rich_k, A_Hill1_k, A_Hill2_k, A_Hillinf_k))%>%
                              mutate(A_Hill0_k=A_Rich_k, .keep = "unused")%>%
                              pivot_longer(
                                cols = contains("_k"),
                                names_to = "Index",
                                values_to = "Value"
                              ), A_totk, by="idname")

mylab <- as_labeller(c(
  A_Hill0_k = 'Hill["0,kcal/day"]',
  A_Hill1_k = 'Hill["1,kcal/day"]',
  A_Hill2_k = 'Hill["2,kcal/day"]',
  A_Hillinf_k = paste('Hill["\U221E,kcal/day"]')
),   default = label_parsed)

pTOTk_withoutlegend<-ggplot(A_dataTOTlong_k, aes(Tot_k, Value))+geom_bin2d(bins=150)+facet_wrap(~Index, scales="free", ncol=1, labeller=mylab)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(panel.border = element_blank())+ #remove borders and grid
  theme(legend.position = "none",
        axis.line.x.bottom = element_line(colour = "black"),
        axis.line.y.left = element_line(colour = "black"),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 13),
        strip.background = element_rect(fill = "slateblue4"), 
        strip.text=element_text(color="white"))+
  scale_fill_gradientn(
    colours = c("#F4EBE1", "#D2B7B1", "slateblue1", "slateblue4", "black"),
    values = c(0, 0.035, 0.17, 0.38, 0.6, 1),
    limits = c(0, 30)
  )+labs(x="Energy intake (kcal/day)", y=NULL, title="LMIC", fill = "Number of \npeople") +xlim(0,8058)

A_mylegend<-grab_legend(pTOTg)
A_gg <- grid.grabExpr(print(pTOTgwithoutlegend))
A_gk <- grid.grabExpr(print(pTOTk_withoutlegend))


ga<-grid.arrange(A_mylegend,A_gg,A_gk,gg,gk,mylegend, widths=c(0.05,0.225,0.225,0.225,0.225,0.05))
ggsave(ga,filename = file.path("ExtraData/EDplots/U_tot.png"), width=18, height=10, dpi=450)

############################ weight versus kcal ####################################

#EPIC

selectHill0<-ck%>%
  select(c(Rich_g,Rich_k))%>%
  mutate(Index="Hill0")%>%
  mutate(weight=Rich_g, energy= Rich_k)%>%
  select(Index, weight, energy)
            
selectHill1<-ck%>%
  select(c(Hill1_g,Hill1_k))%>%
  mutate(Index="Hill1")%>%
  mutate(weight=Hill1_g, energy= Hill1_k)%>%
  select(Index, weight, energy)

selectHill2<-ck%>%
  select(c(Hill2_g,Hill2_k))%>%
  mutate(Index="Hill2")%>%
  mutate(weight=Hill2_g, energy= Hill2_k)%>%
  select(Index, weight, energy)

selectHillinf<-ck%>%
  select(c(Hillinf_g,Hillinf_k))%>%
  mutate(Index="Hillinf")%>%
  mutate(weight=Hillinf_g, energy= Hillinf_k)%>%
  select(Index, weight, energy)

combin<-rbind(selectHill0,selectHill1, selectHill2, selectHillinf)

mylab<-mylab <- as_labeller(c(
  Hill0 = 'Hill["0"]',
  Hill1 = 'Hill["1"]',
  Hill2 = 'Hill["2"]',
  Hillinf = paste('Hill["\U221E"]')
),   default = label_parsed)

cor_data <- combin %>%
  group_by(Index) %>%
  summarise(correlation = cor(weight, energy, method = "spearman"))

label_text <- sprintf("italic(ρ) == %.2f", cor_data$correlation)

gvsk<-ggplot(combin, aes(weight, energy))+facet_wrap(~Index, ncol=1, labeller=mylab, scales="free")+geom_bin2d(bins=150)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(panel.border = element_blank())+ #remove borders and grid
  theme(
        axis.line.x.bottom = element_line(colour = "black"),
        axis.line.y.left = element_line(colour = "black"),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 13),
        strip.background = element_rect(fill = "#003916"), 
        strip.text=element_text(color="white"))+
  scale_fill_gradientn(
    colours = c("#F4EBE1", "#D2B7B1", "#009238", "#003916", "black"),
    values = c(0, 0.035, 0.17, 0.38, 0.6, 1))+
  labs(x="Weight-based \n(number of effective species)", y="Energy-based \n(number of effective species)", title="EPIC", fill= "Number of \npeople")+
  geom_text(data = cor_data, aes(label = sprintf("ρ = %.2f", correlation)),
            x = Inf, y = -Inf, hjust = 1, vjust = -1, color = "black", size=3)


#LMIC

A_indices_gram_kcal<-inner_join(A_indices_gram, A_indices_kcal, by="idname")

A_selectHill0<-A_indices_gram_kcal%>%
  select(c(A_Rich_g,A_Rich_k))%>%
  mutate(Index="Hill0")%>%
  mutate(weight=A_Rich_g, energy= A_Rich_k)%>%
  select(Index, weight, energy)

A_selectHill1<-A_indices_gram_kcal%>%
  select(c(A_Hill1_g,A_Hill1_k))%>%
  mutate(Index="Hill1")%>%
  mutate(weight=A_Hill1_g, energy= A_Hill1_k)%>%
  select(Index, weight, energy)

A_selectHill2<-A_indices_gram_kcal%>%
  select(c(A_Hill2_g,A_Hill2_k))%>%
  mutate(Index="Hill2")%>%
  mutate(weight=A_Hill2_g, energy= A_Hill2_k)%>%
  select(Index, weight, energy)

A_selectHillinf<-A_indices_gram_kcal%>%
  select(c(A_Hillinf_g,A_Hillinf_k))%>%
  mutate(Index="Hillinf")%>%
  mutate(weight=A_Hillinf_g, energy= A_Hillinf_k)%>%
  select(Index, weight, energy)

A_combin<-rbind(A_selectHill0,A_selectHill1, A_selectHill2, A_selectHillinf)

mylab<-mylab <- as_labeller(c(
  Hill0 = 'Hill["0"]',
  Hill1 = 'Hill["1"]',
  Hill2 = 'Hill["2"]',
  Hillinf = paste('Hill["\U221E"]')
),   default = label_parsed)

A_cor_data <- A_combin %>%
  group_by(Index) %>%
  summarise(correlation = cor(weight, energy, method = "spearman"))

A_gvsk<-ggplot(A_combin, aes(weight, energy))+facet_wrap(~Index, ncol=1, labeller=mylab, scales="free")+geom_bin2d(bins=150)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(panel.border = element_blank())+ #remove borders and grid
  theme(legend.position = "left",
    axis.line.x.bottom = element_line(colour = "black"),
    axis.line.y.left = element_line(colour = "black"),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 13),
    strip.background = element_rect(fill = "slateblue4"), 
    strip.text=element_text(color="white"))+
  scale_fill_gradientn(
    colours = c("#F4EBE1", "#D2B7B1", "slateblue1", "slateblue4", "black"),
    values = c(0, 0.035, 0.17, 0.38, 0.6, 1))+
  labs(x="Weight-based \n(number of effective species)", y="Energy-based \n(number of effective species)", fill="Number of \npeople", title="LMIC")+
  geom_text(data = A_cor_data, aes(label = sprintf("ρ = %.2f", correlation)),
            x = Inf, y = -Inf, hjust = 1, vjust = -1, color = "black", size=3)

ga<-grid.arrange(A_gvsk, gvsk,  nrow=1)


ggsave(ga, filename=file.path("ExtraData/EDplots/gram_vs_kcal.png"), dpi=400, width=8, height=6)


