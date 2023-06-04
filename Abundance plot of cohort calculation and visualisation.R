#load packages
library('dplyr')
library('ggplot2')
library('readxl')
library('sas7bdat')

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

################################ calculating mean and median per species ###############################

#selecting the species names and calculating the means and median per column, so per species
#absolute values
plotdata<-data.frame(species_kcal=colnames(kcal),
                     means_kcal=apply(kcal, 2, mean),
                     median_kcal=apply(kcal, 2, median),
                     species_gram=colnames(gram),
                     means_gram=apply(gram, 2, mean),
                     median_gram=apply(gram, 2, median)
)
#proportions
plotdatapi<-data.frame(species_kcal_pi=colnames(kcal_pi),
                       means_kcal_pi=apply(kcal_pi, 2, mean),
                       median_kcal_pi=apply(kcal_pi, 2, median),
                       species_gram_pi=colnames(gram_pi),
                       means_gram_pi=apply(gram_pi, 2, mean),
                       median_gram_pi=apply(gram_pi, 2, median)
)

#save data
write.csv(plotdata, file="data/abundance_plot.csv", row.names = FALSE)
write.csv(plotdatapi, file="data/abundance_plot_pi.csv", row.names = FALSE)

############################ Using the analysed data by IARC ##################################################
############################ Plotting graphs based on the analysed data #######################################
#read data
ap<-read.csv("resultaten IARC 1/abundance plot/abundance_plot.csv")
api<-read.csv("resultaten IARC 1/abundance plot/abundance_plot_pi.csv")

#changing the labels to the actual names of the species
xl1<-read_excel("resultaten IARC 1/Contents of the database.xlsx", sheet=2)
xl2<-as.data.frame(xl1[4:nrow(xl1),c(2,7)]) #selection of relevant columns
colnames(xl2)<-c("species_kcal", "name")
xl3kcal<-xl2[startsWith(xl2$species_kcal, "B_KCAL_"),] #selection of relevant rows
xl3kcal$name <- gsub("Energy for species: ",'',xl3kcal$name) #Delete "Energy for species"

plotdata <- merge(xl3kcal,ap,by="species_kcal")
colnames(xl3kcal)<-c("species_kcal_pi", "name")
plotdatapi<-merge(xl3kcal,api,by="species_kcal_pi")

#create a graph with the 20 most abundant species, weight-based
plotdatapi<-plotdatapi[order(plotdatapi$means_gram_pi, decreasing = TRUE),] #sort the data in decreasing order
plotdatapi$species_gram_pi <- factor(plotdatapi$species_gram_pi, levels = plotdatapi$species_gram_pi, labels=plotdatapi$name) #for graph layout
p3pi<-ggplot(plotdatapi[1:20,], aes(x=species_gram_pi, y=means_gram_pi))+geom_point()+theme_bw()+
  theme(axis.text.x = element_text(angle=90, hjust=1, size=10), 
        panel.border = element_blank(),
        axis.line.x.bottom = element_line(colour = "black"),
        axis.line.y.left = element_line(colour = "black"))+
  labs(x="Species", y="Average proportion")
ggsave(filename = file.path("Clean file/plots","abundance_weight.png"), p3pi, width=8, height=6)

#create a graph with the 20 most abundant species, energy-based
plotdatapi<-plotdatapi[order(plotdatapi$means_kcal_pi, decreasing = TRUE),] #sort the data in decreasing order
plotdatapi$species_kcal_pi <- factor(plotdatapi$species_kcal_pi, levels = plotdatapi$species_kcal_pi, labels=plotdatapi$name) #for graph layout
p1pi<-ggplot(plotdatapi[1:20,], aes(x=species_kcal_pi, y=means_kcal_pi))+geom_point()+ theme_bw()+
  theme(axis.text.x = element_text(angle=90, hjust=1, size=10), 
        panel.border = element_blank(),
        axis.line.x.bottom = element_line(colour = "black"),
        axis.line.y.left = element_line(colour = "black"))+
  labs(x="Species", y="Average proportion")
ggsave(filename = file.path("Clean file/plots","abundance_energy.png"), p1pi, width=8, height=6)


########################### Investigating Pareto principle s##################################
length(plotdatapi$means_gram_pi) #349

plotdatapi<-plotdatapi[order(plotdatapi$means_gram_pi, decreasing = TRUE),]
sum(plotdatapi$means_gram_pi[1:round(349*0.2)])
sum(plotdatapi$means_gram_pi[1:4])
sum(plotdatapi$means_gram_pi[1:20])

plotdatapi<-plotdatapi[order(plotdatapi$means_kcal_pi, decreasing = TRUE),]
sum(plotdatapi$means_kcal_pi[1:round(349*0.2)])
sum(plotdatapi$means_kcal_pi[1:4])
sum(plotdatapi$means_kcal_pi[1:20])

