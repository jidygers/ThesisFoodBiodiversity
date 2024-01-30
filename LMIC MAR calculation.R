library('ggplot2')
library('dplyr')
library('Hmisc')
library('tidyr')

#Formula's for confidence intervals
SE <- function(x) {
  sqrt(var(x) / length(x))
}

CI <- function(x) {
  qt(0.975, df = length(x) - 1) * SE(x)
}


#weight-based
A_indices_gram<-read.csv("clean scripts/A_indices_gram.csv", row.names=1)

#All countries
q_African_gram <- A_indices_gram %>%
  mutate(
    A_Hill0_gram = as.numeric(cut2(A_Rich_g, g = 4)),
    A_Hill1_gram = as.numeric(cut2(A_Hill1_g, g = 4)),
    A_Hill2_gram = as.numeric(cut2(A_Hill2_g, g = 4)),
    A_Hillinf_gram = as.numeric(cut2(A_Hillinf_g, g = 4))
  )

q_African_gram_long <- q_African_gram%>%
  pivot_longer(cols=c(-idname,-MAR,-country, -A_Rich_g, -A_Hill1_g, -A_Hill2_g, -A_Hillinf_g), values_to = "Quartile", names_to = "index")%>%
  group_by(index, Quartile)%>%
  summarise(meanIndex = mean(MAR), 
            lower_CI = meanIndex-CI(MAR),
            upper_CI = meanIndex+CI(MAR),
            CI=CI(MAR))

Data_Stata_gram<-data.frame(meanIndex=c(0, 7.12, 8.18, 12.1, 0, 5.61, 9.49, 13.7, 0, 5.65, 9.21, 12.6, 0, 5.55, 8.58, 11),
                            lower_CI =c(0, 5.84, 6.61, 10.4, 0, 4.21, 8.06, 12.2, 0, 4.24, 7.79, 11.1, 0, 4.12, 7.13, 9.53),
                            upper_CI =c(0, 8.42, 9.75, 13.8, 0, 7.01, 10.9, 15.3, 0, 7.06, 10.6, 14.1, 0, 6.97, 10, 12.6))

newdata<-cbind(q_African_gram_long[,1:2], Data_Stata_gram)

mylab <- as_labeller(c(
  A_Hill0_gram = 'Hill["0,g/day"]',
  A_Hill1_gram = 'Hill["1,g/day"]',
  A_Hill2_gram = 'Hill["2,g/day"]',
  A_Hillinf_gram = 'Hill["\U221E,g/day"]'),   default = label_parsed)

ggplot(newdata, aes(meanIndex,Quartile))+
  facet_wrap(~index, labeller = "mylab")+geom_point()+geom_errorbarh(aes(xmin = lower_CI, xmax = upper_CI, height = 0.00000000002))+
  scale_y_reverse()+
  geom_hline(aes(yintercept = 2), linewidth=11, color="#F8F3F2", alpha=1)+
  geom_hline(aes(yintercept = 4), linewidth=11, color="#F8F3F2", alpha=1)+
  geom_vline(xintercept = 0, linewidth=0.3, linetype = "dashed")+
  geom_point(colour="slateblue4", shape=15, size=2)+
  geom_errorbarh(aes(xmin = lower_CI, xmax = upper_CI, height = 0.00000000002), colour="slateblue4")+
  theme_bw() +
  theme(strip.text.x = element_text(size = 11), 
        strip.text.y = element_text(size = 11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x.bottom = element_line(colour = "black"), 
        strip.background = element_rect(fill = "slateblue4", color = "slateblue4"), 
        strip.text = element_text(size= 14, color="white"))+
  labs(x="MAR (pp)", y="Quartile")

ggsave(file.path("ExtraData/EDplots/MARoff_gram.png"),dpi=400)




#Energy-based

A_indices_kcal<-read.csv("clean scripts/A_indices_kcal.csv",row.names=1)

q_African_kcal <- A_indices_kcal %>%
  mutate(
    A_Hill0_kcal = as.numeric(cut2(A_Rich_k, g = 4)),
    A_Hill1_kcal = as.numeric(cut2(A_Hill1_k, g = 4)),
    A_Hill2_kcal = as.numeric(cut2(A_Hill2_k, g = 4)),
    A_Hillinf_kcal = as.numeric(cut2(A_Hillinf_k, g = 4))
  )

q_African_kcal_long <- q_African_kcal%>%
  pivot_longer(cols=c(-idname,-MAR,-country, -A_Rich_k, -A_Hill1_k, -A_Hill2_k, -A_Hillinf_k), values_to = "Quartile", names_to = "index")%>%
  group_by(index, Quartile)%>%
  summarise(meanIndex = mean(MAR), 
            lower_CI = meanIndex-CI(MAR),
            upper_CI = meanIndex+CI(MAR),
            CI=CI(MAR))

mylab <- as_labeller(c(
  A_Hill0_kcal = 'Hill["0,kcal/day"]',
  A_Hill1_kcal = 'Hill["1,kcal/day"]',
  A_Hill2_kcal = 'Hill["2,kcal/day"]',
  A_Hillinf_kcal = 'Hill["\U221E,kcal/day"]'),   default = label_parsed)



ggplot(q_African_kcal_long, aes(meanIndex,Quartile))+
  facet_wrap(~index, labeller = "mylab")+geom_point()+geom_errorbarh(aes(xmin = lower_CI, xmax = upper_CI, height = 0.00000000002))+
  scale_y_reverse()+
  geom_hline(aes(yintercept = 2), linewidth=11, color="#F8F3F2", alpha=1)+
  geom_hline(aes(yintercept = 4), linewidth=11, color="#F8F3F2", alpha=1)+
  geom_point(colour="slateblue4", shape=15, size=2)+
  geom_errorbarh(aes(xmin = lower_CI, xmax = upper_CI, height = 0.00000000002), colour="slateblue4")+
  theme_bw() +
  theme(strip.text.x = element_text(size = 11), 
        strip.text.y = element_text(size = 11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x.bottom = element_line(colour = "black"), 
        strip.background = element_rect(fill = "slateblue4", color = "slateblue4"), 
        strip.text = element_text(size= 14, color="white"))+
  labs(x="MAR (pp)", y="Quartile")


#Data calculated in Stata:
Data_Stata_kcal<-data.frame(meanIndex=c(0, 7.17, 8.07, 12.1, 0, 4.69, 7.73, 12.2, 0, 3.42, 5.85, 9.11, 0, 3.21, 6.25, 8.58),
                            lower_CI =c(0, 5.88, 6.49, 10.4, 0, 3.19, 6.05, 10.3, 0, 1.9, 4.12, 7.12, 0, 1.68, 4.47, 6.59),
                            upper_CI =c(0, 8.46, 9.64, 13.7, 0, 6.19, 9.41, 14.1, 0, 4.93, 7.59, 11.1, 0, 4.74, 8.03, 10.6))

newdata<-cbind(q_African_kcal_long[,1:2], Data_Stata_kcal)

ggplot(newdata, aes(meanIndex,Quartile))+
  facet_wrap(~index, labeller = "mylab")+geom_point()+geom_errorbarh(aes(xmin = lower_CI, xmax = upper_CI, height = 0.00000000002))+
  scale_y_reverse()+
  geom_hline(aes(yintercept = 2), linewidth=11, color="#F8F3F2", alpha=1)+
  geom_hline(aes(yintercept = 4), linewidth=11, color="#F8F3F2", alpha=1)+
  geom_vline(xintercept = 0, linewidth=0.3, linetype = "dashed")+
  geom_point(colour="slateblue4", shape=15, size=2)+
  geom_errorbarh(aes(xmin = lower_CI, xmax = upper_CI, height = 0.00000000002), colour="slateblue4")+
  theme_bw() +
  theme(strip.text.x = element_text(size = 11), 
        strip.text.y = element_text(size = 11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x.bottom = element_line(colour = "black"), 
        strip.background = element_rect(fill = "slateblue4", color = "slateblue4"), 
        strip.text = element_text(size= 14, color="white"))+
  labs(x="MAR (pp)", y="Quartile")


ggsave(file.path("ExtraData/EDplots/MARoff_kcal.png"),dpi=400)