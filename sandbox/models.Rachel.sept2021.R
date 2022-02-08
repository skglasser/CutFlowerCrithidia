

#fill in your working directory 
setwd("C:/Users/Zonj/Documents/Universites_Research/UMass/cut flower 2021/data")
library(vegan)#general eco stats functions
library(MASS)#glm.nb model comes from this package.
library(ggplot2)#this package has pretty plots #cheatsheets, print
library(car)#Anova () 
library(dplyr)#piping code for data visualiation and organization #cheatsheets, print
library(emmeans)#emmeans aka ls-means, #https://aosmith.rbind.io/2019/03/25/getting-started-with-emmeans/


#calling in data
data <- read.csv("Crithidia counts PA.csv", header = TRUE, sep = ",")
#looking at column names
colnames(data)
#summary of data
summary(data)

#data filtered to exclude "NAs" in the 'Species' and 'PA' columns. 
b<-data%>%filter(Species !="NA", PA !="NA")# in the end we did not use the binomial so the PA variable is not considered here.
summary(b)

#initial data exploration
boxplot(Count~Farm, data=b)#relation of crith count to Farm
boxplot(Count~Species, data=b)#relation of crith count to bee spp. 
hist(b$Count)#spread of count data. 
##We have A LOT of zeros therefore we will need to use a negative binomial distribution in our model. 

#plots for data exploration
#MAKE VERY CLEAR IN PRESENTATION that these are calculated from raw data means not from the model. 

#Species 
spp.means<- b%>%group_by(Species)%>%summarise(Crithidia_mean = mean(Count), n=n(), SE=sd(Count)/sqrt(n()))


##RACHEL
#plot of the mean of crith count for Bombus 'Species', raw data
ggplot(spp.means, aes(x=Species, y=Crithidia_mean))+ 
  geom_bar(stat="identity", 
           width=0.5,
           position=position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin=Crithidia_mean+SE, ymax=Crithidia_mean-SE), width=.3,position=position_dodge(.9))+
  theme_classic() + 
  theme(panel.grid = element_blank(), panel.border = element_blank(), 
        title = element_text(size=23,face="bold"), 
        axis.title = element_text(size=25,face="bold"), 
        axis.text.x= element_text(face="bold", size=22), 
        axis.text.y = element_text(face="bold", size=18)) +
  ggtitle("Mean Crithidia cell counts \n in Bombus spp.") +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Mean value cell counts per 0.02 ml") + 
  xlab("Bombus spp.") #YOU CAN ADD COLOR#
ggsave("raw.spp.plot.jpg", width = 16, height = 8)


#Farms
farm.means<- b%>%group_by(Farm)%>%summarise(Crithidia_mean = mean(Count), n=n(), SE=sd(Count)/sqrt(n()))
#giving order to the variable 'Farm' first the 3 control farms, then the 3 treament farms
farm.means$Farm<-factor(farm.means$Farm,levels=c("GOLO","RCON","SIMP","LASA","RCUT","LAUR"))

##RACHEL
#plot of the mean of crith count for each 'Farm', raw data
ggplot(farm.means, aes(x=Farm, y=Crithidia_mean))+ 
  geom_bar(stat="identity", 
           width=0.5,
           position=position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin=Crithidia_mean+SE, ymax=Crithidia_mean-SE), width=.3,position=position_dodge(.9))+
  theme_classic() + 
  theme(panel.grid = element_blank(), panel.border = element_blank(), 
        title = element_text(size=23,face="bold"), 
        axis.title = element_text(size=25,face="bold"), 
        axis.text.x= element_text(face="bold", size=22), 
        axis.text.y = element_text(face="bold", size=18)) +
  ggtitle("Mean Crithidia cell counts \n in Farms") +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Mean value cell counts per 0.02 ml") + 
  xlab("Farms")#YOU CAN ADD COLOR, make control vs. treatment.
ggsave("raw.farm.plot.jpg", width = 16, height = 8)

###NOT USING######
#binomial model, just looked at presence '1' vs. absence '0'of crithidia. Because this is a 
model.bi<-glm(PA~Farm*Species, family=binomial, data=b)
Anova(model.bi)
plot(model.bi)
AIC(model.bi)

###NOT USING####
#subset count model
sb<-b%>%filter(PA!="0")
model.sb<-glm(Count~Farm*Species, data=sb)
Anova(model.sb)
plot(model.sb)
AIC(model.sb)

####################################################################
#####ThIS IS THE MODEL WE WILL USE########
#total count model, negative binomial #https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/
model.nb <- glm.nb(Count~Farm*Species,data=b)
Anova(model.nb)
summary(model.nb)#sig dif group variance based on main effect of farm and main effect of species but not their interaction.
plot(model.nb)
AIC(model.nb)#checking the fit model, does not actually look that great
#################################################################

AIC(model.bi, model.sb, model.nb) #our model is the worst model, but the simplest for this exercise. 

############ESTIMATED MARGINAL MEANS CALCULATED FROM THE model.nb##########################
#emmeans or 'LS-means' #https://aosmith.rbind.io/2019/03/25/getting-started-with-emmeans/

#emmeans of the effect of 'Species' on crithdia count, this does not work because there are a lot of NAs.i cannot figure this out. 
emmCountSpp<-emmeans(model.nb, specs = pairwise ~ Species)
#extract emmeans dataframe from emmCountSpp list
emmCsppmean<-emmCountSpp$emmeans%>%
  as.data.frame()
#extract constrasts dataframe from emmCountSpp list
emmCsppcon<-emmCountSpp$contrasts%>%
  as.data.frame()

#emmeans of the effect of 'Farm' on crithdia count,  this does not work because there are a lot of NAs. i cannot figure this out. 
emmCountF<-emmeans(model.nb, specs = pairwise ~ Farm, type = "response")
#extract emmeans dataframe from emmCountF list
emmCFmean<-emmCountF$emmeans%>%
  as.data.frame()
#extract constrasts dataframe from emmCountF list
emmCFcon<-emmCountF$contrasts%>%
  as.data.frame()

############USING THE INTERACTION OF VARIABLES FOR GRAPHING PURPOSES########
#looking at the interaction of 'Species:Farm' is better than looking at the main effects alone... which give us a lot of NA values. 
emmCountINT<-emmeans(model.nb, specs = pairwise ~ Species:Farm, type = "response")
#this makes a list of 2 dataframes which are 1. emmeans and 2. the contrast between variables.

#extract emmeans dataframe from emmCountINT list
emmCmeanINT<-emmCountINT$emmeans%>%
  as.data.frame()

#extract constrasts dataframe from emmCountINT list
emmCconINT<-emmCountINT$contrasts%>%
  as.data.frame()


####plotttttttt############### 
####we made this plot by using the emmeans calculated from the effect of the interaction of 'Species' and 'Farm'. 
####we plot the interaction because I could not figure out why the emmeans of the main effects individually were not working. sorry. 

#giving order to the variable 'Farm' first the 3 control farms, then the 3 treatment farms
emmCmeanINT$Farm<-factor(emmCmeanINT$Farm,levels=c("GOLO","RCON","SIMP","LASA","RCUT","LAUR"))


#making error bars only for the upper limit.
limits <- aes(ymax = response + SE, ymin = response)

##RACHEL
#plot, MAKE THE DISTINCTION BETWEEN control vs treatment in powerpoint. add full species name to legend 
ggplot(emmCmeanINT, aes(x=Farm, y=response, fill = Species))+ 
  geom_bar(stat="identity", 
           width=0.5,
           position=position_dodge(width = 0.5)) + 
  geom_errorbar(limits, width=0.05,
                position=position_dodge(width = 0.5)) + 
  theme_classic() + 
  theme(panel.grid = element_blank(), panel.border = element_blank(), 
        title = element_text(size=23,face="bold"), 
        axis.title = element_text(size=25,face="bold"), 
                                  axis.text.x= element_text(face="bold", size=22), 
                                  axis.text.y = element_text(face="bold", size=18), 
                                  legend.title = element_text(size = 25, face="bold") ,
        legend.text = element_text(size = 20, face = "bold")) +
  ggtitle("Effect of Farm on Parasite Counts \n in Bombus spp.") +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Cell counts per 0.02 ml") + 
  xlab("Farm") + 
  theme(legend.position="right") 

#saved plot to working directory, put me in powerpoint. 
ggsave("emm.INT.plot.jpg", width = 16, height = 8)

