#Calculate Alpha and Beta Diversity for Terrestrial invertebrates
#Load data
terrest_invert<-read.csv("data/Occurences(terrestrial_inv).csv",sep=";")



#Calculate alpha diversity presence only
library(tidyverse)
library(dplyr)

dat_TerrestInvert_red <-terrest_invert[,c("family","eventID")]
dat_TerrestInvert_red<-dat_TerrestInvert_red[-which(dat_TerrestInvert_red == ""),]
dat_TerrestInvert_red$presence <- 1
dat_TerrestInvert_red<-distinct(dat_TerrestInvert_red) #Remove cases where the same family was detected int he same event

dat_TerrestInvert_pa <- dat_TerrestInvert_red %>% 
  pivot_wider(names_from=family,values_from=c(presence))
list0 <- as.list(rep(0,ncol(dat_TerrestInvert_pa)))
names(list0) <- names(dat_TerrestInvert_pa)
dat_TerrestInvert_pa <- as.data.frame(dat_TerrestInvert_pa %>% replace_na(list0))
row.names(dat_TerrestInvert_pa) <- dat_TerrestInvert_pa$eventID
dat_TerrestInvert_pa <- dat_TerrestInvert_pa[,-1]

#Alpha diversity of each event 
rowSums(dat_TerrestInvert_pa)
NorthAlpha <-  mean(rowSums(dat_TerrestInvert_pa[grep("^N",rownames(dat_TerrestInvert_pa)),]))
SouthAlpha <- mean(rowSums(dat_TerrestInvert_pa[grep("^S",rownames(dat_TerrestInvert_pa)),]))
#Alpha diversity just considering the site
dat_TerrestInvertSites<- dat_TerrestInvert
dat_TerrestInvertSites[grep("^N",dat_TerrestInvertSites$eventID),2]<-"North"
dat_TerrestInvertSites[grep("^S",dat_TerrestInvertSites$eventID),2]<-"South"
dat_TerrestInvertSites$presence <- 1
dat_TerrestInvertSites<-distinct(dat_TerrestInvertSites) #Remove cases where the same family was detected int he same event
dat_TerrestInvertSites_pa <- dat_TerrestInvertSites %>% 
  pivot_wider(names_from=family,values_from=c(presence))
list01 <- as.list(rep(0,ncol(dat_TerrestInvertSites_pa)))
names(list01) <- names(dat_TerrestInvertSites_pa)
dat_TerrestInvertSites_pa <- as.data.frame(dat_TerrestInvertSites_pa %>% replace_na(list01))
row.names(dat_TerrestInvertSites_pa) <- dat_TerrestInvertSites_pa$eventID
dat_TerrestInvertSites_pa <- dat_TerrestInvertSites_pa[,-1]

AlphaDiversityTerrestInvert<- rowSums(dat_TerrestInvertSites_pa)
length(unique(dat_TerrestInvert[grep("^S",dat_TerrestInvert$eventID),2]))
length(unique(dat_TerrestInvert[grep("^N",dat_TerrestInvert$eventID),2]))
#Save the alpha diversity results
AlphaResultsTerrestialInvert<-data.frame(c(AlphaDiversityTerrestInvert,NorthAlpha,SouthAlpha))
colnames(AlphaResultsTerrestialInvert)<-"Alpha Diversity"
AlphaResultsTerrestialInvert$Site<-(c("Total South", "Total North", "Average North", "Average South"))
write.table(AlphaResultsTerrestialInvert,"results/AlphaDiveristyTerrestInvert.txt")

library(betapart)
#Beta diversity between both sites
Beta<-beta.pair(dat_TerrestInvertSites_pa)

#Calculate the abundance curve
dat_TerrestInvert_ab <-terrest_invert[,c("family","eventID","individualCount")]
dat_TerrestInvert_ab<-dat_TerrestInvert_ab[-which(dat_TerrestInvert_ab == ""),]
dat_TerrestInvert_ab_South<-dat_TerrestInvert_ab[grep("^S",dat_TerrestInvert_ab$eventID),c(1,3)]
dat_TerrestInvert_ab_North<-dat_TerrestInvert_ab[grep("^N",dat_TerrestInvert_ab$eventID),c(1,3)]

#Add up the abundances of each family across sampling events
dat_TerrestInvert_ab_South <-dat_TerrestInvert_ab_South %>% 
  group_by(family) %>%
  summarise(SumCount = sum(individualCount))

dat_TerrestInvert_ab_North <-dat_TerrestInvert_ab_North %>% 
  group_by(family) %>%
  summarise(SumCount = sum(individualCount))

#Make graph of Rank abundance Distribution
dat_TerrestInvert_ab_South$family <- factor(dat_TerrestInvert_ab_South$family, levels = dat_TerrestInvert_ab_South$family[order(dat_TerrestInvert_ab_South$SumCount, decreasing=T)])
dat_TerrestInvert_ab_North$family <- factor(dat_TerrestInvert_ab_North$family, levels = dat_TerrestInvert_ab_North$family[order(dat_TerrestInvert_ab_North$SumCount, decreasing=T)])


library(ggplot2)
  ggplot(dat_TerrestInvert_ab_South,aes(x=family, y=SumCount)) +
  geom_col(color="black",fill="#8C9A8D") +
  theme_bw() +
  scale_y_continuous(limits = c(0,50), expand = c(0, 0.5)) +
  labs(x="Family",y="Abundance",title="Rank Abundance Distribution South Site") +
  guides(x=guide_axis(angle=45)) 
ggsave("results/RADInvertSouth.png",width=7,height=5)
  ggplot(dat_TerrestInvert_ab_North,aes(x=family, y=SumCount)) +
    geom_col(color="black",fill="#8C9A8D") +
    theme_bw() +
    theme(plot.margin = margin(1, 4,1, 15))+
    scale_y_continuous(limits = c(0,50), expand = c(0, 0.5)) +
    labs(x="Family",y="Abundance", title="Rank Abundance Distribution North Site") +
    guides(x=guide_axis(angle=45)) 
ggsave("results/RADInvertNorth.png",width=7,height=5)
#Calculate Simpsons diversity from abundance
dat_TerrestInvert_ab_South$pi<-dat_TerrestInvert_ab_South$SumCount/sum(dat_TerrestInvert_ab_South$SumCount)
dat_TerrestInvert_ab_North$pi<-dat_TerrestInvert_ab_North$SumCount/sum(dat_TerrestInvert_ab_North$SumCount)

SimpsonSouth<-1/sum((dat_TerrestInvert_ab_South$pi)^2)
SimpsonNorth<-1/sum((dat_TerrestInvert_ab_North$pi)^2)
