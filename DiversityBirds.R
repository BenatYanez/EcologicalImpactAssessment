#Bird Diversity
bird_transect <-read.csv("data/Occurences(bird_transect).csv",sep=";")
bird_pointcount<-read.csv("data/Occurences(point_count).csv",sep=";")

#Change the species names to those in GBIF
#BIRD TRANSECT
library("rgbif")
library(tidyverse)
library(dplyr)
spp_check_ok  <- name_backbone("Felis catus",verbose=T,strict=T) #We initialize a data frame for a species we know is in backbone taxonomy
spp_check_ok  <- spp_check_ok[-1,] #Remove this now
spp_check_bad  <- name_backbone("xxx",verbose = T,strict = T) #We start a dataframe with a species that in not in the backbone
spp_check_bad  <- spp_check_bad[-1,]

for(i in 1:nrow(bird_transect)) {
  toto  <- name_backbone(bird_transect[i,15],verbose=T,strict = T) #Check species I againts the abckbone
  if(length(which(names(toto)=="acceptedUsageKey"))==1) { #IF there is a colum acceptedusagekey we remove it and will not be included for all species
    toto  <- toto[,-which(names(toto)=="acceptedUsageKey")]
  }
  if(ncol(toto)==ncol(spp_check_ok)) { #If there are 23 colums the species name was recognised
    if(length(which(toto$status=="ACCEPTED")) >0) {
      #If there is a species with status ACCEPTED in the returned datframe
      spp_check_ok  <- rbind(spp_check_ok,toto[which(toto$status=="ACCEPTED"),]) #If so we only keep this name
    } else if(length(which(toto$status=="SYNONYM"))>0 ){
      #If there is no species with the status ACCEPTED in the returend datframe is there a species with the name SYNONYM instead?
      warning(paste("Species",bird_transect[i,15],"is a synonym")) #We print a warning
      spp_check_ok   <- rbind(spp_check_ok,toto[which(toto$status=="SYNONYM")[1],])
    } else if(length(which(toto$status=="DOUBTFUL"))>0) {
      warning(paste("Species",bird_transect[i,15],"is doubtful"))
      spp_check_ok  <- rbind(spp_check_ok,toto[which(toto$status=="DOUBTFUL")[1],])
    } else {
      stop("Status unknown") #The status is neither of the others 
    }
    
  }
  else if(ncol(toto)==ncol(spp_check_bad)){
    spp_check_bad  <- rbind(spp_check_bad,toto)
  }
  #else{
  # stop("Unknown length") #If we have  a data frame witha  different size we wnat to check why
  #}
}
duplicated(spp_check_ok$canonicalName)
length(which(spp_check_ok$status=="SYNONYM"))
spp_check_ok_syn  <- spp_check_ok[which(spp_check_ok$status=="SYNONYM"),] #Get a list of synonyms
for (i in 1:nrow(spp_check_ok_syn) ) {
  bird_transect[which(bird_transect[,15] == paste(spp_check_ok_syn[i,3])),15] <- spp_check_ok_syn[i,13]
}
#Calculate Diversity
dat_BirdTransect_red <-bird_transect[,c("species","eventID")]
dat_BirdPointcount_red <-bird_pointcount[,c("species","eventID")]
dat_Bird_red<-rbind(dat_BirdTransect_red,dat_BirdPointcount_red)
dat_Bird_red$presence <- 1


dat_Bird_pa <- dat_Bird_red %>% 
  pivot_wider(names_from=species,values_from=c(presence))
list0 <- as.list(rep(0,ncol(dat_Bird_pa)))
names(list0) <- names(dat_Bird_pa)
dat_Bird_pa <- as.data.frame(dat_Bird_pa %>% replace_na(list0))
row.names(dat_Bird_pa) <- dat_Bird_pa$eventID
dat_Bird_pa <- dat_Bird_pa[,-1]

#Alpha diversity
rowSums(dat_Bird_pa)
NorthAlpha <-  mean(rowSums(dat_Bird_pa[grep("^N",rownames(dat_Bird_pa),ignore.case=T),]))
SouthAlpha <-  mean(rowSums(dat_Bird_pa[grep("^S",rownames(dat_Bird_pa),ignore.case=T),]))
#Alpha diversity just considering the site
dat_BirdSites_red<-dat_Bird_red
dat_BirdSites_red[grep("^N",dat_BirdSites_red$eventID,ignore.case=T),2]<-"North"
dat_BirdSites_red[grep("^S",dat_BirdSites_red$eventID,ignore.case= T),2]<-"South"
dat_BirdSites_red<-distinct(dat_BirdSites_red) #Remove cases where the same species was detected in he same event

dat_BirdSites_pa <- dat_BirdSites_red %>% 
  pivot_wider(names_from=species,values_from=c(presence))
list0 <- as.list(rep(0,ncol(dat_BirdSites_pa)))
names(list0) <- names(dat_BirdSites_pa)
dat_BirdSites_pa <- as.data.frame(dat_BirdSites_pa %>% replace_na(list0))
row.names(dat_BirdSites_pa) <- dat_BirdSites_pa$eventID
dat_BirdSites_pa <- dat_BirdSites_pa[,-1]
AlphaSites<- rowSums(dat_BirdSites_pa)
write.table(c(AlphaSites,NorthAlpha,SouthAlpha),file="results/AlphaDiversityBirds.txt")
library(betapart)
Beta<-beta.pair(dat_BirdSites_pa)

dat_Bird_transect_ab <-bird_transect[,c("species","eventID","individualCount")]
dat_Bird_pointcount_ab<-bird_pointcount[,c("species","eventID","individualCount")]
dat_Bird_ab<-rbind(dat_Bird_pointcount_ab,dat_Bird_transect_ab)
dat_Bird_ab_South<-dat_Bird_ab[grep("^S",dat_Bird_ab$eventID,ignore.case = T),c(1,3)]
dat_Bird_ab_North<-dat_Bird_ab[grep("^N",dat_Bird_ab$eventID,ignore.case = T),c(1,3)]


#Add up the abundances of each species across sampling events
dat_Bird_ab_South <-dat_Bird_ab_South %>% 
  group_by(species) %>%
  summarise(SumCount = sum(individualCount))

dat_Bird_ab_North <-dat_Bird_ab_North %>% 
  group_by(species) %>%
  summarise(SumCount = sum(individualCount))

#Put the common name
dat_Bird_ab_South$CommonName<-c("Meadow pipit","Western jackdaw","Eurasian blue tit", "European robin","Europena chaffinch","Great tit","Coal tit","Dunnock","Goldcrest","Eurasian siskin","Eurasian wren")
dat_Bird_ab_North$CommonName<-c("Meadow pipit","Hen harrier", "Western jackdaw", "Common raven","Hooded crow","Carrion crow","Common kestrel","Northern wheatear","Great tit","Dunnock","Eurasian wren")
#MAke Rank Abundance Distribution Graph

dat_Bird_ab_South$CommonName <- factor(dat_Bird_ab_South$CommonName, levels = dat_Bird_ab_South$CommonName[order(dat_Bird_ab_South$SumCount, decreasing=T)])
dat_Bird_ab_North$CommonName <- factor(dat_Bird_ab_North$CommonName, levels = dat_Bird_ab_North$CommonName[order(dat_Bird_ab_North$SumCount, decreasing=T)])
library(ggplot2)
ggplot(dat_Bird_ab_South,aes(x=CommonName, y=SumCount)) +
  geom_col(color="black",fill="#8C9A8D") +
  theme_bw() +
  scale_y_continuous(limits = c(0,30), expand = c(0, 0.5)) +
  labs(x="Species",y="Abundance",title="Rank Abundance Distribution South Site") +
  guides(x=guide_axis(angle=45)) 
ggsave("results/RADBirdSouth.png",width=7,height=5)

ggplot(dat_Bird_ab_North,aes(x=CommonName, y=SumCount)) +
  geom_col(color="black",fill="#8C9A8D") +
  theme_bw() +
  scale_y_continuous(limits = c(0,30), expand = c(0, 0.5)) +
  labs(x="Species",y="Abundance",title="Rank Abundance Distribution North Site") +
  guides(x=guide_axis(angle=45)) 
ggsave("results/RADBirdNorth.png",width=7,height=5)

#Simpson Diversity
dat_Bird_ab_South$pi<-dat_Bird_ab_South$SumCount/sum(dat_Bird_ab_South$SumCount)
dat_Bird_ab_North$pi<-dat_Bird_ab_North$SumCount/sum(dat_Bird_ab_North$SumCount)

SimpsonSouth<- 1/sum((dat_Bird_ab_South$pi)^2)
SimpsonNorth<- 1/sum((dat_Bird_ab_North$pi)^2)

write.table(SimpsonSouth,file="results/ShannonAbundanceBirdSouth.txt")
write.table(SimpsonNorth,file="results/ShannonAbundanceBirdNorth.txt")
