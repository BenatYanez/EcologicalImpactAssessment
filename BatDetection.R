#Load Data
bats<-read.csv("data/Occurences(bats).csv",sep=";")

#Only consider the species detected with a class probability above a threshold of 0.71
dat_bat<- bats[,c("eventID","species","class_prob")]

SpeciesID<-dat_bat[which(dat_bat$class_prob >= 0.71),]
write.table(SpeciesID,file="results/BatsDetected.txt")
