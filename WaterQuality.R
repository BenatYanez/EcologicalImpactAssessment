#Load data
aquatic_invert<-read.csv("data/Occurences(aquatic_inv).csv",sep=";")
waterquality<-read.csv("data/ASPT_WaterQuality.csv",sep=";")

dat_AquaticInvert<-aquatic_invert[c("family","eventID")]
dat_AquaticInvert<-dat_AquaticInvert[-which(dat_AquaticInvert == ""),]
dat_AquaticInvert$Score<- 0
for (i in 1:nrow(dat_AquaticInvert)) {
 n <- which(waterquality == dat_AquaticInvert[i,1])
 if (identical(n,integer(0)) == FALSE) {
 dat_AquaticInvert[i,3]<- as.numeric(waterquality[n,2])
}
 }
dat_AquaticInvert<-dat_AquaticInvert[-which(dat_AquaticInvert$Score == 0),]
dat_AquaticInvertSouth<- (dat_AquaticInvert[grep("^S",dat_AquaticInvert$eventID),])
dat_AquaticInvertNorth<- (dat_AquaticInvert[grep("^N",dat_AquaticInvert$eventID),])
ASPTSouth<-mean(dat_AquaticInvertSouth$Score)
ASPTNorth<-mean(dat_AquaticInvertNorth$Score)

write.table(ASPTSouth,file="results/WaterQualitySouth.txt")
write.table(ASPTNorth,file="results/WaterQualityNorth.txt")
