#Load the data
bird_transect <-read.csv("data/Occurences(bird_transect).csv",sep=";")
bird_pointcount<-read.csv("data/Occurences(point_count).csv",sep=";")
terrest_invert<-read.csv("data/Occurences(terrestrial_inv).csv",sep=";")
aquatic_invert<-read.csv("data/Occurences(aquatic_inv).csv",sep=";")
bats<-read.csv("data/Occurences(bats).csv",sep=";")
#Load protected species list
ProtectedSpecies<-read.table("results/ProtectedSpeciesCorrectNames.txt")
ProtectedSpecies <-as.vector(ProtectedSpecies)
ProtectedSpecies<- unlist(ProtectedSpecies)
#Load Scottish Biodiversity List Species
BiodiversityList<-read.table("results/BiodiversityListCorrectNames.txt")
#Change the species names to those in GBIF
  #BIRD TRANSECT
library("rgbif")

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
  #BIRD POINTCOUNT
rm(spp_check_bad)
rm(spp_check_ok)
rm(spp_check_ok_syn)
rm(toto)
spp_check_ok  <- name_backbone("Felis catus",verbose=T,strict=T) #We initialize a data frame for a species we know is in backbone taxonomy
spp_check_ok  <- spp_check_ok[-1,] #Remove this now
spp_check_bad  <- name_backbone("xxx",verbose = T,strict = T) #We start a dataframe with a species that in not in the backbone
spp_check_bad  <- spp_check_bad[-1,]
for(i in 1:nrow(bird_pointcount)) {
  toto  <- name_backbone(bird_pointcount[i,15],verbose=T,strict = T) #Check species I againts the abckbone
  if(length(which(names(toto)=="acceptedUsageKey"))==1) { #IF there is a colum acceptedusagekey we remove it and will not be included for all species
    toto  <- toto[,-which(names(toto)=="acceptedUsageKey")]
  }
  if(ncol(toto)==ncol(spp_check_ok)) { #If there are 23 colums the species name was recognised
    if(length(which(toto$status=="ACCEPTED")) >0) {
      #If there is a species with status ACCEPTED in the returned datframe
      spp_check_ok  <- rbind(spp_check_ok,toto[which(toto$status=="ACCEPTED"),]) #If so we only keep this name
    } else if(length(which(toto$status=="SYNONYM"))>0 ){
      #If there is no species with the status ACCEPTED in the returend datframe is there a species with the name SYNONYM instead?
      warning(paste("Species",bird_pointcount[i,15],"is a synonym")) #We print a warning
      spp_check_ok   <- rbind(spp_check_ok,toto[which(toto$status=="SYNONYM")[1],])
    } else if(length(which(toto$status=="DOUBTFUL"))>0) {
      warning(paste("Species",bird_pointcount[i,15],"is doubtful"))
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
  bird_pointcount[which(bird_pointcount[,15] == paste(spp_check_ok_syn[i,3])),15] <- spp_check_ok_syn[i,13]
}

  #TERRESTIAL INVERTS
rm(spp_check_bad)
rm(spp_check_ok)
rm(spp_check_ok_syn)
rm(toto)
spp_check_ok  <- name_backbone("Felis catus",verbose=T,strict=T) #We initialize a data frame for a species we know is in backbone taxonomy
spp_check_ok  <- spp_check_ok[-1,] #Remove this now
spp_check_bad  <- name_backbone("xxx",verbose = T,strict = T) #We start a dataframe with a species that in not in the backbone
spp_check_bad  <- spp_check_bad[-1,]
for(i in 1:nrow(terrest_invert)) {
  toto  <- name_backbone(terrest_invert[i,15],verbose=T,strict = T) #Check species I againts the abckbone
  if(length(which(names(toto)=="acceptedUsageKey"))==1) { #IF there is a colum acceptedusagekey we remove it and will not be included for all species
    toto  <- toto[,-which(names(toto)=="acceptedUsageKey")]
  }
  if(ncol(toto)==ncol(spp_check_ok)) { #If there are 23 colums the species name was recognised
    if(length(which(toto$status=="ACCEPTED")) >0) {
      #If there is a species with status ACCEPTED in the returned datframe
      spp_check_ok  <- rbind(spp_check_ok,toto[which(toto$status=="ACCEPTED"),]) #If so we only keep this name
    } else if(length(which(toto$status=="SYNONYM"))>0 ){
      #If there is no species with the status ACCEPTED in the returend datframe is there a species with the name SYNONYM instead?
      warning(paste("Species",terrest_invert[i,15],"is a synonym")) #We print a warning
      spp_check_ok   <- rbind(spp_check_ok,toto[which(toto$status=="SYNONYM")[1],])
    } else if(length(which(toto$status=="DOUBTFUL"))>0) {
      warning(paste("Species",terrest_invert[i,15],"is doubtful"))
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
  terrest_invert[which(terrest_invert[,15] == paste(spp_check_ok_syn[i,3])),15] <- spp_check_ok_syn[i,13]
}
  ##BATS

rm(spp_check_bad)
rm(spp_check_ok)
rm(spp_check_ok_syn)
rm(toto)
spp_check_ok  <- name_backbone("Felis catus",verbose=T,strict=T) #We initialize a data frame for a species we know is in backbone taxonomy
spp_check_ok  <- spp_check_ok[-1,] #Remove this now
spp_check_bad  <- name_backbone("xxx",verbose = T,strict = T) #We start a dataframe with a species that in not in the backbone
spp_check_bad  <- spp_check_bad[-1,]
for(i in 1:nrow(bats)) {
  toto  <- name_backbone(bats[i,8],verbose=T,strict = T) #Check species I againts the abckbone
  if(length(which(names(toto)=="acceptedUsageKey"))==1) { #IF there is a colum acceptedusagekey we remove it and will not be included for all species
    toto  <- toto[,-which(names(toto)=="acceptedUsageKey")]
  }
  if(ncol(toto)==ncol(spp_check_ok)) { #If there are 23 colums the species name was recognised
    if(length(which(toto$status=="ACCEPTED")) >0) {
      #If there is a species with status ACCEPTED in the returned datframe
      spp_check_ok  <- rbind(spp_check_ok,toto[which(toto$status=="ACCEPTED"),]) #If so we only keep this name
    } else if(length(which(toto$status=="SYNONYM"))>0 ){
      #If there is no species with the status ACCEPTED in the returend datframe is there a species with the name SYNONYM instead?
      warning(paste("Species",bats[i,8],"is a synonym")) #We print a warning
      spp_check_ok   <- rbind(spp_check_ok,toto[which(toto$status=="SYNONYM")[1],])
    } else if(length(which(toto$status=="DOUBTFUL"))>0) {
      warning(paste("Species",bats[i,8],"is doubtful"))
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
  bats[which(bats[,9] == paste(spp_check_ok_syn[i,3])),9] <- spp_check_ok_syn[i,13]
}
bats[c(1,20),c(8,15)]<-"Barbastella barbastellus"

#Check if there are any protected species
DataBirdsProtected<-c(unique(c(bird_transect$species,bird_pointcount$species)),ProtectedSpecies)
DataTerrestrialInvertProtected<-c(unique(terrest_invert$species),ProtectedSpecies)
DataBatsProtected<-c(unique(bats$species),ProtectedSpecies)

ProtectedSpeciesBirds <- DataBirdsProtected[which(duplicated(DataBirdsProtected) == TRUE)]
write.table(ProtectedSpeciesBirds,file="results/ProtectedBirds.txt")
ProtectedSpeciesTerrestrialInvert <- DataTerrestrialInvertProtected[which(duplicated(DataTerrestrialInvertProtected) == TRUE)]
write.table(ProtectedSpeciesTerrestrialInvert,file="results/Protectedinverts.txt")
ProtectedSpeciesBats <- DataBatsProtected[which(duplicated(DataBatsProtected) == TRUE)]
write.table(ProtectedSpeciesBats,file="results/ProtectedBats.txt")
#Check if any species can be found in the Biodiversity list
DataBirdsBiodiversity<-c(unique(c(bird_transect$species,bird_pointcount$species)),unique(BiodiversityList$Scientific.Name))
DataTerrestrialInvertBiodiversity<-c(unique(terrest_invert$species),unique(BiodiversityList$Scientific.Name))
DataBatsBiodiversity<-c(unique(bats$species),unique(BiodiversityList$Scientific.Name))

BiodiversityListAllDataframe<- BiodiversityList[1,]
BiodiversityListAllDataframe<-BiodiversityListAllDataframe[-1,]
BiodiversityListAll <- c(DataBirdsBiodiversity[which(duplicated(DataBirdsBiodiversity) == TRUE)],DataTerrestrialInvertBiodiversity[which(duplicated(DataTerrestrialInvertBiodiversity) == TRUE)],DataBatsBiodiversity[which(duplicated(DataBatsBiodiversity) == TRUE)])
for (i in 1:length(BiodiversityListAll)){
  BiodiversityListAllDataframe[i,]<-BiodiversityList[which(BiodiversityList$Scientific.Name ==  paste(BiodiversityListAll[i])),]
}

write.table(BiodiversityListAllDataframe,file="results/AnimalsInBiodiversityList.txt")
