ProtectedSpecies<- read.csv("data/ProtectedSpeciesScotland.csv",sep=";")

NorthSite<- read.csv("data/SpeciesListNorth.csv",header=T,sep=";")
SouthSite<- read.csv("data/SpeciesListSouth.csv",header=T,sep=";")
BiodiversityList<- read.csv("data/ScottishBiodiversityList.csv",sep=";")
BiodiversityList<-BiodiversityList[-1,c(1,3,4,5,6)]
library("rgbif")

#Compare species names From North to GBIF backbone
spp_check_ok  <- name_backbone("Felis catus",verbose=T,strict=T) #We initialize a data frame for a species we know is in backbone taxonomy
spp_check_ok  <- spp_check_ok[-1,] #Remove this now
spp_check_bad  <- name_backbone("xxx",verbose = T,strict = T) #We start a dataframe with a species that in not in the backbone
spp_check_bad  <- spp_check_bad[-1,]

pb  <- txtProgressBar(min=0, max=nrow(NorthSite), initial=0,style=3)
for(i in 1:nrow(NorthSite)) {
  toto  <- name_backbone(NorthSite[i,1],verbose=T,strict = T) #Check species I againts the abckbone
  if(length(which(names(toto)=="acceptedUsageKey"))==1) { #IF there is a colum acceptedusagekey we remove it and will not be included for all species
    toto  <- toto[,-which(names(toto)=="acceptedUsageKey")]
  }
  if(ncol(toto)==ncol(spp_check_ok)) { #If there are 23 colums the species name was recognised
    if(length(which(toto$status=="ACCEPTED")) >0) {
      #If there is a species with status ACCEPTED in the returned datframe
      spp_check_ok  <- rbind(spp_check_ok,toto[which(toto$status=="ACCEPTED"),]) #If so we only keep this name
    } else if(length(which(toto$status=="SYNONYM"))>0 ){
      #If there is no species with the status ACCEPTED in the returend datframe is there a species with the name SYNONYM instead?
      warning(paste("Species",NorthSite[i,1],"is a synonym")) #We print a warning
      spp_check_ok   <- rbind(spp_check_ok,toto[which(toto$status=="SYNONYM")[1],])
    } else if(length(which(toto$status=="DOUBTFUL"))>0) {
      warning(paste("Species",NorthSite[i,1],"is doubtful"))
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
  info    <- sprintf("%d%% done", round((i/nrow(NorthSite)*100))) #Update progress bar
}
close(pb)
duplicated(spp_check_ok$canonicalName)
length(which(spp_check_ok$status=="SYNONYM")) #There are 8
spp_check_ok_syn  <- spp_check_ok[which(spp_check_ok$status=="SYNONYM"),] #Get a list of synonyms

#Change the names that are synonyms to the appropriate name
for (i in 1:nrow(spp_check_ok_syn) ) {
  NorthSite[which(NorthSite[,1] == paste(spp_check_ok_syn[i,3])),1] <- spp_check_ok_syn[i,13]
}

#Do the same for the South
rm(spp_check_bad)
rm(spp_check_ok)
rm(spp_check_ok_syn)
rm(toto)
spp_check_ok  <- name_backbone("Felis catus",verbose=T,strict=T) #We initialize a data frame for a species we know is in backbone taxonomy
spp_check_ok  <- spp_check_ok[-1,] #Remove this now
spp_check_bad  <- name_backbone("xxx",verbose = T,strict = T) #We start a dataframe with a species that in not in the backbone
spp_check_bad  <- spp_check_bad[-1,]

pb  <- txtProgressBar(min=0, max=nrow(SouthSite), initial=0,style=3)
for(i in 1:nrow(SouthSite)) {
  toto  <- name_backbone(SouthSite[i,1],verbose=T,strict = T) #Check species I againts the abckbone
  if(length(which(names(toto)=="acceptedUsageKey"))==1) { #IF there is a colum acceptedusagekey we remove it and will not be included for all species
    toto  <- toto[,-which(names(toto)=="acceptedUsageKey")]
  }
  if(ncol(toto)==ncol(spp_check_ok)) { #If there are 23 colums the species name was recognised
    if(length(which(toto$status=="ACCEPTED")) >0) {
      #If there is a species with status ACCEPTED in the returned datframe
      spp_check_ok  <- rbind(spp_check_ok,toto[which(toto$status=="ACCEPTED"),]) #If so we only keep this name
    } else if(length(which(toto$status=="SYNONYM"))>0 ){
      #If there is no species with the status ACCEPTED in the returend datframe is there a species with the name SYNONYM instead?
      warning(paste("Species",SouthSite[i,1],"is a synonym")) #We print a warning
      spp_check_ok   <- rbind(spp_check_ok,toto[which(toto$status=="SYNONYM")[1],])
    } else if(length(which(toto$status=="DOUBTFUL"))>0) {
      warning(paste("Species",SouthSite[i,1],"is doubtful"))
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
  info    <- sprintf("%d%% done", round((i/nrow(SouthSite)*100))) #Update progress bar
}
close(pb)
duplicated(spp_check_ok$canonicalName)
length(which(spp_check_ok$status=="SYNONYM")) #There are 8
spp_check_ok_syn  <- spp_check_ok[which(spp_check_ok$status=="SYNONYM"),] #Get a list of synonyms
#Change the names that are synonyms to the appropriate name
for (i in 1:nrow(spp_check_ok_syn) ) {
  SouthSite[which(SouthSite[,1] == paste(spp_check_ok_syn[i,3])),1] <- spp_check_ok_syn[i,13]
}

#DO the sam efor the Protected SPecies list
Protspp <- unique(ProtectedSpecies$Current.taxon.name)

rm(spp_check_bad)
rm(spp_check_ok)
rm(spp_check_ok_syn)
rm(toto)
spp_check_ok  <- name_backbone("Felis catus",verbose=T,strict=T) #We initialize a data frame for a species we know is in backbone taxonomy
spp_check_ok  <- spp_check_ok[-1,] #Remove this now
spp_check_bad  <- name_backbone("xxx",verbose = T,strict = T) #We start a dataframe with a species that in not in the backbone
spp_check_bad  <- spp_check_bad[-1,]

pb  <- txtProgressBar(min=0, max=length(Protspp), initial=0,style=3)
for(i in 1:length(spp)) {
  toto  <- name_backbone(Protspp[i],verbose=T,strict = T) #Check species I againts the abckbone
  if(length(which(names(toto)=="acceptedUsageKey"))==1) { #IF there is a colum acceptedusagekey we remove it and will not be included for all species
    toto  <- toto[,-which(names(toto)=="acceptedUsageKey")]
  }
  if(ncol(toto)==ncol(spp_check_ok)) { #If there are 23 colums the species name was recognised
    if(length(which(toto$status=="ACCEPTED")) >0) {
      #If there is a species with status ACCEPTED in the returned datframe
      spp_check_ok  <- rbind(spp_check_ok,toto[which(toto$status=="ACCEPTED"),]) #If so we only keep this name
    } else if(length(which(toto$status=="SYNONYM"))>0 ){
      #If there is no species with the status ACCEPTED in the returend datframe is there a species with the name SYNONYM instead?
      warning(paste("Species",Protspp[i],"is a synonym")) #We print a warning
      spp_check_ok   <- rbind(spp_check_ok,toto[which(toto$status=="SYNONYM")[1],])
    } else if(length(which(toto$status=="DOUBTFUL"))>0) {
      warning(paste("Species",Protspp[i],"is doubtful"))
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
  info    <- sprintf("%d%% done", round((i/length(Protspp)*100))) #Update progress bar
}
close(pb)
duplicated(spp_check_ok$canonicalName)
length(which(spp_check_ok$status=="SYNONYM")) #There are 8
spp_check_ok_syn  <- spp_check_ok[which(spp_check_ok$status=="SYNONYM"),] #Get a list of synonyms
#Change the synonym names to the appropriate name
for (i in 1:nrow(spp_check_ok_syn) ) {
  Protspp[which(Protspp == paste(spp_check_ok_syn[i,3]))] <- spp_check_ok_syn[i,13]
}
Protspp <-unlist(Protspp)
#DO the same for the Biodiversity list
BioListspp <- unique(BiodiversityList$Scientific.Name)

rm(spp_check_bad)
rm(spp_check_ok)
rm(spp_check_ok_syn)
rm(toto)
spp_check_ok  <- name_backbone("Felis catus",verbose=T,strict=T) #We initialize a data frame for a species we know is in backbone taxonomy
spp_check_ok  <- spp_check_ok[-1,] #Remove this now
spp_check_bad  <- name_backbone("xxx",verbose = T,strict = T) #We start a dataframe with a species that in not in the backbone
spp_check_bad  <- spp_check_bad[-1,]

pb  <- txtProgressBar(min=0, max=length(BioListspp), initial=0,style=3)
for(i in 1:length(BioListspp)) {
  toto  <- name_backbone(BioListspp[i],verbose=T,strict = T) #Check species I againts the abckbone
  if(length(which(names(toto)=="acceptedUsageKey"))==1) { #IF there is a colum acceptedusagekey we remove it and will not be included for all species
    toto  <- toto[,-which(names(toto)=="acceptedUsageKey")]
  }
  if(ncol(toto)==ncol(spp_check_ok)) { #If there are 23 colums the species name was recognised
    if(length(which(toto$status=="ACCEPTED")) >0) {
      #If there is a species with status ACCEPTED in the returned datframe
      spp_check_ok  <- rbind(spp_check_ok,toto[which(toto$status=="ACCEPTED"),]) #If so we only keep this name
    } else if(length(which(toto$status=="SYNONYM"))>0 ){
      #If there is no species with the status ACCEPTED in the returend datframe is there a species with the name SYNONYM instead?
      warning(paste("Species",BioListspp[i],"is a synonym")) #We print a warning
      spp_check_ok   <- rbind(spp_check_ok,toto[which(toto$status=="SYNONYM")[1],])
    } else if(length(which(toto$status=="DOUBTFUL"))>0) {
      warning(paste("Species",BioListspp[i],"is doubtful"))
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
  info    <- sprintf("%d%% done", round((i/length(BioListspp)*100))) #Update progress bar
}
close(pb)
duplicated(spp_check_ok$canonicalName)
length(which(spp_check_ok$status=="SYNONYM")) #There are 270
spp_check_ok_syn  <- spp_check_ok[which(spp_check_ok$status=="SYNONYM"),] #Get a list of synonyms
#Change the synonym names to the appropriate name
for (i in 1:nrow(spp_check_ok_syn) ) {
  BioListspp[which(BioListspp == paste(spp_check_ok_syn[i,3]))] <- spp_check_ok_syn[i,13]
}
BioListspp <-unlist(BioListspp)
BiodiversityList$Scientific.Name<-BioListspp

#Identify which species are shared between the Sites and the protected species vectors
DataNorth<-c(unique(NorthSite$Species.Name),Protspp)
DataSouth<-c(unique(SouthSite$ï..Species.Name),Protspp)
ProtectedSpeciesNorthSite <- DataNorth[which(duplicated(DataNorth) == TRUE)]
ProtectedSpeciesSouthSite <- DataSouth[which(duplicated(DataSouth) == TRUE)]
#Save data
write.table(ProtectedSpeciesNorthSite,file="results/ProtectedSpeciesNorthDeskSurvey.txt")
write.table(ProtectedSpeciesSouthSite,file="results/ProtectedSpeciesSouthDeskSurvey.txt")
write.table(Protspp,file="results/ProtectedSpeciesCorrectNames.txt")
write.table(BiodiversityList,file="results/BiodiversityListCorrectNames.txt")
