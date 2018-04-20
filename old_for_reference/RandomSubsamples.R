# script to make random subsamples of importance at intervals
#2018-04-10

library(data.table)
library(plyr)
library(stringr)

setwd("E:/Dropbox (PNHP @ WPC)/RCOAassoc/201804")

#######################################################################################################
# reads the habitat proportions in
habitat <- read.csv("Habitat_DSLDistance.csv", na.strings=c("NA")) # DSL and Distance to Water
habitat$habitatcode <- paste(habitat$BaseHabitatData1,habitat$BaseHabitatData2, sep="_")
habitat <- habitat[,!(names(habitat) %in% c("OBJECTID","VALUE"))] #drop a few unneeded columns
habitat$expected <- habitat$Count / sum(habitat$Count) # this counts the 'expected' values for the analysis.
habitat <- subset(habitat, !duplicated(habitatcode)) # this gets down to a unique set of habitat codes as having duplicates seems to upset the merge below. this step might not be necessary

#######################################################################################################
# reads in the species occurence data as delivered by NatureServe
EO_data <- read.csv("Data_SF_all1.csv", stringsAsFactors=FALSE)
EO_data <- EO_data[which(EO_data$SPECIES_TYPE_1!="P"),]
# cleans up some values in the NS species dataset
EO_data <- replace(EO_data, EO_data=="Cryptobranchus alleganiensis", "Cryptobranchus alleganiensis alleganiensis") # both species in the NS data
EO_data <- replace(EO_data, EO_data=="Coluber constrictor", "Coluber constrictor constrictor") # both species in the NS data
EO_data <- replace(EO_data, EO_data=="Acipenser oxyrinchus oxyrinchus", "Acipenser oxyrinchus") # both species in the NS data
EO_data <- replace(EO_data, EO_data=="Abagrotis nefascia", "Abagrotis nefascia benjamini")
EO_data <- replace(EO_data, EO_data=="Aphredoderus sayanus", "Aphredoderus sayanus gibbosus")
EO_data <- replace(EO_data, EO_data=="Cicindela patruela", "Cicindela patruela patruela")
EO_data <- replace(EO_data, EO_data=="Cicindela rufiventris", "Cicindela rufiventris hentzii")
EO_data <- replace(EO_data, EO_data=="Crotalus horridus [Coastal Plain population]", "Crotalus horridus")
EO_data <- replace(EO_data, EO_data=="Crotalus horridus horridus", "Crotalus horridus")
EO_data <- replace(EO_data, EO_data=="Malaclemys terrapin", "Malaclemys terrapin terrapin")
EO_data <- replace(EO_data, EO_data=="Passerculus sandwichensis ssp. princeps", "Passerculus sandwichensis princeps")
EO_data <- replace(EO_data, EO_data=="Terrapene carolina carolina", "Terrapene carolina")

# the next line reclasses the distances to water into new categories using the cut function -- this is cool!
EO_data$DISTANCE_CAT <- cut(EO_data$Distance, breaks=c(-Inf, 100, 300, 1000, Inf), labels=c("100","300","1000","1001")) # "<100","100-300","300-1000",">1000"
EO_data$DISTANCE_CAT <- as.character(EO_data$DISTANCE_CAT)
EO_data$ECO_CODE <- substr(EO_data$ECO_CODE,1,6)      # drops the extra values and only keeps the majority value
EO_data$ECOREGION <- gsub(";.*","",EO_data$ECOREGION) # drops the extra values and only keeps the majority value
EO_data$FISH_FIN <- gsub(";.*","",EO_data$FISH_FIN)   # drops the extra values and only keeps the majority value
EO_data$HUC8 <- gsub(";.*","",EO_data$HUC8)           # drops the extra values and only keeps the majority value
##EO_data$ECO_ID <- gsub(";.*","",EO_data$ECO_ID)       # drops the extra values and only keeps the majority value
EO_data$SUM_23 <- gsub(";.*","",EO_data$SUM_23)       # drops the extra values and only keeps the majority value
# adds a column for the HUC06 code (based on a truncation of the HUC08 data already in the table)
EO_data$HUC8 <- str_pad(EO_data$HUC8, 8, side = c("left"), pad = "0")# pads zeros to make HUC8 codes 8 digits long...
EO_data$HUC6 <- substr(EO_data$HUC8,1,6)
EO_data <- droplevels(subset(EO_data,EORANK!="X" & EORANK!="X?" &  EORANK!="H" & EORANK!="H?")) # deletes all the extirpated and historics at the EO level.
EO_data <- droplevels(subset(EO_data,SNAME!="MA sensitive species")) # deletes the 'MA sensitive species' records 
# picks the most recent year between the EO and the SF.  They may be be slightly incorrect and not handling all the dates properly...
EO_data$EO_LASTOBS_YEAR <- as.numeric(as.character(EO_data$EO_LASTOBS_YEAR)) 
EO_data$SF_VISIT_YEAR <- as.numeric(as.character(EO_data$SF_VISIT_YEAR)) # creates a bunch of NAs for all the wierd date values
EO_data$LASTYEAR <- ifelse(!is.na(EO_data$EO_LASTOBS_YEAR)>!is.na(EO_data$SF_VISIT_YEAR), EO_data$EO_LASTOBS_YEAR, EO_data$SF_VISIT_YEAR) 
# subsets the species data to the most recent 30 years of data
EO_data <- subset(EO_data,LASTYEAR>=1986|is.na(LASTYEAR)) # the is.na is to deal with the lack of date data from Maine
EO_data$habitatcode <- paste(EO_data$gridcode,EO_data$DISTANCE_CAT, sep="_")
lu_habitat <- EO_data[,c("habitatcode","hab_class_name")] # for terrestrial
#lu_habitat <- species_final[,c("habitatcode","SUM_23")] # for aquatic
#### pick one of the above
lu_habitat <- subset(lu_habitat, !duplicated(habitatcode))

### Max value - this code is repeated below in the loop for the subsets
#

# turns the data frame into a table of counts and calculate proportions. 
# the margin setting in prop.table adds row summaries of the individual habitat proportions
count_table <- table(EO_data$SNAME,EO_data$habitatcode) # turns the raw data into a table of counts
prop_table <- prop.table(count_table,margin=1) # adds column? summaries
prop_table <- addmargins(prop_table,margin=2) #adds row? summaries
# convert prop_table into a dataframe
occ_prop <- as.data.frame.matrix(prop_table) 
occ_prop <- cbind(row.names = rownames(occ_prop), occ_prop) # adds the row names into the data frame
setDT(occ_prop, keep.rownames = TRUE)[]
# this transforms the data into a table with one entry per species and habitat combination, this makes
results_melt <- melt(occ_prop, id="rn")
setnames(results_melt,"variable","habitatcode") 
setnames(results_melt,"value","observed")
data_merge <- merge(results_melt,habitat,by="habitatcode",all=TRUE) # , allow.cartesian=TRUE
data_merge <- data_merge[ which(!is.na(data_merge$expected)), ] # gets rid of empty values
#calculates the observed minus expected values
data_merge$minus <- (((data_merge$observed - data_merge$expected)/data_merge$expected)+1)
# log transform 
data_merge$minuslog <- 100/(1+(exp(-0.1*(data_merge$minus-48)))) # replaces the log transform 4/08
data_merge$WghtTimesTSS <- data_merge$minuslog  # this just bypasses the TSS step that we are not doing at the moment
# this sums up the values by habitat. Can change to any weighting method we want
habitat_weights <-aggregate(data_merge$WghtTimesTSS, by=list(data_merge$habitatcode), FUN="mean", na.rm=TRUE)
habitat_weights <- habitat_weights[complete.cases(habitat_weights), ]
names(habitat_weights)[names(habitat_weights)=="Group.1"] <- "habitatcode"
names(habitat_weights)[names(habitat_weights)=="x"] <- "weight"
habitat_weights <- join(habitat_weights,lu_habitat,by=c('habitatcode')) ## remove if aquatic
habitat_weights <- habitat_weights[order(-habitat_weights$weight),] # sorts in decending order
habitat_weightsMAX <- habitat_weights
habitat_weightsMAX$hab_class_name <- NULL
setnames(habitat_weightsMAX,"weight","weightMAX")
#
####



# get a list of all the species and make a list for subset
SpeciesList <- sort(unique(EO_data$SNAME))

# set up a empty data frame for the results
HabImport_results <- data.frame(RunSample=character(),Average=character(),stringsAsFactors=FALSE) 

# begin to insert loop here
for(x in 1:30){
  
for(j in seq(from=1, to=length(SpeciesList), by=20)){
  SpeciesList_sample <- sample(SpeciesList, j,replace=FALSE,prob=NULL)
  print(j)
  EO_data_sample <- EO_data[EO_data$SNAME %in% SpeciesList_sample,]
  # turns the data frame into a table of counts and calculate proportions. 
  # the margin setting in prop.table adds row summaries of the individual habitat proportions
  count_table <- table(EO_data_sample$SNAME,EO_data_sample$habitatcode) # turns the raw data into a table of counts
  prop_table <- prop.table(count_table,margin=1) # adds column? summaries
  prop_table <- addmargins(prop_table,margin=2) #adds row? summaries
  # convert prop_table into a dataframe
  occ_prop <- as.data.frame.matrix(prop_table) 
  occ_prop <- cbind(row.names = rownames(occ_prop), occ_prop) # adds the row names into the data frame
  setDT(occ_prop, keep.rownames = TRUE)[]
  # this transforms the data into a table with one entry per species and habitat combination, this makes
  #    the calculatations a little easier (eg. less matrix math)
  results_melt <- melt(occ_prop, id="rn")
  setnames(results_melt,"variable","habitatcode") 
  setnames(results_melt,"value","observed")
  data_merge <- merge(results_melt,habitat,by="habitatcode",all=TRUE) # , allow.cartesian=TRUE
  data_merge <- data_merge[ which(!is.na(data_merge$expected)), ] # gets rid of empty values
  #calculates the observed minus expected values
  data_merge$minus <- (((data_merge$observed - data_merge$expected)/data_merge$expected)+1)
  # log transform 
  data_merge$minuslog <- 100/(1+(exp(-0.1*(data_merge$minus-48)))) # replaces the log transform 4/08
  data_merge$WghtTimesTSS <- data_merge$minuslog  # this just bypasses the TSS step that we are not doing at the moment
  # this sums up the values by habitat. Can change to any weighting method we want
  habitat_weights <-aggregate(data_merge$WghtTimesTSS, by=list(data_merge$habitatcode), FUN="mean", na.rm=TRUE)
  habitat_weights <- habitat_weights[complete.cases(habitat_weights), ]
  names(habitat_weights)[names(habitat_weights)=="Group.1"] <- "habitatcode"
  names(habitat_weights)[names(habitat_weights)=="x"] <- "weight"
  habitat_weights <- join(habitat_weights,lu_habitat,by=c('habitatcode')) ## remove if aquatic
  habitat_weights <- habitat_weights[order(-habitat_weights$weight),] # sorts in decending order

  habitat_weightsCOMPARE <- merge(habitat_weights,habitat_weightsMAX, by=c('habitatcode'), all=TRUE)
  habitat_weightsCOMPARE$diff <- (habitat_weightsCOMPARE$weightMAX-habitat_weightsCOMPARE$weight)^2
  habitat_weightsCOMPARE <- habitat_weightsCOMPARE[complete.cases(habitat_weightsCOMPARE), ]
  runResults <- data.frame(j, mean(habitat_weightsCOMPARE$diff))
  
  HabImport_results <- rbind(HabImport_results, runResults)
}

}

plot(HabImport_results$mean.habitat_weightsCOMPARE.diff.~HabImport_results$j, ylim=c(0,10))


