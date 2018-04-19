
library(data.table)
library(plyr)
library(stringr)
library(tidyr)

setwd("E:/Dropbox (PNHP @ WPC)/RCOAassoc/201804/RCOA_Habitat")

# fucntion for reading in habitat data
myfunction <- function(arg1, arg2, ... ){
  #habitat <- read.csv("Habitat_DSLDistance.csv", na.strings=c("NA")) # DSL and Distance to Water
  #habitat$habitatcode <- paste(habitat$BaseHabitatData1,habitat$BaseHabitatData2, sep="_")
  statements
return(object)
}


# function to created summarized habitat weights
myfunction <- function(arg1, arg2, ... ){
# statements
  # turns the data frame into a table of counts and calculate proportions. 
# the margin setting in prop.table adds row summaries of the individual habitat proportions
count_table <- table(species_final$SNAME,species_final$habitatcode) # turns the raw data into a table of counts
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

# formation subset   #######################################################
lu_formation <- read.csv("lu_formation.csv", stringsAsFactors=FALSE)
data_merge$Formation <- lu_formation[,3][match(data_merge$BaseHabitatData1,lu_formation[,5])] # join the formation in
#gets list of acceptable formations
formation <- read.csv("lu_SGCN_Formation1.csv", stringsAsFactors=FALSE)
library(tidyr)
formation <- separate_rows(formation,Formation,sep=";") # Split delimited strings in a column and insert as new rows
formation$Formation_expected <- formation$Formation # just for reference
data_merge <- merge(x=data_merge, y=formation, by.x=c("rn","Formation"),by.y=c("SNAME","Formation"), all.x=TRUE)
data_merge <- data_merge[which(!is.na(data_merge$Formation_expected)), ]
#####

#calculates the observed minus expected values
data_merge$minus <- (((data_merge$observed - data_merge$expected)/data_merge$expected)+1)
# log transform 
data_merge$minuslog <- 100/(1+(exp(-0.1*(data_merge$minus-48)))) # replaces the log transform 4/08

# Joins the TSS Weights, then multiples the Habitat scores by the TSS weights
###data_merge <- setnames(data_merge,"rn","SNAME")
###SpeciesList_All <- setnames(SpeciesList_All,"Global.Scientific.Name..GNAME.","SNAME") # is this a problem?
##data_merge <- merge(x=data_merge,y=SpeciesList_All[ , c("SNAME", "TSS.quant")], by = "SNAME", all.x=TRUE) 
###data_merge$WghtTimesTSS <- data_merge$minuslog*as.numeric(as.character(data_merge$TSS.quant))

#unweighted
data_merge$WghtTimesTSS <- data_merge$minuslog  # this just bypasses the TSS step that we are not doing at the moment

## INSERT CODE FOR ResponsibilityXConcern weights 
#RSGCNweights_list <- unique(species[ , c("SNAME", "final_weight")])
#data_merge <- merge(x=data_merge,y=RSGCNweights_list,by.x="rn", by.y="SNAME", all.x=TRUE) 
#data_merge$WghtTimesTSS <- data_merge$minuslog*data_merge$final_weight

# creates a habitat LookUp table for the weighting below
#### PICK ONE OF THE BELOW
lu_habitat <- species_final[,c("habitatcode","hab_class_name")] # for terrestrial
#lu_habitat <- species_final[,c("habitatcode","SUM_23")] # for aquatic
#### pick one of the above
lu_habitat <- subset(lu_habitat, !duplicated(habitatcode))

# this sums up the values by habitat. Can change to any weighting method we want
habitat_weights <-aggregate(data_merge$WghtTimesTSS, by=list(data_merge$habitatcode), FUN="sum", na.rm=TRUE)
names(habitat_weights)[names(habitat_weights)=="Group.1"] <- "habitatcode"
names(habitat_weights)[names(habitat_weights)=="x"] <- "weight"
habitat_weights <- join(habitat_weights,lu_habitat,by=c('habitatcode')) ## remove if aquatic
habitat_weights <- habitat_weights[order(-habitat_weights$weight),] # sorts in decending order

# write the summarized weights to a file so we can join it to the GIS
write.csv(habitat_weights, "20180419_RSGCN_update_Regional.csv")
  
return(object)
}
