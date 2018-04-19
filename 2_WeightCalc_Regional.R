#library(reshape)
library(data.table)
library(plyr)
library(stringr)

setwd("E:/Dropbox (PNHP @ WPC)/RCOAassoc/201804")
#######################################################################################################
# reads the habitat proportions in
habitat <- read.csv("Habitat_DSLDistance.csv", na.strings=c("NA")) # DSL and Distance to Water
habitat$habitatcode <- paste(habitat$BaseHabitatData1,habitat$BaseHabitatData2, sep="_")
########## pick one of the above #######################################################################

habitat <- habitat[,!(names(habitat) %in% c("OBJECTID","VALUE"))] #drop a few unneeded columns
# this counts the 'expected' values for the analysis. Basically calculates a proportion
habitat$expected <- habitat$Count / sum(habitat$Count)
habitat <- subset(habitat, !duplicated(habitatcode)) # this gets down to a unique set of habitat codes as 
# having duplicates seems to upset the merge below. this step might not be necessary

# just a little bit of cleanup code to deal with something I didn't do correctly...
lu_FWeco <- read.csv("FWeco.csv")
lu_FWeco <- lu_FWeco[,!(names(lu_FWeco) %in% c("OBJECTID","REALM_ABBR","MHT_NAME","FILTERERO","Shape_Length","Shape_Area"))]

#######################################################################################################
# reads in the species occurence data as delivered by NatureServe
# data should be a csv file with the followign format:
# ****NEED TO UPDATE THIS COMMENT BLOCK!!!****
species <- EO_data # read.csv("Data_SF_all.csv", na.strings=c("NA"))
# the next line reclasses the distances to water into new categories using the cut function -- this is cool!
species$DISTANCE_CAT <- cut(species$Distance, breaks=c(-Inf, 100, 300, 1000, Inf), labels=c("100","300","1000","1001")) # "<100","100-300","300-1000",">1000"

species <- join(species,lu_FWeco,by=c('ECOREGION'))

species$ECO_CODE <- substr(species$ECO_CODE,1,6)      # drops the extra values and only keeps the majority value
species$ECOREGION <- gsub(";.*","",species$ECOREGION) # drops the extra values and only keeps the majority value
species$FISH_FIN <- gsub(";.*","",species$FISH_FIN)   # drops the extra values and only keeps the majority value
species$HUC8 <- gsub(";.*","",species$HUC8)           # drops the extra values and only keeps the majority value
species$ECO_ID <- gsub(";.*","",species$ECO_ID)       # drops the extra values and only keeps the majority value
species$SUM_23 <- gsub(";.*","",species$SUM_23)       # drops the extra values and only keeps the majority value

# adds a column for the HUC06 code (based on a truncation of the HUC08 data already in the table)
species$HUC8 <- str_pad(species$HUC8, 8, side = c("left"), pad = "0")
species$HUC6 <- substr(species$HUC8,1,6)# pads zeros to make HUC8 codes 8 digits long...

# the following section allows one to subset on a species list (i.e RSGCN)
#species <- merge(species, RSGCN, by="SNAME", sort = FALSE)  ##UNCOMMENT TO USE


species1 <- species # copies this over to maintain a backup. Drop rows that are not as needed for this analysis or ones that are duplicates.
species1 <- droplevels(subset(species1,EORANK!="X" & EORANK!="X?" &  EORANK!="H" & EORANK!="H?")) # deletes all the extirpated and historics at the EO level.
species1 <- droplevels(subset(species1,SNAME!="MA sensitive species")) # deletes the 'MA sensitive species' records as they are mixed and we can't code them to any particular habitat

##species1 <- droplevels(subset(species1,hab_class_name!="Dam" & hab_class_name!="Culvert/bridge" & hab_class_name!="Track"
#                              & hab_class_name!="Local road" & hab_class_name!="Active train" & hab_class_name!="Motorway"
#                              & hab_class_name!="Primary road" & hab_class_name!="Secondary road" & hab_class_name!="Tertiary road"
#                              & hab_class_name!="Developed- medium intensity" & hab_class_name!="Developed- high intensity"
#                              & hab_class_name!="Developed- low intensity" & hab_class_name!="Developed- open space"
#                              & hab_class_name!="Abandoned train" & hab_class_name!="Barren land" & hab_class_name!="Shrubland & grassland (NLCD 52/71)" 
#                              & hab_class_name!="Pasture/hay" & hab_class_name!="Cultivated crops" # may want to drop out these ag types
##)) # drops out a bunch of anthropogenic types


# picks the most recent year between the EO and the SF.  They may be be slightly incorrect and not handling all the dates properly...
species1$EO_LASTOBS_YEAR <- as.numeric(as.character(species1$EO_LASTOBS_YEAR)) 
species1$SF_VISIT_YEAR <- as.numeric(as.character(species1$SF_VISIT_YEAR)) # creates a bunch of NAs for all the wierd date values
species1$LASTYEAR <- ifelse(!is.na(species1$EO_LASTOBS_YEAR)>!is.na(species1$SF_VISIT_YEAR), 
                            species1$EO_LASTOBS_YEAR, species1$SF_VISIT_YEAR) 

# subsets the species data to the most recent 30 years of data
species_final <- subset(species1,LASTYEAR>=1986|is.na(LASTYEAR)) # the is.na is to deal with the lack of date data from Maine

# pick one of the following ####################################################################################
species_final$habitatcode <- paste(species_final$gridcode,species_final$DISTANCE_CAT, sep="_")
# pick one of the above ########################################################################################


write.csv(species_final,"20180419_SpeciesFinal.csv")

# the following section provides counts of the unique SF for each species by various geographies ##########################
# these are overwritten each time...
#Summary_SourceSpeciesFinal <- subset(species_final, !duplicated(SHAPE_JOIN)) # 
#Summary_SpeciesByState <- as.data.frame.matrix(table(Summary_SourceSpeciesFinal$SNAME,Summary_SourceSpeciesFinal$SUBNATION))
#Summary_SpeciesByTNCecoregion <- as.data.frame.matrix(table(Summary_SourceSpeciesFinal$SNAME,Summary_SourceSpeciesFinal$ECO_CODE))
#Summary_SpeciesByFWecoregion <- as.data.frame.matrix(table(Summary_SourceSpeciesFinal$SNAME,Summary_SourceSpeciesFinal$ECOREGION))
#Summary_SpeciesByHUC8 <- as.data.frame.matrix(table(Summary_SourceSpeciesFinal$SNAME,Summary_SourceSpeciesFinal$HUC8))
#Summary_SpeciesByHUC6 <- as.data.frame.matrix(table(Summary_SourceSpeciesFinal$SNAME,Summary_SourceSpeciesFinal$HUC6))
#Summary_SpeciesByStateGroup <- as.data.frame.matrix(table(Summary_SourceSpeciesFinal$INFO_TAX_1,Summary_SourceSpeciesFinal$SUBNATION))
##############################################################################################
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

