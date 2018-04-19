# 2018-04-09 

#library(reshape)
library(data.table)
library(plyr)
library(stringr)

setwd("E:/Dropbox (PNHP @ WPC)/RCOAassoc/201804")

RSGCN <- read.csv("RSGCN_FINAL_04072017.csv", stringsAsFactors = FALSE)
RSGCN_species <- sort(unique(RSGCN$SNAME))
RSGCN_species <- replace(RSGCN_species, RSGCN_species=="Cryptobranchus a. alleganiensis", "Cryptobranchus alleganiensis alleganiensis")
RSGCN_species <- replace(RSGCN_species, RSGCN_species=="Bombus bohemicus/ashtoni", "Bombus ashtoni")
RSGCN_species <- replace(RSGCN_species, RSGCN_species=="Acronicta dolli", "Merolonche dolli")
RSGCN_species <- replace(RSGCN_species, RSGCN_species=="Coluber c. constrictor", "Coluber constrictor constrictor")
RSGCN_species <- replace(RSGCN_species, RSGCN_species=="Cottus carolinae kanawhae", "Cottus kanawhae")
RSGCN_species <- replace(RSGCN_species, RSGCN_species=="Gomphus abbreviates", "Gomphus abbreviatus") # spelling mistake???
RSGCN_species <- replace(RSGCN_species, RSGCN_species=="Gomphus septima delawarensis", "Gomphus septima")
RSGCN_species <- replace(RSGCN_species, RSGCN_species=="Malaclemys t. terrapin", "Malaclemys terrapin terrapin")
RSGCN_species <- replace(RSGCN_species, RSGCN_species=="Malaclemys terrapin", "Malaclemys terrapin terrapin")
RSGCN_species <- replace(RSGCN_species, RSGCN_species=="Ophiogomphus mainensis fastigiatus", "Ophiogomphus mainensis")  # not sure if this was the correct call
RSGCN_species <- replace(RSGCN_species, RSGCN_species=="Photedes carterae", "Spartiniphaga carterae")
RSGCN_species <- replace(RSGCN_species, RSGCN_species=="Plebejus idas empetri", "Lycaeides idas empetri")
RSGCN_species <- replace(RSGCN_species, RSGCN_species=="Plestiodon a. anthracinus", "Plestiodon anthracinus anthracinus")
RSGCN_species <- replace(RSGCN_species, RSGCN_species=="Pleuronaia barnesiana", "Fusconaia barnesiana")
RSGCN_species <- replace(RSGCN_species, RSGCN_species=="Psectrotarsia hebardi", "Erythroecia hebardi")
RSGCN_species <- replace(RSGCN_species, RSGCN_species=="Pseudemys rubriventris pop.", "Pseudemys rubriventris pop. 1") # mistake in the RSGCN list?
RSGCN_species <- replace(RSGCN_species, RSGCN_species=="Pseudotriton m. montanus", "Pseudotriton montanus montanus")

# Weighting
RSGCNweights <- RSGCN
RSGCNweights$RegionalID <- NULL
RSGCNweights$Taxa <- NULL
RSGCNweights$Subtaxon <- NULL
RSGCNweights$StatesListingSGCN <- NULL
RSGCNweights$SCOMNAME <- NULL

RSGCNweights$Category[RSGCNweights$Category=="1. Very High"] <- "1. Very High Concern"
RSGCNweights$Category[RSGCNweights$Category=="1. Very high concern"] <- "1. Very High Concern"
RSGCNweights$Category[RSGCNweights$Category=="2. High concern"] <- "2. High Concern"
RSGCNweights$Category[RSGCNweights$Category=="2. High concern"] <- "2. High Concern"
RSGCNweights$Category[RSGCNweights$Category=="3. Moderate"] <- "3. Moderate Concern"
RSGCNweights$Category[RSGCNweights$Category=="3. Moderate concern"] <- "3. Moderate Concern"
RSGCNweights$Category[RSGCNweights$Category=="Data Deficient"] <- "4. Data Deficient"
#Calculate weights
RSGCNweights$catweight[RSGCNweights$Category=="1. Very High Concern"] <- 1
RSGCNweights$catweight[RSGCNweights$Category=="2. High Concern"] <- 0.9
RSGCNweights$catweight[RSGCNweights$Category=="3. Moderate Concern"] <- 0.8
RSGCNweights$catweight[RSGCNweights$Category=="4. Data Deficient"] <- 0.6

RSGCNweights$fweight[RSGCNweights$f=="NE Endemic (100%)"] <- 1
RSGCNweights$fweight[RSGCNweights$f=="75-100%"] <- 0.8
RSGCNweights$fweight[RSGCNweights$f=="50-75%"] <- 0.65
RSGCNweights$fweight[RSGCNweights$f=="25-50%"] <- 0.5
RSGCNweights$fweight[RSGCNweights$f=="<25%"] <- 0.25
RSGCNweights$fweight[RSGCNweights$f==""] <- 0.1

RSGCNweights$final_weight <- RSGCNweights$catweight * RSGCNweights$fweight



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

# merge the weights
EO_data <- merge(EO_data,RSGCNweights,by="SNAME")

# makes a list of species we have data for.
EO_species <- sort(unique(EO_data$SNAME))
matchlist <- match(RSGCN_species,EO_species)
RSGCNlist <- data.frame(col1=RSGCN_species,col2=matchlist)



write.csv(RSGCNlist, "RSGCNlist.csv")


# the following species aren't in the NS dataset
### Alopias superciliosus      # marine species
### Alopias vulpinus           # marine species
### Alosa pseudoharengus
### Amblyraja radiata          # marine species
### Anguispira clarki          # taxonomic confusion?
### Anguispira stihleri
### Boloria montinus
### Bombus affinis
### Bombus ashtoni
### Bombus citrinus
### Bombus pensylvanicus
### Bombus variabilis
### Cambarus hatfieldi
### Carcharhinus obscurus     # marine species
### Carcharhinus signatus     # marine species
### Carcharodon carcharias    # marine species
### Catostomus utawana
### Centropristis striata     # marine species
### Cottus sp. 5 
### Cupido amyntula maritima
### Cyclophora culicaria
### Cynoscion regalis         # marine species
### Danaus plexippus
### Dipturus laevis           # marine species
### Drasteria occulta
### Elliptio angustata
### Elliptio angustata        # taxonomic confsion?
### Eretmochelys i. imbricata # marine species
### Etheostoma longimanum
### Glyphyalinia sp. 1        # taxonomic issues
### Gyrinophilus porphyriticus duryi # taxomonic issues
### Helicodiscus villosus
### Hemipachnobia subporphyrea
### Hippocampus erectus       # marine species
### Isurus oxyrinchus         # marine species
### Isurus paucus             # marine species
### Lamna nasus               # marine species
### Leucoraja ocellata        # marine species
### Lithobates kauffeldi      # too new a species?
### Malacoraja senta          # marine species
### Melospiza georgiana nigrescens
### Mesomphix luisant
### Mesomphix sp. 1
### Metarranthis sp. Near duaria  # unless this is Metarranthis sp. 1????
### Neurocordulia michaeli
### Numenius phaeopus
### Oeneis melissa semidea
### Paralichthys dentatus    #marine species
### Paravitrea mira 
### Paravitrea pontis
### Patera panselenus
### Spartiniphaga carterae
### Plethodon kentucki
### Plethodon virginia
### Pomatomus saltatrix      # marine species
### Pseudopleuronectes americanus # marine species
### Rhincodon typus          # marine species
### Schinia septentrionalis
### Somateria mollissima dresseri
### Sphyrna lewini     # marine species
### Sphyrna zygaena     # marine species
### Squalus acanthias     # marine species
### Stenotrema simile
### Sthenopis auratus
### Synaptomys borealis
### Tautoga onitis    # marine species
### Thunnus thynnus   # marine species
### Triodopsis anteridon
### Triodopsis sp. 1
### Venustaconcha trabalis   # marine species
### Vertigo clappi
### Vertigo parvula
### Villosa constricta

#### all of this was copied and modified from the original HabitatAssociations.R script

## replaced a bunch of subset of the species lists in here---need to figure out what we need from the above....
