library(stringr)
library(data.table)
library(plyr)

Weight_DSLDist_RSGCN <- read.csv("20170728_RSGCN.csv", na.strings=c("NA"))
Weight_DSLDist_RSGCN <- droplevels(subset(Weight_DSLDist_RSGCN,weight!=0.0000))

Weight_DSLDistHUC6_RSGCN <- read.csv("20170728_RSGCN_HUC6.csv", na.strings=c("NA"))
Weight_DSLDistHUC6_RSGCN <- droplevels(subset(Weight_DSLDistHUC6_RSGCN,weight!=0.0000))
Weight_DSLDistHUC6_RSGCN$HUC6 <- str_sub(Weight_DSLDistHUC6_RSGCN$habitatcode, start= -6) # pulls out the HUC6 for grouping later
Weight_DSLDistHUC6_RSGCN$habitatcode1 <- gsub("^([^_]*_[^_]*)_.*$", "\\1",Weight_DSLDistHUC6_RSGCN$habitatcode)

# quantile score the REGIONAL layers
## Just SGCN
Weight_DSLDist_RSGCN$WeightQuantile <- with(Weight_DSLDist_RSGCN, .bincode(weight, breaks=qu <- sort(quantile(weight, probs=seq(0,1,0.01),na.rm=TRUE)),(labels=(as.numeric(gsub("%.*","",names(qu))))/100)[-1],include.lowest=TRUE))

Weight_DSLDist_RSGCN$WeightQuantile <- as.numeric(Weight_DSLDist_RSGCN$WeightQuantile)


# this stuff below deals with the quantiles within groups.
# function to get quantiles
qfun <- function(x, q=100) {
  quantile <- .bincode(x, breaks=sort(quantile(x, probs=0:q/q)), right=TRUE,include.lowest=TRUE)
  quantile
}

#quantile it
Weight_DSLDistHUC6_RSGCN$q <- ave(Weight_DSLDistHUC6_RSGCN$weight,Weight_DSLDistHUC6_RSGCN$HUC6,FUN=qfun)

# join it
Weight_DSLDist_RSGCN_JOIN <- merge(Weight_DSLDistHUC6_RSGCN, Weight_DSLDist_RSGCN, by.x="habitatcode1", by.y="habitatcode", sort = FALSE)
Weight_DSLDist_RSGCN_JOIN$SUM <-Weight_DSLDist_RSGCN_JOIN$WeightQuantile  + Weight_DSLDist_RSGCN_JOIN$q


# make a final file for joining to the GIS
#FinalWeight_DSLDist <- Weight_DSLDist_All_JOIN[,!(names(Weight_DSLDist_All_JOIN) %in% 
#                      c("X.x","weight.x","HUC6","q","X.y","weight.y","hab_class_name.y","WeightQuantile"))]   # drop a bunch of columns
#setnames(FinalWeight_DSLDist,"SUM","AllSpecies")
TempWeight_DSLDist_RSGCN_JOIN <- Weight_DSLDist_RSGCN_JOIN[,!(names(Weight_DSLDist_RSGCN_JOIN) %in% 
                                                                c("X.x","weight.x","HUC6","q","X.y","weight.y","hab_class_name.x","habitatcode1","hab_class_name.y","WeightQuantile"))]   # drop a bunch of columns
setnames(TempWeight_DSLDist_RSGCN_JOIN,"SUM","SGCN")


#FinalWeight_DSLDist <- merge(FinalWeight_DSLDist, TempWeight_DSLDist_SGCN_JOIN, by="habitatcode", sort=FALSE)
#FinalWeight_DSLDist$Difference <- FinalWeight_DSLDist$AllSpecies-FinalWeight_DSLDist$SGCN
#summary(FinalWeight_DSLDist$Difference)

write.csv(TempWeight_DSLDist_RSGCN_JOIN, "20170728_FinalWeight.csv")

