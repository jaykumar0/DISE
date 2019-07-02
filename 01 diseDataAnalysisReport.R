# http://www.dise.in/
# http://www.dise.in/GraphicPresentation.htm
# http://www.dise.in/use_of_dise_data.htm
# http://www.dise.in/drc.htm

#######################################################################
#######################################################################
### 1. Read the file 
### 2. Get the columns with missing values
### 3. Get min, max, mean, outliers for numeric columns
### 4. Get the values for the categorical columns
### 5. Write the file
#######################################################################
#######################################################################

###
# Set the directory
###
setwd("C:/project/DISE")
library(dplyr)

###
# read UP file, set NA's
###

d <- read.csv('DISE-2014-15/Uttar Pradesh.csv', stringsAsFactors=F,na.strings = c("NA",""))
dim(d)

###
# ToCheck - get the names of cols which have NA values
###
nacols <- function(df) {
  colnames(df)[unlist(lapply(df, function(x) any(is.na(x))))]
}

nacols(d)

###
# Process files one at a time
###

 
###
# Get the number of missing values 
###
dM <- sapply(d, function(x) sum(length(which(is.na(x)))))
dM <- data.frame(dM)

###
# Set the column names
###

dM <- add_rownames(dM, "Cols")
dim(dM)
names(dM) <- c("Column","NumMissingValues")
head(dM) #,20)

###
# Set the missing percentage
###
dM$PercentMissing <- (round(dM$NumMissingValues/(nrow(d)),4))*100
#dM$PercentMissing < round(dM$PercentMissing,2)

###
# Set the number of zeroes - valid for numeric value
###
dM$NumZeroes <- sapply(d, function(x) sum(length(which(x==0))))
#dM$PercentZeroes3 <- round(dM$NumZeroes/nrow(d),3)*100
dM$PercentZeroes <- round(dM$NumZeroes/nrow(d),4)*100
nrow(d)  
length(d)
View(dM)

###
# Set the min, max and mean - valid for numeric values
###
dM$MinValue <- sapply(d, function(x) min(x,na.rm=T))
dM$MaxValue <- sapply(d, function(x) max(x,na.rm=T))
dM$MeanValue <- sapply(d, function(x) round(mean(x,na.rm=T),2))
warnings()

###
# Set the class as Categorical for these columns
###

d$RURURB <- as.factor(d$RURURB)
d$MEDINSTR1 <- as.factor(d$MEDINSTR1)
d$PPSEC_YN <- as.factor(d$PPSEC_YN)
d$SCHRES_YN <- as.factor(d$SCHRES_YN)
d$SCHMGT <- as.factor(d$SCHMGT)
#d$LOWCLASS <- as.factor(d$LOWCLASS)
#d$HIGHCLASS <- as.factor(d$HIGHCLASS)
d$SCHCAT <- as.factor(d$SCHCAT)
d$SCHTYPE <- as.factor(d$SCHTYPE)
d$SCHSHI_YN <- as.factor(d$SCHSHI_YN)
d$RESITYPE <- as.factor(d$RESITYPE)
d$BLDSTATUS <- as.factor(d$BLDSTATUS)
d$MEALSINSCH_x <- as.factor(d$MEALSINSCH_x)
d$CAL_YN <- as.factor(d$CAL_YN)
d$HMROOM_YN <- as.factor(d$HMROOM_YN)
d$ELECTRIC_YN <- as.factor(d$ELECTRIC_YN)
d$BNDRYWALL <- as.factor(d$BNDRYWALL)
d$LIBRARY_YN <- as.factor(d$LIBRARY_YN)
d$PGROUND_YN <- as.factor(d$PGROUND_YN)
d$WATER <- as.factor(d$WATER)
d$MEDCHK_YN <- as.factor(d$MEDCHK_YN)
d$RAMPS_YN <- as.factor(d$RAMPS_YN)
d$APPROACHBYROAD <- as.factor(d$APPROACHBYROAD)
d$CCE_YN <- as.factor(d$CCE_YN)
d$PCR_MAINTAINED <- as.factor(d$PCR_MAINTAINED)
d$PCR_SHARED <- as.factor(d$PCR_SHARED)
d$SMC_YN <- as.factor(d$SMC_YN)
d$SMCSDP_YN <- as.factor(d$SMCSDP_YN)
d$SPLTRG_BY <- as.factor(d$SPLTRG_BY)
d$SPLTRG_PLACE <- as.factor(d$SPLTRG_PLACE)
d$SPLTRG_TYPE <- as.factor(d$SPLTRG_TYPE)
d$TXTBKRECD_YN <- as.factor(d$TXTBKRECD_YN)
d$TXTBKMNTH <- as.factor(d$TXTBKMNTH)
d$ACSTARTMNTH <- as.factor(d$ACSTARTMNTH)
d$MEALSINSCH_y <- as.factor(d$MEALSINSCH_y)
d$KITSHED <- as.factor(d$KITSHED)
d$MDM_MAINTAINER <- as.factor(d$MDM_MAINTAINER)
d$KITDEVGRANT_YN <- as.factor(d$KITDEVGRANT_YN)

###
# Define function to get the number of outliers
###
numOutLiers <- function(x) {
  if (is.numeric(x)) {
    y <- boxplot(x)
    length(y$out)
  }
  else
    #class(x)
    "NA"
}  

###
# Define function to get the percentage  of outliers
###
numOutLiersP <- function(x,rTot) {
  if (is.numeric(x)) {
    y <- boxplot(x)
    round(length(y$out)/rTot, 4) * 100
  }
  else
    #class(x)
    "NA"
}  

###
# Call the two functions ot set the Values
###
dM$NumOutliers <- sapply(d, function(x) numOutLiers(x))
dM$PercentOutliers <- sapply(d, function(x) numOutLiersP(x,nrow(d)))

###
# Define function to get the levels for Categorical values
###

numLevels <- function(x) {
  if (is.numeric(x))
    "NA"
  else
    levels(x)
}  
###
# Call the function to set the values, can identify the invalid values from description
# Delete column after use
###
dM$CatLevels <- sapply(d, function(x) numLevels(x))
class(dM$CatLevels)
dM$CatLevels <- as.character(dM$CatLevels)

###
# Set the column type
###
dM$ClassValue <- sapply(d, function(x) class(x))
dim(filter(dM,ClassValue == "character"))
table(dM$ClassValue)

####
# Get the description, has valid values for the categorical columns for easy compare
###

ColDesc <- read.csv('master/diseColDescription.csv', stringsAsFactors=F)

###
# Merge the two files 
### 
dAll <- inner_join(dM,ColDesc,by="Column")

View(dAll)
dim(dAll)

###
# Write the file
###
write.csv(dAll,"analysis/UP-DataExplorationAnalysis20150126.csv")
