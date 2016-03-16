#A TOTAL CASE RATE (TCR) ANALYSIS OF INJURIES AND ILLNESS AT WORK MAY ALLOW US TO SET DIFFERENT 
#TYPES OF STRATEGIES TO MINIMISE THEM BASED ON A SEGMENTATION APPROACH BY MAJOR GROUP (SIC CODES)

#By Giancarlo Callaoapaza

#Loading the data
ODI <- read.csv("ODI2011.csv", colClasses = "character")
sicCodes <- read.csv("sic4-list-master/sic-codes.csv", colClasses = "character")
majorGroups <- read.csv("sic4-list-master/major-groups.csv", colClasses = "character")
industryGroups <- read.csv("sic4-list-master/industry-groups.csv", colClasses = "character")

#Transforming to numeric values
ODI$TCR <- as.numeric(ODI$TCR)
ODI$DART <- as.numeric(ODI$DART)
ODI$DAFWII <- as.numeric(ODI$DAFWII)

#Merging the datasets by SIC Codes and Major Groups
ODI <- merge(ODI, sicCodes, by = "SIC")
ODI <- merge(ODI, majorGroups, by = "Major.Group")
ODI <- ODI[,-17]

names(ODI)[names(ODI)=="Description.x"] <- "SIC.Description"
names(ODI)[names(ODI)=="Description.y"] <- "MajorGroup.Description"
names(ODI)[names(ODI)=="Division.x"] <- "Division"


#Aggregating the ODI data set to obtain the TCR mean, sd and count by Major Group
aggODI1 <- aggregate(TCR ~ MajorGroup.Description, data = ODI, FUN = mean)
aggODI2 <- aggregate(TCR ~ MajorGroup.Description, data = ODI, FUN = sd)
aggODI3 <- aggregate(TCR ~ MajorGroup.Description, data = ODI, FUN = length)

aggODI <- merge(aggODI1, aggODI2, by = "MajorGroup.Description")
aggODI <- merge(aggODI, aggODI3, by = "MajorGroup.Description")

names(aggODI)[names(aggODI)=="TCR.x"] <- "TCR.mean"
names(aggODI)[names(aggODI)=="TCR.y"] <- "TCR.sd"
names(aggODI)[names(aggODI)=="TCR"] <- "TCR.count"

#Calculating the coefficient of variation
aggODI$TCR.cv <- aggODI$TCR.sd/aggODI$TCR.mean

#Cleaning the dataset
aggODI <- aggODI[complete.cases(aggODI),]
aggODI <- aggODI[,-3]

#Extracting from aggODI the observation with a coefficient of variation over 2.5
aggODI.discard <- aggODI[aggODI$TCR.cv > 2.5,]
aggODI <- aggODI[aggODI$TCR.cv < 2.5,] 

#Hierarchical Clustering Analysis
testaggODI <- aggODI
testaggODI <- scale(testaggODI[,c(2,4)]) 

d = dist(testaggODI, method = "euclidean") 
hcward = hclust(d, method="ward.D")
aggODI$groups<-cutree(hcward,k=3) #we define 3 groups to segment the data

#Plotting the hierarchical clustering matrix
library(lattice) 

quartz()
xyplot(TCR.mean~ TCR.cv,main = "TCR (Total Case Rate) Clustering - US 2011", type="p", group=groups,data=aggODI,
       pch=c(19,19,19), col = c("blue", "red", "black"), ylab = "TCR mean", xlab = "TCR coefficient of variation")

#Saving the hierarchical clustering matrix
write.csv(file = "aggODI.csv", aggODI, row.names = FALSE)