hunterdata <- read.csv("~/Downloads/Hunter Douglas Quality Data.csv")

#Only Shades from plant B (ST)
rollerB <- hunterdata[hunterdata$ORIGINAL_PLANT == 'B',]
rollerB <- rollerB[rollerB$PRODUCT_CATEGORY == '07 Roller Shades',]
#Only STD REM and REP
rollerB <- rollerB[rollerB$ORDER_REASON_ID == 'STD' | rollerB$ORDER_REASON_ID == 'REM' | rollerB$ORDER_REASON_ID == 'REP' ,]
## Removed NULL order lines
rollerB <- rollerB[rollerB$ORIGINAL_ORDER != 'NULL' & rollerB$SALES_ORDER_LINE != 'NULL',]

#Created smahed column to match original orders with remake/repair orders

rollerB$smashed <- paste0(rollerB$ORIGINAL_ORDER, rollerB$SALES_ORDER_LINE)

orderequal <- (rollerB[rollerB$ORIGINAL_ORDER == rollerB$SALES_ORDER,])

ordernotequal <- (rollerB[!rollerB$ORIGINAL_ORDER == rollerB$SALES_ORDER,])

orderequal$smashed <- paste0(orderequal$ORIGINAL_ORDER, orderequal$SALES_ORDER_LINE)
ordernotequal$smashed <- paste0(ordernotequal$ORIGINAL_ORDER, ordernotequal$ORIGINAL_ORDER_LINE)

#flagged problem orders

flagorders <- ordernotequal$smashed


orderequal$flagged <- ifelse(orderequal$smashed %in% ordernotequal$smashed,1,0)

sum(orderequal$flagged)

ordernotequal$flagged <- 0

ordernotequal$missing <- ifelse(!ordernotequal$smashed %in% orderequal$smashed,1,0)




orderequal$missing <- 0


#created new csv with flagged problems

newcsv <- rbind(orderequal, ordernotequal)

names(newcsv)[length(names(newcsv))] <- 'flagged2'

head(newcsv)

sum(newcsv$flagged)

write.csv(newcsv,"~/Downloads/HDrollerB.csv", row.names = TRUE)


newHD <- read.csv("~/Downloads/HDrollerB.csv")

flagged <- newHD[newHD$flagged == 1,]


head(rollerB[rollerB$ORIGINAL_ORDER == rollerB$SALES_ORDER,],100)

newHD <- read.csv("~/Downloads/HDrollerB.csv")
flagged <- newHD[newHD$flagged == 1,]

#created new date column to compare 90 days

newdate <- as.Date(as.factor(newHD$SO_CREATED_DATE), "%Y%m%d")

newHD$newdate <- newdate

head(newHD)

str(newHD$newdate[1] - newHD$newdate[2])

newHD$SO_CREATED_DATE

difftime(newHD$newdate[1], newHD$newdate[2], units = c("days"))



flagged <- newHD[newHD$flagged == 1,]

nrow(flagged)

#labeled mistakes

mistakes <- newHD[newHD$ORIGINAL_ORDER != newHD$SALES_ORDER,]


#matched original with mistakes

orderedmistakes <- mistakes[order(mistakes$newdate),]
head(orderedmistakes)
noduplicates <- orderedmistakes[!duplicated(orderedmistakes$smashed),] 

rowspulling <- match(flagged$smashed, noduplicates$smashed)
tail(orderedmistakes)



nrow(noduplicates)

head(noduplicates)
flagged$datediff <- 999

for( i in 1:nrow(flagged)){
	flagged[i]$datedif <- flagged$newdate[i] - noduplicates[noduplicates $smashed == flagged$smashed[i],]$newdate}
}

rowspulling <- match(flagged$smashed, noduplicates$smashed)

pulledremakes <- noduplicates[rowspulling,]


flagged$datedifference <- as.numeric(pulledremakes$newdate - flagged$newdate)

##flagged if within 90 days

flagged$predictorcolumn <- ifelse(flagged$datedifference <= 90,1,0) 


originalorders <- newHD[newHD$ORIGINAL_ORDER == newHD$SALES_ORDER,]



flaggedrows <- match(flagged[flagged$predictor==1,]$smashed, originalorders$smashed)

#create final predictor column

originalorders$predictorcolumn <- 0

originalorders[flaggedrows,]$predictorcolumn <- 1

#deleted unnecessary columns

originalorders$flagged <- NULL
originalorders$missing <- NULL

head(originalorders)



#############################################
# level down categorical variables
#############################################
predcols = hdprediction
# COLOR_ID
color49 = names(sort(table(predcols$COLOR_ID), decreasing = T)[1:49])
predcols$COLOR_ID50 = ifelse(as.character(predcols$COLOR_ID) %in% color49,as.character(predcols$COLOR_ID), "other" )
as.matrix(table(predcols$COLOR_ID50))
predcols$COLOR_ID50 = as.factor(predcols$COLOR_ID50)
# FABRIC_ID
fabric49 = names(sort(table(predcols$FABRIC_ID), decreasing = T)[1:49])
predcols$FABRIC_ID50 = ifelse(as.character(predcols$FABRIC_ID) %in% fabric49,as.character(predcols$FABRIC_ID), "other" )
as.matrix(table(predcols$FABRIC_ID50))
predcols$FABRIC_ID50 = as.factor(predcols$FABRIC_ID50)
# SOLD_TO_ID
soldto49 = names(sort(table(predcols$SOLD_TO_ID), decreasing = T)[1:49])
predcols$SOLD_TO_ID50 = ifelse(as.character(predcols$SOLD_TO_ID) %in% soldto49,as.character(predcols$SOLD_TO_ID), "other" )
as.matrix(table(predcols$SOLD_TO_ID50))
predcols$SOLD_TO_ID50 = as.factor(predcols$SOLD_TO_ID50)

# write.csv(predcols,"HDprediction50Levels.csv", row.names = TRUE)



sum(originalorders$predictorcolumn)
top50 <- read.csv('~/Downloads/HDprediction50Levels.csv')
newHD <- read.csv("~/Downloads/HDrollerB.csv")
mistakes <- newHD[newHD$ORIGINAL_ORDER != newHD$SALES_ORDER,]
originalmistakes <- top50[top50$predictorcolumn == 1,]
originalnonmistakes <- top50[top50$predictorcolumn == 0,]
mistakesmatched <- (mistakes[match(originalmistakes$smashed, mistakes$smashed),])
nrow(newHD)

head(mistakesmatched)
head(originalmistakes)
originalmistakes$remake <- 0
originalmistakes$repair <- 0
originalnonmistakes$remake <- 0
originalnonmistakes$repair <- 0


#label repair and remakes

for(i in 1:nrow(originalmistakes)){
	if(mistakesmatched$ORDER_REASON_ID[i] == 'REM'){
		originalmistakes$remake[i] <- 1}
	if(mistakesmatched$ORDER_REASON_ID[i] == 'REP'){
		originalmistakes$repair[i] <- 1
	}
	}


#combine full data

fulldata <- rbind(originalmistakes, originalnonmistakes)

regstate49 = names(sort(table(fulldata$REGION_STATE_ID), decreasing = T)[1:49])
fulldata$REGION_STATE_ID50 = ifelse(as.character(fulldata$REGION_STATE_ID) %in%
regstate49,as.character(fulldata$REGION_STATE_ID), "other" )
as.matrix(table(fulldata$REGION_STATE_ID))
fulldata$REGION_STATE_ID = as.factor(fulldata$REGION_STATE_ID)

table(originalmistakes$REGION_STATE_ID50)
nrow(fulldata)
head(fulldata)

##write final csv for modeling
write.csv(fulldata,"~/Downloads/RRpredictions.csv", row.names = TRUE)

