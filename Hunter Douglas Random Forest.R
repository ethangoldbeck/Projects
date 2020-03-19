#Load in Data

final <- read.csv('~/Downloads/RRpredictions.csv')

#Create Area Column

final$area <- as.numeric(final$HEIGHT) * as.numeric(final$WIDTH)

#Change columns to factors/numeric

final$REGIONAL_SALES_MGR_ID <- as.factor(final$REGIONAL_SALES_MGR_ID)
final$HEIGHT <- as.numeric(final$HEIGHT)
final$WIDTH <- as.numeric(final$WIDTH)

#Train and tests sets (80%/20%) 

set.seed(123, sample.kind = 'Rounding')
training <- sample(1:nrow(final), nrow(final) * .8)

final.train <- final[training,]
final.test <- final[-training,]
nrow(final.test)
nrow(final.train)


#Random Forest

library(randomForest)
HDforest <- randomForest(as.factor(predictorcolumn) ~ REGIONAL_SALES_MGR_ID + ALLIANCE_LEVEL_ID + FABRIC_ID50 + COLOR_ID50 + OPERATING_SYSTEM_ID + OPERATING_SYS_OPT_ID + ORIGINAL_MATERIAL_ID + HEIGHT + WIDTH + SOLD_TO_ID50 + REGION_STATE_ID50,  data = final.train, ntree = 200, mtry = 6, nodesize = 1)

pred.HD <- predict(HDforest, newdata = final.test, cutoff = c(.65,1-.65))

#Confusion Matrix

table(final.test$predictorcolumn, pred.HD)*5

pred.allHD <- predict(HDforest, newdata = final, cutoff = c(.65,1-.65))

sum(final$predictorcolumn)               
cbound <- cbind(final.test, pred.HD)                    


head(cbound)
nrow(cbound[cbound$remake==1 & cbound$pred.HD == 1, ])*5
nrow(cbound[cbound$repair==1 & cbound$pred.HD == 1, ])*5


