##############################         DATA EXTRACTION       #######################
#############         Business File        ############
install.packages("jsonlite");
library("jsonlite")
#assign file to yelp variable
yelp<- "/Users/Shrenik/Desktop/Adv Data Science/Final Project/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_business.json"
#read in file
business <- stream_in(file(yelp)) 
#Flatten JSON file
business <- flatten(business,recursive = TRUE) 
#create dataframe that work with write
business<- data.frame(lapply(business, as.character), stringsAsFactors = FALSE)
#import csv file
write.csv(business, file = "/Users/Shrenik/Desktop/Adv Data Science/Final Project/Dataset/business.csv",row.names=TRUE, na="")
#############         Checkin File        ############
#assign file to yelp variable
yelp1<- "/Users/Shrenik/Desktop/Adv Data Science/Final Project/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_checkin.json"
#read in file
checkin <- stream_in(file(yelp1)) 
#Flatten JSON file
checkin <- flatten(checkin,recursive = TRUE) 
#create dataframe that work with write
checkin<- data.frame(lapply(checkin, as.character), stringsAsFactors = FALSE)
#import csv file
write.csv(checkin, file = "/Users/Shrenik/Desktop/Adv Data Science/Final Project/Dataset/checkin.csv",row.names=TRUE, na="")

#############         Reviews File        ############
#assign file to yelp variable
yelp2<- "/Users/Shrenik/Desktop/Adv Data Science/Final Project/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_review.json"
#read in file
reviews <- stream_in(file(yelp2)) 
#Flatten JSON file
reviews <- flatten(reviews,recursive = TRUE) 
#create dataframe that work with write
reviews<- data.frame(lapply(reviews, as.character), stringsAsFactors = FALSE)
#import csv file
write.csv(reviews, file = "/Users/Shrenik/Desktop/Adv Data Science/Final Project/Dataset/reviews.csv",row.names=TRUE, na="")

#############         User File        ############
#assign file to yelp variable
yelp3<- "/Users/Shrenik/Desktop/Adv Data Science/Final Project/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_user.json"
#read in file
user <- stream_in(file(yelp3)) 
#Flatten JSON file
user <- flatten(user,recursive = TRUE) 
#create dataframe that work with write
user<- data.frame(lapply(user, as.character), stringsAsFactors = FALSE)
#import csv file
write.csv(user, file = "/Users/Shrenik/Desktop/Adv Data Science/Final Project/Dataset/reviews.csv",row.names=TRUE, na="")
#############         Tips File        ############
#assign file to yelp variable
yelp4<- "/Users/Shrenik/Desktop/Adv Data Science/Final Project/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_tips.json"
#read in file
tips <- stream_in(file(yelp4)) 
#Flatten JSON file
tips <- flatten(tips,recursive = TRUE) 
#create dataframe that work with write
tips<- data.frame(lapply(tips, as.character), stringsAsFactors = FALSE)
#import csv file
write.csv(tips, file = "/Users/Shrenik/Desktop/Adv Data Science/Final Project/Dataset/tips.csv",row.names=TRUE, na="")

##############################         DATA CLEANING       #######################
###### create a business file of US restaurants which are open #######
business <- business[!grepl("FALSE",business$open),]
business <- business[grep("staurant", business$categories), ]
business <- business[!grepl("SCB",business$state),]
business <- business[!grepl("BW",business$state),]
business <- business[!grepl("RP",business$state),]
business <- business[!grepl("QC",business$state),]
business <- business[!grepl("ON",business$state),]
business <- business[!grepl("NW",business$state),]
business <- business[!grepl("NTH",business$state),]
business <- business[!grepl("MLN",business$state),]
business <- business[!grepl("KHL",business$state),]
business <- business[!grepl("HAM",business$state),]
business <- business[!grepl("FIF",business$state),]
business <- business[!grepl("ELN",business$state),]
business <- business[!grepl("EDH",business$state),]

###### new business subsets which contains the type of restaurant  ######
ethnicity_chinese <- business[grep("Chinese", business$categories), ]
ethnicity_german <- business[grep("German", business$categories), ]
ethnicity_american <- business[grep("American", business$categories), ]
ethnicity_indian <- business[grep("Indian", business$categories), ]
ethnicity_japanese <- business[grep("Japanese", business$categories), ]
ethnicity_korean <- business[grep("Korean", business$categories), ]
ethnicity_greek <- business[grep("Greek", business$categories), ]
ethnicity_italian <- business[grep("Italian", business$categories), ]
ethnicity_mexican <- business[grep("Mexican", business$categories), ]
ethnicity_irish <- business[grep("Irish", business$categories), ]
ethnicity_caribbean <- business[grep("Caribbean", business$categories), ]
ethnicity_pakistani <- business[grep("Pakistani", business$categories), ]
ethnicity_brazilian <- business[grep("Brazilian", business$categories), ]

###### Creates a new column type i.e. ethnicity type for each subset  ######
ethnicity_chinese$type <- rep("Chinese",nrow(ethnicity_chinese))
ethnicity_german$type <- rep("German",nrow(ethnicity_german))
ethnicity_american$type <- rep("American",nrow(ethnicity_american))
ethnicity_indian$type <- rep("Indian",nrow(ethnicity_indian))
ethnicity_japanese$type <- rep("Japanese",nrow(ethnicity_japanese))
ethnicity_korean$type <- rep("Korean",nrow(ethnicity_korean))
ethnicity_greek$type <- rep("Greek",nrow(ethnicity_greek))
ethnicity_italian$type <- rep("Italian",nrow(ethnicity_italian))
ethnicity_mexican$type <- rep("Mexican",nrow(ethnicity_mexican))
ethnicity_irish$type <- rep("Irish",nrow(ethnicity_irish))
ethnicity_caribbean$type <- rep("Caribbean",nrow(ethnicity_caribbean))
ethnicity_pakistani$type <- rep("Pakistani",nrow(ethnicity_pakistani))
ethnicity_brazilian$type <- rep("Brazilian",nrow(ethnicity_brazilian))

###### Merge subsets of business file which gives a dataset with ethnicity type  ######
business <- rbind(ethnicity_chinese,ethnicity_german,ethnicity_american,ethnicity_indian,ethnicity_japanese,ethnicity_korean,
                  ethnicity_greek,ethnicity_italian,ethnicity_mexican,ethnicity_irish,ethnicity_caribbean,ethnicity_pakistani,
                  ethnicity_brazilian)

# Function to remove empty character(0) from the list #
library(gtools)
removeEmptyCharacter <- function(x){
  if(invalid(x)) {
    x <- "dnr"
  } 
  return (x)
}
# Function to help conversion of list to vector #
removeEmptyCharacter2 <- function(x){ 
  t1 <- x
  if(invalid(x)) {
    x <- NA
  } 
  return (x)
}
# Replace character(0) in a neighbourhood column #
business$neighborhoods <- sapply(business$neighborhoods, removeEmptyCharacter)
# Convert list column to vector column as model does not accept list #
business$neighborhoods <- as.vector(business$neighborhoods,mode="character")
# Function to clean the names of the variables
clean.names <- function(df){
  colnames(df) <- gsub("attributes[^[:alnum:]]", "", colnames(df))
  colnames(df) <- gsub("[^[:alnum:]]", "", colnames(df))
  colnames(df) <- tolower(colnames(df))
  return(df)
}
#running the clean.name function which will give a new columns name : #
business <- clean.names(business)
colnames(business)
# Fixing business attributes #
business <- flatten(business, recursive = TRUE)
business <- clean.names(business)
# there are duplicates in business file so to remove duplicate variables #
business <- business[,!duplicated(colnames(business))]

# Convert acceptcreditcard column from list to vector #
temp <- sapply(business$acceptscreditcards, removeEmptyCharacter2)
temp <- as.vector(temp,mode="logical")
business$acceptscreditcards <- temp
#colnames(business)

# We will leave out business id, full address , name, lattitue, longitude information. 
business <- business[c(-2,-3,-4,-7,-8,-9,-12)]
business <- business[c(1:6,21:91)]

#replace missing values by "dnr"
business[is.na(business)] <- "dnr"
# rename ambiguous columns as we will be joining business and review file
#install.packages("data.table")
library(data.table)
setnames(reviews, old=c("votes.funny","votes.useful"), new=c("funny", "useful"))
#Remove the reviews which do not have useful vote
library(sqldf)
review1 <- sqldf("SELECT * FROM reviews WHERE useful >= '1'")
#calculate weighted star rating on basis of useful vote
review2 <- sqldf("SELECT business_id,Count(stars) AS reviewcount, SUM(stars * useful) / SUM(useful) AS Weighted_stars
                 FROM review1 GROUP BY business_id ")
#merge business and review data & calcualte the weighted factor for ambience,parking,goodforfood and music
business_review1 <-  sqldf("SELECT type, city, state, stars, Weighted_stars, 
                           review2.reviewcount AS userreviewcount, pricerange, 
                           waiterservice, wheelchairaccessible, hastv, alcohol, takeout, attire, goodforkids,
                           goodfordancing, goodforgroups,
                           
                           CASE WHEN parkinggarage='TRUE' THEN 1 ELSE 0 END
                           + CASE WHEN parkingstreet='TRUE' THEN 1 ELSE 0 END 
                           + CASE WHEN parkingvalidated= 'TRUE'   THEN 1 ELSE 0 END 
                           + CASE WHEN parkinglot= 'TRUE'   THEN 1 ELSE 0 END 
                           + CASE WHEN parkingvalet= 'TRUE'   THEN 1 ELSE 0 END 
                           AS parking,
                           
                           CASE WHEN goodfordessert='TRUE' THEN 1 ELSE 0 END
                           + CASE WHEN goodforlatenight='TRUE' THEN 1 ELSE 0 END 
                           + CASE WHEN goodforlunch= 'TRUE'   THEN 1 ELSE 0 END 
                           + CASE WHEN goodfordinner= 'TRUE'   THEN 1 ELSE 0 END 
                           + CASE WHEN goodforbrunch= 'TRUE'   THEN 1 ELSE 0 END 
                           + CASE WHEN goodforbreakfast= 'TRUE'   THEN 1 ELSE 0 END 
                           AS goodforfood,
                           
                           CASE WHEN ambienceromantic='TRUE' THEN 1 ELSE 0 END
                           + CASE WHEN ambienceintimate= 'TRUE'   THEN 1 ELSE 0 END 
                           + CASE WHEN ambienceclassy= 'TRUE'   THEN 1 ELSE 0 END 
                           + CASE WHEN ambiencehipster= 'TRUE'   THEN 1 ELSE 0 END 
                           + CASE WHEN ambiencedivey='TRUE' THEN 1 ELSE 0 END 
                           + CASE WHEN ambiencetouristy= 'TRUE'   THEN 1 ELSE 0 END 
                           + CASE WHEN ambiencetrendy= 'TRUE'   THEN 1 ELSE 0 END 
                           + CASE WHEN ambienceupscale= 'TRUE'   THEN 1 ELSE 0 END 
                           + CASE WHEN ambiencecasual= 'TRUE'   THEN 1 ELSE 0 END 
                           AS ambience,
                           
                           CASE WHEN musicdj='TRUE' THEN 1 ELSE 0 END
                           + CASE WHEN musicbackgroundmusic='TRUE' THEN 1 ELSE 0 END 
                           + CASE WHEN musicjukebox= 'TRUE'   THEN 1 ELSE 0 END
                           +  CASE WHEN musiclive='TRUE' THEN 1 ELSE 0 END
                           + CASE WHEN musicvideo='TRUE' THEN 1 ELSE 0 END 
                           + CASE WHEN musickaraoke= 'TRUE'   THEN 1 ELSE 0 END AS music
                           
                           FROM business
                           INNER JOIN review2 ON business.businessid = review2.business_id ")
# make all character columns as factors
business_review1[sapply(business_review1, is.character)] <- lapply(business_review1[sapply(business_review1, is.character)],  as.factor)
# make all factors as numeric variables
business_review1[sapply(business_review1, is.factor)] <- lapply(business_review1[sapply(business_review1, is.factor)],  as.numeric)
# make star rating as factor
business_review1$stars <- as.factor(business_review1$stars)
str(business_review1)

###################################   DATA MODELING (RANDOM FOREST)  ################################### 
library("caret")
set.seed(123)
inTrain <- createDataPartition(business_review1$stars, p=0.70, list=F)
trainData <- business_review1[inTrain, ]
crossValidationData <- business_review1[-inTrain, ]
dim(trainData)
na.omit(trainData)
install.packages("randomForest")
library("randomForest")
# fit random forest
rf.1 <- randomForest(as.factor(stars) ~. , data=trainData, importance=TRUE, ntree=500)

#### calculate variable importance ####
imp<-importance(rf.1)
vars<-dimnames(imp)[[1]]
imp<-data.frame(vars=vars,imp=as.numeric(imp[,1]))
imp<-imp[order(imp$imp,decreasing=T),]
imp

# Select important variables for retraining
#set.seed(42)
selected<-c(as.character(imp[1:17,1]),'stars')
#selected

# train again
rf.2<-randomForest(stars~.,data=trainData[,selected],replace=T,ntree=500)
# Peform prediction

#### plot variable importance ####
varImpPlot(rf.1,main='Variable Importance : Final Model',pch=16,col='blue')
predict_rf <- predict(rf.1, crossValidationData, type = "class")
install.packages("e1071")
library("e1071")
confusion_matrix <- confusionMatrix(predict_rf,crossValidationData$stars)
confusion_matrix

# Create prediction table
all.predictions <- data.frame(actual=crossValidationData$stars,random.forest = predict_rf)
all.predictions <- gather(all.predictions,key = model,value = predictions,2)
all.predictions$actual <- as.numeric(as.character( all.predictions$actual ))
all.predictions$predictions <- as.numeric(as.character( all.predictions$predictions ))
all.predictions$difference <- all.predictions$predictions-all.predictions$actual
str(all.predictions)


##plot the predicition difference
g <- ggplot(all.predictions)
g <- g + geom_histogram(aes(x=difference),fill="orange",binwidth=.3)
g <-  g + ggtitle("Difference between predicted and actual rating")
g <- g + ylab("Count") + xlab("Predicted - Actual")
g

#Calculate the percentage as if how many predictions where within +1 and -1 range
difference <- as.data.frame(table(all.predictions$difference))
difference
(difference[2,2]+difference[3,2]+difference[4,2]+difference[5,2]+difference[6,2])/sum(difference[,2])
