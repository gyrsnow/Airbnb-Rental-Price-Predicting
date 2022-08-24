setwd('/Users/gyr/Desktop/5200/Kaggle/rentlala2021')
data = read.csv('analysisData.csv',na.strings = c("","NA","N/A",NA," "), stringsAsFactors=T)
scoringData = read.csv('scoringData.csv', na.strings = c("","NA","N/A",NA, " "), stringsAsFactors=T)

library(dplyr)
library(tidyverse)
library(ggplot2)
library(caret)
library(readr)
library(rpart)
library(rpart.plot)
library(naniar)

#####Initial Exploration#####
## INITIAL EXPLORATION STARTS HERE
#Missing value
library(visdat)
vis_miss(data,warn_large_data = F)
vis_miss(scoringData,warn_large_data = F)

#Remove text-content description variables
str(data)
names(data)
clean = data[,-c(2:12,14:16,20,23,26,27,28,30,31,34,35,36,45,62,80,81)]

#Remove single result variables
unique(clean$has_availability) 
unique(clean$requires_license)   
unique(clean$is_business_travel_ready)
clean = clean[,-which(names(clean) %in% c("has_availability","requires_license","is_business_travel_ready"))]

#Remove outliers
clean = clean[clean$price>0,]
clean = clean[clean$bedrooms <= 10,]

#Convert variable type
library(stringr)
clean$first_review = as.Date(clean$first_review)
clean$last_review = as.Date(clean$last_review)
clean$host_since = as.Date(clean$host_since)
## INITIAL EXPLORATION ENDS HERE

#####Data Cleaning#####
## DATA CLEANING STARTS HERE
#Missing Value greater than 20%
library(naniar)
miss_var_summary(clean)
clean = clean[,-which(names(clean) %in% c("monthly_price","square_feet","weekly_price","host_acceptance_rate","security_deposit","host_response_rate"))]
#'host_listings_count' is the same with 'host_total_listings_count', I choose to remove former one
clean = clean[,-which(names(clean) %in% "host_listings_count")]

#Clean Variables with a small amount of missing value
miss_var_summary(clean)
a = c()
for(col in 1:ncol(clean)) {
  if (length(which(is.na(clean[,col]))) >0) {
    a = c(a,col)
  } 
}
print(a) #(2  3  4  5  6  8  9 16 19 36 37 53)
names(clean[c(2,3,4,5,6,8,9,16,19,36,37,53)])

#a.Clean NA of non-numeric variables 
#1.host_is_superhost
which(is.na(clean$host_is_superhost))
table(clean$host_is_superhost) 
clean$host_is_superhost[is.na(clean$host_is_superhost)] = as.factor("f")
table(clean$host_is_superhost)

#2.host_has_profile_pic 
which(is.na(clean$host_has_profile_pic))
table(clean$host_has_profile_pic)
clean$host_has_profile_pic[is.na(clean$host_has_profile_pic)] = as.factor("t")
table(clean$host_has_profile_pic)

#3.host_identity_verified
which(is.na(clean$host_identity_verified))
table(clean$host_identity_verified) 
clean$host_identity_verified[is.na(clean$host_identity_verified)] = as.factor("f")
table(clean$host_identity_verified)

#4.market
which(is.na(clean$market))
table(clean$market) 
clean$market[is.na(clean$market)] = as.factor("New York")
table(clean$market)

#b.Clean Date
library(lubridate)
clean$host_since = year(clean$host_since)
clean$first_review = year(clean$first_review)
clean$last_review = year(clean$last_review)

#c.Clean NA of numeric variables
miss_var_summary(clean)
length(which(is.na(clean$cleaning_fee)))
length(which(is.na(clean$zipcode)))

#Impute method to fill NA of numeric variables
library(caret)
set.seed(1000)
pp = predict(preProcess(clean[,c("cleaning_fee","beds","host_total_listings_count","reviews_per_month","host_since","first_review","last_review")],method = "bagImpute"),clean[,c("cleaning_fee","beds","host_total_listings_count","reviews_per_month","host_since","first_review","last_review")])
clean$cleaning_fee =pp$cleaning_fee
clean$beds =pp$beds
clean$host_total_listings_count = pp$host_total_listings_count
clean$reviews_per_month =pp$reviews_per_month

clean$host_since = as.factor(floor(pp$host_since))
clean$first_review = as.factor(floor(pp$first_review))
clean$last_review = as.factor(floor(pp$last_review))

#d.Encode Missing Data of zipcode
levels(cleanData$zipcode) #After running Linear Regression, the test data that don’t appear in the training data
clean$zipcode = str_remove(clean$zipcode, "NY ")
clean$zipcode = str_remove(clean$zipcode, "-3233")
clean$zipcode = str_remove(clean$zipcode, "-2308")
clean$zipcode[which(clean$zipcode == 1009)] = "10009"
clean$zipcode[clean$zipcode == 11559] =11581
clean$zipcode[is.na(clean$zipcode)] = "1"
clean$zipcode = as.factor(clean$zipcode)
cleanData = clean
sum(is.na(cleanData))


#####Clean scoringData #####
scoringclean = scoringData[,-c(2:12,14:16,20,23,26,27,28,30,31,34,35,36,45,61,79,80)]
scoringclean = scoringclean[,-which(names(scoringclean) %in% c("has_availability", "requires_license","is_business_travel_ready",
                                                               "host_response_rate","host_acceptance_rate","square_feet", "weekly_price",
                                                               "monthly_price", "security_deposit","host_listings_count"))]
#Covert variable type
scoringclean$first_review = as.Date(scoringclean$first_review)
scoringclean$last_review = as.Date(scoringclean$last_review)
scoringclean$host_since = as.Date(scoringclean$host_since)


#Clean Variables with a small amount of missing value
miss_var_summary(scoringclean)
b = c()
for(col in 1:ncol(scoringclean)) {
  if (length(which(is.na(scoringclean[,col]))) >0) {
    b = c(b,col)
  } 
}
print(b) #( 2  3  4  5  6  8  9 16 18)
names(clean[c(2,3,4,5,6,8,9,16,18)])

#Clean NA of non-numeric variables 
#1.host_is_superhost
which(is.na(scoringclean$host_is_superhost))
table(scoringclean$host_is_superhost) 
scoringclean$host_is_superhost[is.na(scoringclean$host_is_superhost)] = as.factor("f")
table(scoringclean$host_is_superhost)

#2.host_has_profile_pic 
which(is.na(scoringclean$host_has_profile_pic))
table(scoringclean$host_has_profile_pic) 
scoringclean$host_has_profile_pic[is.na(scoringclean$host_has_profile_pic)] = as.factor("t")
table(scoringclean$host_has_profile_pic)

#3.host_identity_verified
which(is.na(scoringclean$host_identity_verified))
table(scoringclean$host_identity_verified) 
scoringclean$host_identity_verified[is.na(scoringclean$host_identity_verified)] = as.factor("f")
table(scoringclean$host_identity_verified)

#4.market
which(is.na(scoringclean$market))
table(scoringclean$market) 
scoringclean$market[is.na(scoringclean$market)] = as.factor("New York")
table(scoringclean$market)

#b.Clean Date
library(lubridate)
scoringclean$host_since = year(scoringclean$host_since)
scoringclean$first_review = as.factor(year(scoringclean$first_review))
scoringclean$last_review = as.factor(year(scoringclean$last_review))

#c.Clean NA of numeric variables
miss_var_summary(scoringclean)
library(caret)
set.seed(1000)
pp2 = predict(preProcess(scoringclean[,c("cleaning_fee","beds","host_total_listings_count","host_since")],method = "bagImpute"),scoringclean[,c("cleaning_fee","beds","host_total_listings_count","host_since")])

scoringclean$cleaning_fee =pp2$cleaning_fee
scoringclean$beds =pp2$beds
scoringclean$host_total_listings_count = pp2$host_total_listings_count

scoringclean$host_since = as.factor(floor(pp2$host_since))

#d.Encode Missing Data of zipcode
scoringclean$zipcode = str_remove(scoringclean$zipcode, "NY ")
scoringclean$zipcode = str_remove(scoringclean$zipcode, "-3220")
scoringclean$zipcode[606]= "11249"
scoringclean$zipcode[scoringclean$zipcode == 11003] = 11103
scoringclean$zipcode[is.na(scoringclean$zipcode)] = "1"
scoringclean$zipcode = as.factor(scoringclean$zipcode)

scoringcleanData = scoringclean
sum(is.na(scoringcleanData))


#Change the factor variable levels to make the two datasets consistent
str(cleanData)
str(scoringcleanData)

levels(cleanData$market)
levels(scoringcleanData$market)

levels(cleanData$zipcode)
levels(scoringcleanData$zipcode)

levels(cleanData$property_type)
levels(scoringcleanData$property_type)


#Remove market
model1 = lm(price~market+property_type, data = cleanData)
summary(model1) #market is not significant variable
cleanData = cleanData[,-which(names(cleanData) %in% "market")]
scoringcleanData = scoringcleanData[,-which(names(scoringcleanData) %in% "market")]

##property_type aggregation
str(cleanData$property_type)
str(scoringcleanData$property_type)  
table(cleanData$property_type)
table(scoringcleanData$property_type)

#Combine ('Barn','Boat','Bungalow', Cabin','Camper/RV',Casa particular (Cuba)','Castle','Cave','Cottage','Island','Nature lodge','Resort','Tent','Villa') to 'Other'
levels(cleanData$property_type)[c(3:5,7:12,14,23,25,32,27,29,33)] = 'Other'
#Combine ('Aparthotel','Boutique hotel ','Hostel','Hotel')to 'Hotel'
levels(cleanData$property_type)[c(1,4,10,11)] = 'Hotel'
#Combine ('Dome house','Earth house','Guesthouse','Houseboat','Tiny house',) to 'House'
levels(cleanData$property_type)[c(5,6,8:10,13)] = 'House'
#Combine('Serviced apartment','Apartment') to 'Apartment'
levels(cleanData$property_type)[8] = 'Apartment'
str(cleanData$property_type)

#Combine ('Lighthouse','Timeshare') to 'Other'
levels(scoringcleanData$property_type)[c(3,4,6:9,11,20,23,25,26,29)] = 'Other'
#Combine to 'Hotel'
levels(scoringcleanData$property_type)[c(1,4,10,11)] = 'Hotel'
#Combine to 'House'
levels(scoringcleanData$property_type)[c(5,6,8,9,10,13)] = 'House'
#Combine to 'Apartment'
levels(scoringcleanData$property_type)[8] = 'Apartment'
str(scoringcleanData$property_type)
## DATA CLEANING ENDS HERE

#####Models and Feature Selection#####
## MODELING AND FEATURE SELECTION STARTS HERE
###Variables Filter Methods###
#Bivariate Filter
str(cleanData)
names(cleanData)
cor(cleanData[,c(4,12:15,17:34,37:43,48:52)])
library(tidyr); library(dplyr); library(ggplot2)
corMatrix = as.data.frame(cor(cleanData[,c(4,12:15,17:34,37:43,48:52)]))
corMatrix$var1 = rownames(corMatrix)

corMatrix %>%
  gather(key=var2,value=r,1:35)%>%
  arrange(var1,desc(var2))%>%
  ggplot(aes(x=var1,y=reorder(var2, order(var2,decreasing=F)),fill=r))+
  geom_tile()+
  geom_text(aes(label=round(r,2)),size=3)+
  scale_fill_gradientn(colours = c('#d7191c','#fdae61','#ffffbf','#a6d96a','#1a9641'))+
  theme(axis.text.x=element_text(angle=75,hjust = 1))+xlab('')+ylab('')

#Non-redundant and variance inflating factor check
model = lm(price~.-host_since- host_is_superhost- host_has_profile_pic- host_identity_verified
           -neighbourhood_group_cleansed- zipcode- is_location_exact- property_type- room_type- bed_type 
           -first_review- last_review- instant_bookable- cancellation_policy
           -require_guest_phone_verification- require_guest_phone_verification,cleanData)
library(broom)
summary(model) %>%
  tidy()
library(car)
vif(model)
data.frame(Predictor = names(vif(model)), VIF = vif(model)) %>%
  ggplot(aes(x=VIF, y = reorder(Predictor, VIF), fill=VIF))+
  geom_col()+
  geom_vline(xintercept= 5, color = 'gray', size = 1.5)+
  geom_vline(xintercept = 10, color = 'red', size = 1.5)+
  scale_fill_gradient(low = '#fff7bc', high = '#d95f0e')+
  scale_y_discrete(name = "Predictor")+
  theme_classic()
#Remove(maximum_nights_avg_ntm,minimum_maximum_nights,maximum_maximum_nights)
cleanData = cleanData[,-which(names(cleanData) %in% c('maximum_nights_avg_ntm','minimum_maximum_nights','maximum_maximum_nights'))]
scoringcleanData = scoringcleanData[,-which(names(scoringcleanData) %in% c('maximum_nights_avg_ntm','minimum_maximum_nights','maximum_maximum_nights'))]


library(caret)
set.seed(1000)
split = createDataPartition(y=cleanData$price,p = 0.8,list = F,groups = 50)
train = cleanData[split,]
test = cleanData[-split,]

## FEATURE SELECTION 1: Stepwise Variable Selection STARTS HERE
##Stepwise Variable Selection
start_mod = lm(price~1,train)
empty_mod = lm(price~1,train)
full_mod = lm(price~.,train)
hybridStepwise = step(start_mod,
                      scope=list(upper=full_mod,lower=empty_mod),
                      direction='both')
summary(hybridStepwise)
##Hybrid result
summary(lm(formula = price ~ accommodates + zipcode + cleaning_fee + 
     room_type + bathrooms + bedrooms + property_type + availability_90 + 
     minimum_minimum_nights + review_scores_cleanliness + number_of_reviews_ltm + 
     guests_included + last_review + host_is_superhost + beds + 
     neighbourhood_group_cleansed + first_review + calculated_host_listings_count + 
     review_scores_rating + review_scores_value + instant_bookable + 
     calculated_host_listings_count_shared_rooms + host_since + 
     availability_365 + is_location_exact + cancellation_policy + 
     minimum_nights_avg_ntm + host_total_listings_count + maximum_minimum_nights + 
     extra_people + review_scores_checkin, data = train))
## FEATURE SELECTION 1: Stepwise Variable Selection ENDS HERE

## FEATURE SELECTION 2: Lasso Regression STARTS HERE
##Lasso Regression
library(glmnet)
x = model.matrix(price ~.,data = train)
y = train$price
set.seed(1000)
cv_lasso = cv.glmnet(x = x, y = y, alpha = 1,type.measure = 'mse')
plot(cv_lasso)
bestlam <- cv_lasso$lambda.min
coef(cv_lasso, s = cv_lasso$lambda.1se) %>%
  round(4)
#Lasso result
summary(lm(price~host_since+host_is_superhost+neighbourhood_group_cleansed+zipcode+is_location_exact
           +property_type+room_type+accommodates+bathrooms+bedrooms+cleaning_fee+guests_included
           +extra_people+minimum_nights+minimum_minimum_nights+minimum_nights_avg_ntm+availability_60
           +availability_90+availability_365+number_of_reviews+number_of_reviews_ltm+first_review
           +last_review+review_scores_rating+review_scores_cleanliness+review_scores_location+instant_bookable
           +instant_bookable+calculated_host_listings_count_private_rooms+calculated_host_listings_count_shared_rooms 
           ,data = train))
## FEATURE SELECTION 2: Lasso Regression ENDS HERE

##MODEL 1
## MODEL 1: LINEAR REGRESSION WITH FEATURES FROM FEATURE SELECTION 1 STARTS HERE
lm_hy = lm(price ~ accommodates + zipcode + cleaning_fee + 
                room_type + bathrooms + bedrooms + property_type + availability_90 + 
                minimum_minimum_nights + review_scores_cleanliness + number_of_reviews_ltm + 
                guests_included + last_review + host_is_superhost + beds + 
                neighbourhood_group_cleansed + first_review + calculated_host_listings_count + 
                review_scores_rating + review_scores_value + instant_bookable + 
                calculated_host_listings_count_shared_rooms + host_since + 
                availability_365 + is_location_exact + cancellation_policy + 
                minimum_nights_avg_ntm + host_total_listings_count + maximum_minimum_nights + 
                extra_people + review_scores_checkin
              ,data = train)
summary(lm_hy)

pred_lmhy = predict(lm_hy,newdata = test)
rmse_lmhy = sqrt(mean((pred_lmhy - test$price)^2));rmse_lmhy
#train:70.45943
#test:73.80931
#kaggle:69.05165
## MODEL 1: LINEAR REGRESSION WITH FEATURES FROM FEATURE SELECTION 1 ENDS HERE

## MODEL 1: LINEAR REGRESSION WITH FEATURES FROM FEATURE SELECTION 2 STARTS HERE
lm_lasso = lm(price~host_since+host_is_superhost+neighbourhood_group_cleansed+zipcode+is_location_exact
                 +property_type+room_type+accommodates+bathrooms+bedrooms+cleaning_fee+guests_included
                 +extra_people+minimum_nights+minimum_minimum_nights+minimum_nights_avg_ntm+availability_60
                 +availability_90+availability_365+number_of_reviews+number_of_reviews_ltm+first_review
                 +last_review+review_scores_rating+review_scores_cleanliness+review_scores_location+instant_bookable
                 +instant_bookable+calculated_host_listings_count_private_rooms+calculated_host_listings_count_shared_rooms 
                 ,data = train)
summary(model_lasso)

pred_lmlasso = predict(lm_lasso,newdata = test)
rmse_lmlasso = sqrt(mean((pred_lmlasso- test$price)^2));rmse_lmlasso
#train:70.61456
#test:73.89937
#kaggle:69.01543
## MODEL 1: LINEAR REGRESSION WITH FEATURES FROM FEATURE SELECTION 2 ENDS HERE

## MODEL 2
## MODEL 2: DECISION TREE WITH FEATURES FROM FEATURE SELECTION 1 STARTS HERE
tree_default1 = rpart(price ~ accommodates + zipcode + cleaning_fee + 
                       room_type + bathrooms + bedrooms + property_type + availability_90 + 
                       minimum_minimum_nights + review_scores_cleanliness + number_of_reviews_ltm + 
                       guests_included + last_review + host_is_superhost + beds + 
                       neighbourhood_group_cleansed + first_review + calculated_host_listings_count + 
                       review_scores_rating + review_scores_value + instant_bookable + 
                       calculated_host_listings_count_shared_rooms + host_since + 
                       availability_365 + is_location_exact + cancellation_policy + 
                       minimum_nights_avg_ntm + host_total_listings_count + maximum_minimum_nights + 
                       extra_people + review_scores_checkin
                     ,data=train,method = 'anova',control = rpart.control(cp = 0.001))
pred_default = predict(tree_default1 ,newdata=test)
rmse_tree_default  = sqrt(mean((pred_default -test$price)^2)); rmse_tree_default

#train:66.00471
#test:74.95212
#kaggle:71.82747
## MODEL 2: DECISION TREE WITH FEATURES FROM FEATURE SELECTION 1 ENDS HERE

## MODEL 2: DECISION TREE WITH FEATURES FROM FEATURE SELECTION 2 STARTS HERE
tree_default2 = rpart(price~host_since+host_is_superhost+neighbourhood_group_cleansed+zipcode+is_location_exact
                     +property_type+room_type+accommodates+bathrooms+bedrooms+cleaning_fee+guests_included
                     +extra_people+minimum_nights+minimum_minimum_nights+minimum_nights_avg_ntm+availability_60
                     +availability_90+availability_365+number_of_reviews+number_of_reviews_ltm+first_review
                     +last_review+review_scores_rating+review_scores_cleanliness+review_scores_location+instant_bookable
                     +instant_bookable+calculated_host_listings_count_private_rooms+calculated_host_listings_count_shared_rooms 
                     ,data = train,method = 'anova',control = rpart.control(cp = 0.001))
pred_default_lasso = predict(tree_default2,newdata=test)
rmse_tree_default_lasso  = sqrt(mean((pred_default_lasso -test$price)^2)); rmse_tree_default_lasso
#train:65.95381
#test:75.10524
#kaggle:70.86210
## MODEL 2: DECISION TREE WITH FEATURES FROM FEATURE SELECTION 2 ENDS HERE

##MODEL 3
## MODEL 3: BOOSTING WITH XGBOOST FROM FEATURE SELECTION 1 STARTS HERE
library(vtreat)
trt = designTreatmentsZ(dframe = train,
                        varlist = names(train)[c(2,3,4,7:15,18,28,29,23,24,36,33,32,34,40,41,42,25,45,31,19,20,37,48)])
newvars = trt$scoreFrame[trt$scoreFrame$code%in% c('clean','lev'),'varName']

train_input = prepare(treatmentplan = trt, 
                      dframe = train,
                      varRestriction = newvars)
test_input = prepare(treatmentplan = trt, 
                     dframe = test,
                     varRestriction = newvars)

library(xgboost); library(caret)
set.seed(1000)
tune_nrounds = xgb.cv(data=as.matrix(train_input), 
                      label = train$price,
                      nrounds=250,
                      nfold = 5,
                      verbose = 0)
xgboost2= xgboost(data=as.matrix(train_input), 
                  label = train$price,
                  nrounds=which.min(tune_nrounds$evaluation_log$test_rmse_mean),
                  verbose = 0)
#Test
pred = predict(xgboost2,newdata=as.matrix(train_input))
rmse_xgboost = sqrt(mean((pred - train$price)^2)); rmse_xgboost

#train:39.75157
#test:64.05637
#kaggle:67.17714
## MODEL 3: BOOSTING WITH XGBOOST FROM FEATURE SELECTION 1 ENDS HERE
## MODEL 3: BOOSTING WITH XGBOOST FROM FEATURE SELECTION 2 STARTS HERE
trt = designTreatmentsZ(dframe = train,
                        varlist = names(train)[c(2,3,7,8,9,10,11,12,13,14,18,19,20,21,23,25,27,28,29,30,31,32,33,34,36,39,41,48)])
newvars = trt$scoreFrame[trt$scoreFrame$code%in% c('clean','lev'),'varName']

train_input = prepare(treatmentplan = trt, 
                      dframe = train,
                      varRestriction = newvars)
test_input = prepare(treatmentplan = trt, 
                     dframe = test,
                     varRestriction = newvars)

library(xgboost); library(caret)
set.seed(1000)
tune_nrounds = xgb.cv(data=as.matrix(train_input), 
                      label = train$price,
                      nrounds=250,
                      nfold = 5,
                      verbose = 0)
ggplot(data=tune_nrounds$evaluation_log, aes(x=iter, y=test_rmse_mean))+
  geom_point(size=0.4, color='sienna')+
  geom_line(size=0.1, alpha=0.1)+
  theme_bw()
which.min(tune_nrounds$evaluation_log$test_rmse_mean)

xgboost2= xgboost(data=as.matrix(train_input), 
                  label = train$price,
                  nrounds=which.min(tune_nrounds$evaluation_log$test_rmse_mean),
                  verbose = 0)
#Test
pred = predict(xgboost2,newdata=as.matrix(test_input))
rmse_xgboost = sqrt(mean((pred - test$price)^2)); rmse_xgboost


#train:45.74325
#test:65.19082
#kaggle:61.61641(private), 66.25892(public)
## MODEL 3: BOOSTING WITH XGBOOST FROM FEATURE SELECTION 2 ENDS HERE
## MODELING AND FEATURE SELECTION ENDS HERE

#####Output#####
test_input = prepare(treatmentplan = trt, 
                     dframe = scoringcleanData,
                     varRestriction = newvars)
pred = predict(xgboost2, 
               newdata=as.matrix(test_input))
submissionFile = data.frame(id = scoringcleanData$id, price = pred)
write.csv(submissionFile, 'my_17th_sumbission.csv',row.names = F)
sum(is.na(submissionFile$price))

###new submission to check kaggle RMSE
predt = predict(lm_hy,newdata = scoringcleanData)
submissionFile = data.frame(id = scoringData$id, price = predt)
write.csv(submissionFile, 'model1.1 check.csv',row.names = F)

predt = predict(lm_lasso,newdata = scoringcleanData)
submissionFile = data.frame(id = scoringData$id, price = predt)
write.csv(submissionFile, 'model1.2 check.csv',row.names = F)

predt = predict(tree_default1,newdata = scoringcleanData)
submissionFile = data.frame(id = scoringData$id, price = predt)
write.csv(submissionFile, 'model2.1 check.csv',row.names = F)

predt = predict(tree_default2,newdata = scoringcleanData)
submissionFile = data.frame(id = scoringData$id, price = predt)
write.csv(submissionFile, 'model2.2 check.csv',row.names = F)



