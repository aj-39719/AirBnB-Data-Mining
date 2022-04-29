# ********************************** IMPORTING DATA AND LIBRARIES ************************************* 
#install.packages("BBmisc")
#install.packages("data.table")
#install.packages("caret")
#install.packages("ggplot2")
#install.packages("corrplot")
#install.packages("tree")
#install.packages("e1071")
#install.packages("leaps")
#install.packages("ModelMetrics")
# install.packages("polywog")
# install.packages("rpart.plot")
#install.packages("kernlab")
#install.packages("ggthemes")
#install.packages("cowplot")
#install.packages("RColorBrewer")
#install.packages("randomForest")
#install.packages("gbm")
library(ggthemes)
library(kernlab)
library(BBmisc)
library(data.table)
library(corrplot)
library(caret)
library(data.table)
library(ggplot2)
library(ModelMetrics)
library(polywog)
library (tree)
library(rpart.plot)
library(e1071)
library(cowplot)
library(RColorBrewer)
library(leaps)
library(dplyr) 
library(randomForest)
library(gbm)

#path = '/Users/admin/Downloads/listings.csv'
airbnb = fread(input = "listings.csv")
#airbnb = fread(input = path)

# Check the structure of the table
str(airbnb)

# Check NA values per column
colMeans(is.na(airbnb))

# Check NA values as a percentage of total data
hist(colMeans(is.na(airbnb)),
     labels = TRUE,
     col = "darkblue",
     main = "NA Values as a percentage of Data",
     xlab = "Mean NA Values",
     border = "white",
     ylim = c(0,65))


# ********************************** PRELIMINARY DATA CLEANING **************************************


blank_cols = c("neighbourhood_group_cleansed","calendar_updated","license")
# We will keep the blank bathrooms column for now, as we can extract that data from bathrooms_text

# Removing Blank columns, note how the variables in our data frame drop from 74 to 71
airbnb = subset(airbnb, select = !(names(airbnb) %in% blank_cols))

#Lets store number of bathrooms in the blank column, by extracting data from the bathrooms_text column
airbnb$bathrooms = as.numeric(gsub('[a-zA-Z]', '', airbnb$bathrooms_text))

# Saving host_since as a individual variable
host_since =as.character(airbnb [["host_since"]])

# Calculate the joining time for each host
join_time = c()
for(i in 1: length(host_since )){
  join_time[i] = difftime("2021-12-09", strptime(host_since[i], format = "%Y-%m-%d"), ,unit = "days")
}

# Introduce join_time as a new column into data set
airbnb = cbind(airbnb,join_time)

# Lets remove columns not required for our analysis 
cols_to_be_removed = c("id","listing_url","scrape_id","last_scraped","name","description","neighborhood_overview",
                       "picture_url","host_id","host_url","host_name","host_since","host_about","host_thumbnail_url",
                       "host_picture_url","calendar_last_scraped", "first_review", "last_review", "bathrooms_text", 
                       "host_listings_count","neighbourhood","minimum_minimum_nights",
                       "maximum_minimum_nights","minimum_maximum_nights","maximum_maximum_nights",
                       "minimum_nights_avg_ntm","maximum_nights_avg_ntm")

airbnb = subset(airbnb, select = !(names(airbnb) %in% cols_to_be_removed))

# We see that some columns still have N/A values which are not being treated as such
# because R is reading them as strings, the following code helps us with that - 
airbnb[airbnb=="N/A"] = NA

# We also have some blank values in some columns that should be converted to NA as well
airbnb[airbnb == "" | airbnb == " "] = NA 

# Check NA values as a percentage of total data again 
hist(colMeans(is.na(airbnb)),
     labels = TRUE,
     col = "darkblue",
     main = "NA Values as a percentage of Data",
     xlab = "Mean NA Values",
     border = "white",
     ylim = c(0,65))



# **************************** EXPLORATORY DATA ANALYSIS OF RAW DATA ******************************


#  ****** Most frequent neighborhoods ******

freq_area <- data.frame(cbind(Frequency = table(airbnb$neighbourhood_cleansed), 
                              Percent = prop.table(table(airbnb$neighbourhood_cleansed)) * 100))
freq_area <- freq_area[order(freq_area$Frequency),]

df <- data.frame(neighbourhood = row.names(tail(freq_area, 10)),
                 Frequency = tail(freq_area, 10)$Frequency)

tema <- theme(plot.title = element_text(size = 15, hjust = .5, face = "bold"),
              axis.text.x = element_text(size = 9, angle = 20),
              axis.text.y = element_text(size = 9),
              axis.title.x = element_text(size = 10, face = "bold"),
              axis.title.y = element_text(size = 10, face = "bold"),
              legend.text = element_text(size = 9, face = "bold"))

options(repr.plot.width=40, repr.plot.height=12)
ggplot(data = df, mapping = aes(x = neighbourhood, y = Frequency)) +
  theme_excel_new() +
  geom_point(size = 6, color = "blue") +
  ggtitle("The 10 most Frequent Neighbourhood") +
  xlab("Neighbourhood") +
  geom_line(color = "black", size = 1, linetype= 16, group = 1, alpha = .8) + 
  geom_bar( stat = "identity",mapping = aes(fill = neighbourhood, color = neighbourhood), alpha = .5, size = 0.8) +   
  tema


#  ****** Distribution of minimum nights required ******

df2 = airbnb[,minimum_nights]
df2 = data.frame(df2[df2 <= 40])
colnames(df2) = c("minimum_nights")

a = ggplot(data = airbnb, mapping = aes(x = minimum_nights)) +
  geom_histogram(fill = "darkblue", bins =21 , size = 1, color = "white") +
  theme_clean() +
  ylab("Frequency") +
  xlab("Minimum nights") +
  ggtitle("Minimum nights") +
  tema


b <- ggplot(data = df2, mapping = aes(x = minimum_nights)) +
  geom_histogram(fill = "darkblue", bins = 11, size = 1, color = "white") +
  theme_clean() +
  ylab("Frequency") +
  xlab("Minimum nights") +
  ggtitle("Minimum nights less than 40") +
  tema

plot_grid(a, b, ncol=2, nrow=1)

# ****** Distribution of Accommodation Type ******

tema1 <- theme(plot.title = element_text(size = 14, hjust = .5),
               axis.title.x = element_text(size = 9, face = "bold"),
               axis.title.y = element_text(size = 9, face = "bold"),
               axis.text.x = element_text(size = 9),
               axis.text.y = element_text(size = 9),
               legend.position = "none")
freq_type <- data.frame(cbind(Frequency = table(airbnb$room_type),
                              Percent = prop.table(table(airbnb$room_type)) * 100))
freq_type <- freq_type[order(freq_type$Frequency),]

options(repr.plot.width=18, repr.plot.height=6)
c <- ggplot(data = freq_type, mapping = aes(x = Frequency, y = row.names.data.frame(freq_type))) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(freq_type),
                                            color = row.names(freq_type)), alpha = .7, size = 0.8) +
  geom_label(mapping = aes(label=Frequency), fill = "black", size = 3,
             fontface = "bold", color = "white", hjust=.5) +
  ylab("Room Type") +
  ggtitle("Room type distribution") +
  theme_clean() +
  tema1

c


#  ****** Average Price per Room Type ******

#Removing currency symbol
airbnb$price = as.numeric(gsub("\\$", "", airbnb$price)) 
airbnb$price = as.numeric(gsub(",", "", airbnb$price)) 
#imputing mean values by group
airbnb$price[is.na(airbnb$price)] = ave(airbnb$price, airbnb$room_type, 
                                        FUN = function(x) 
                                          mean(x, na.rm = TRUE))[c(which(is.na(airbnb$price)))]

#calculating average room price
mean_room_type <- aggregate(list(average_price = airbnb$price), 
                            list(room_type = airbnb$room_type), mean)
mean_room_type$Percent <- prop.table(mean_room_type$average_price) * 100

mean_room_type$average_price = round(mean_room_type$average_price, digits = 2)

d <- ggplot(data = mean_room_type, aes(x=room_type, y=average_price)) +
  geom_segment(aes(xend=room_type, yend=0, color = room_type), size = 15) +
  geom_label(mapping = aes(label=average_price), fill = "black", size = 3,
             fontface = "bold", color = "white", hjust=0) +
  theme_minimal() +
  ylab("Price in GBP") +
  xlab("Room Type") +
  ggtitle("Average Price per Room Type") +
  tema1

d

#  ****** Most Cheapest / Expensive Neighborhoods ******

top_10_neighbourhood <- aggregate(list(airbnb$price), list(airbnb$neighbourhood_cleansed), mean)
colnames(top_10_neighbourhood) <- c("neighbourhood", "Average_price_per_neighborhood")
top_10_neighbourhood <- top_10_neighbourhood[order(top_10_neighbourhood$Average_price_per_neighborhood),]

top_expensive = tail(top_10_neighbourhood,10)
top_cheapest = head(top_10_neighbourhood,10)

colourCount = 10
getPalette = colorRampPalette(brewer.pal(9, "Blues"))
options(repr.plot.width=25, repr.plot.height=6)

tema2 <- theme(
  plot.title = element_text(size = 15, hjust = .5),
  axis.text.x = element_text(size = 10, face = "bold"),
  legend.position="none")


e <- ggplot(data = top_cheapest, 
            mapping = aes(x = neighbourhood, 
                          y = Average_price_per_neighborhood)) +
  geom_bar(stat = "identity",
           mapping = aes(fill = neighbourhood),
           alpha = .8, size = 1.5) +
  geom_label(mapping = aes(label = round(Average_price_per_neighborhood, 1)),
             size = 2.5, fill = "#F5FFFA", fontface = "bold") +
  theme_minimal() + 
  ggtitle("Top 10 cheapest neighborhoods") +
  xlab("") +
  ylab("") +
  tema2

e + coord_polar(clip = "off",direction = 1)

# ********************************* DATA CLEANING FOR PREDICTION **********************************


# removes listings with no stays or corrupted/incomplete reviews
airbnb = airbnb[!is.na(airbnb$review_scores_location) & 
                  !is.na(airbnb$review_scores_checkin) & 
                  !is.na(airbnb$review_scores_cleanliness) & 
                  !is.na(airbnb$review_scores_value),]

# count number of verification ways
verification = airbnb [,c("host_verifications")]
counts_verification = c()
for (i in 1: dim(verification)[1]){
  a = toString(verification[i])
  counts_verification[i] = length(explode( a,',')) #count number of verifications required for property
}
airbnb = cbind(airbnb,counts_verification)


# count number of amenities
amenities = airbnb [,c("amenities")]
counts_amenities = c()
for (i in 1: dim(amenities)[1]){
  a = toString(amenities[i])
  counts_amenities[i] = length(explode( a,','))
}
airbnb = cbind(airbnb,counts_amenities)

# Clean verifications & amenities and count unique for each
unique_verifications = gsub("[[:punct:]]","",unlist(strsplit(airbnb$host_verifications,split=",")))
unique_verifications = unique(unique_verifications)

clean_host_verifications = strsplit(gsub("[[:punct:]]","",airbnb$host_verifications), split = " ")
airbnb = airbnb[,-c("host_verifications")]

unique_amenities = gsub("[[:punct:]]","",unlist(strsplit(airbnb$amenities,split=",")))
temp = as.factor(unique_amenities) # store factor information before we change unique_amenities
unique_amenities = unique(unique_amenities)

clean_amenities = strsplit(gsub("[[:punct:]]","",airbnb$amenities), split = " ")
airbnb = airbnb[,-c("amenities")]

summary = summary(temp)
for (i in 1:90) { # remove white space that R introduced
  if(substring(names(summary)[i],1,1)==" ") {
    names(summary)[i] = substring(names(summary)[i],2)
  }
}
keepT90 = names(summary)[1:90] # top 90 most frequent ammenities
keepT90 = keepT90[-c(29,32,38,74)] # remove duplicate shampoo washer Hot water heating

# keep only top 90 amenities and discard the rest
for (i in 1:length(clean_amenities)) {
  temp_index = clean_amenities[[i]][1:length(clean_amenities[[i]])] %in% keepT90
  clean_amenities[[i]] = clean_amenities[[i]][temp_index]
}

# convert to dummy
# create temporary table first of all 0s for every dummy
temp_table = airbnb[,1:2]
temp_table[ , keepT90] <- c(0)
temp_table = temp_table[,3:length(temp_table[1])]

# for every amenity go to the appropriate dummy column and turn that 
# flat's value to 1
for (i in 1:length(clean_amenities)) {
  for (j in clean_amenities[[i]]){
    #if (i %% 1000 == 0) {print(i)}
    temp_table[[j]][i] = 1 
  }
}
# attach the dummy columns to main data table
airbnb = cbind(airbnb,temp_table)

# Encode t/f to binary variable 1/0;
airbnb$host_is_superhost[airbnb$host_is_superhost == "t"] = 1
airbnb$host_is_superhost[airbnb$host_is_superhost == "f"] = 0
airbnb$host_has_profile_pic[airbnb$host_has_profile_pic == "t"] = 1
airbnb$host_has_profile_pic[airbnb$host_has_profile_pic == "f"] = 0
airbnb$host_identity_verified[airbnb$host_identity_verified == "t"] = 1
airbnb$host_identity_verified[airbnb$host_identity_verified == "f"] = 0
airbnb$has_availability[airbnb$has_availability == "t"] = 1
airbnb$has_availability[airbnb$has_availability == "f"] = 0
airbnb$instant_bookable[airbnb$instant_bookable == "t"] = 1
airbnb$instant_bookable[airbnb$instant_bookable == "f"] = 0

# Encoding Categorical Variables
airbnb$host_response_time[is.na(airbnb$host_response_time)] = 0
airbnb$host_response_time[airbnb$host_response_time == "within an hour"] = 1
airbnb$host_response_time[airbnb$host_response_time == "within a few hours"] = 2
airbnb$host_response_time[airbnb$host_response_time == "within a day"] = 3
airbnb$host_response_time[airbnb$host_response_time == "a few days or more"] = 4

airbnb$host_response_rate = (as.numeric(gsub("%$","",airbnb$host_response_rate)))/100
airbnb$host_acceptance_rate = (as.numeric(gsub("%$","",airbnb$host_acceptance_rate)))/100



# Remove irrelevant columns with too many missing values that cant be imputed
airbnb = airbnb[!is.na(airbnb$host_total_listings_count) &
                  !is.na(airbnb$host_identity_verified)&
                  !is.na(airbnb$host_location)]

# Removing commas from price
airbnb$price = gsub(",","",airbnb$price)

# Converting price column to numeric type
airbnb$price = as.numeric(gsub("\\$", "", airbnb$price)) 

# ********************************* TRAINING AND TESTING SPLIT **********************************

set.seed(1)
test_set_indices = sample(1:nrow(airbnb),round(0.3*nrow(airbnb)),replace = FALSE)
training_set = airbnb[-test_set_indices,]
test_set = airbnb[test_set_indices,]

# Removing spaces from column names
names(training_set) <- gsub(" ", "_", names(training_set))
names(test_set) <- gsub(" ", "_", names(test_set))

# IMPUTING VALUES ON TRAINING SET :-

#imputing values for bathroom by Room_type (168 values)
airbnb[,bathrooms := replace(bathrooms, is.na(bathrooms), 
                             median(training_set$bathrooms, na.rm=TRUE)),by=.(room_type)]
test_set[,bathrooms := replace(bathrooms, is.na(bathrooms), 
                               median(training_set$bathrooms, na.rm=TRUE)),by=.(room_type)]
training_set[,bathrooms := replace(bathrooms, is.na(bathrooms), 
                                   median(training_set$bathrooms, na.rm=TRUE)),by=.(room_type)]

#imputing values for beds by Room_type (2188 values)
airbnb[,beds := replace(beds, is.na(beds), 
                        median(training_set$beds, na.rm=TRUE)),by=.(room_type)]
test_set[,beds := replace(beds, is.na(beds), 
                          median(training_set$beds, na.rm=TRUE)),by=.(room_type)]
training_set[,beds := replace(beds, is.na(beds), 
                              median(training_set$beds, na.rm=TRUE)),by=.(room_type)]

#imputing values for bedrooms by Room_type (2520 values)
airbnb[,bedrooms := replace(bedrooms, is.na(bedrooms),median(training_set$bedrooms, na.rm=TRUE)),by=.(room_type)]
test_set[,bedrooms := replace(bedrooms, is.na(bedrooms),median(training_set$bedrooms, na.rm=TRUE)),by=.(room_type)]
training_set[,bedrooms := replace(bedrooms, is.na(bedrooms),median(training_set$bedrooms, na.rm=TRUE)),by=.(room_type)]


# turns all NA join times into the mean join time on training set
airbnb$join_time[which(is.na(airbnb$join_time))] = mean(training_set$join_time[which(!is.na(training_set$join_time))])
test_set$join_time[which(is.na(airbnb$join_time))] = mean(training_set$join_time[which(!is.na(training_set$join_time))])
training_set$join_time[which(is.na(airbnb$join_time))] = mean(training_set$join_time[which(!is.na(training_set$join_time))])

# turns all NA host_response_rate into the median host_response_rate on training set
airbnb[,host_response_rate := replace(host_response_rate, is.na(host_response_rate),median(training_set$host_response_rate, na.rm=TRUE))]
test_set[,host_response_rate := replace(host_response_rate, is.na(host_response_rate),median(training_set$host_response_rate, na.rm=TRUE))]
training_set[,host_response_rate := replace(host_response_rate, is.na(host_response_rate),median(training_set$host_response_rate, na.rm=TRUE))]


# turns all NA host_acceptance_rate into the median host_acceptance_rate on training set
airbnb[,host_acceptance_rate := replace(host_acceptance_rate, is.na(host_acceptance_rate),median(training_set$host_acceptance_rate, na.rm=TRUE))]
test_set[,host_acceptance_rate := replace(host_acceptance_rate, is.na(host_acceptance_rate),median(training_set$host_acceptance_rate, na.rm=TRUE))]
training_set[,host_acceptance_rate := replace(host_acceptance_rate, is.na(host_acceptance_rate),median(training_set$host_acceptance_rate, na.rm=TRUE))]



# Convert all Character type variables to Factors
training_set = training_set %>% mutate_if(sapply(training_set, is.character), as.factor)
test_set = test_set %>% mutate_if(sapply(test_set, is.character), as.factor)


# ********************************** PREDICTING LOCATION REVIEWS *************************************

set.seed(1)
training_set_location = training_set[,c("latitude","longitude","review_scores_location")]
test_set_location = test_set[,c("latitude","longitude","review_scores_location")]
colMeans(is.na(training_set_location)) # no missing value
summary(training_set_location)


# EDA
hist(training_set_location$latitude, main = "Histogram of latitude", xlab = "latitude") 
# looks like normal distribution

hist(training_set_location$longitude,main = "Histogram of longitude", xlab = "longitude")
# looks like normal distribution

hist(training_set_location$review_scores_location,main = "Histogram of location score", xlab = "location score")
# skewed to the right, needs some transformation

log_location = log(training_set_location$review_scores_location)# taking logarithm
hist(log_location,main = "Histogram of log(location score)", xlab = "log(location score)")
# still not normal

pairs(training_set_location) 
corrplot(cor(training_set_location), method = "circle", diag = FALSE,type = "upper")

# 1) latitude is positively correlated with longitude
# 2) latitude does not correlated with location score
# 3) longitude is slightly negatively correlated with location score


# LINEAR REGRESSION MODEL

lm_reg = train(review_scores_location ~ latitude + longitude,
               data = training_set_location, method = 'lm',
               trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
               preProcess = c('center','scale'))
lm_reg$finalModel
# Result is consistent with correlation table, showing that latitude does not play a significant role 
# in the model and longitude is negatively related to the location score.
# linear model is location score = -0.027805 *scale(longitude)+0.002703*scale(latitude)+4.749113
# it is not a good model since r square is almost zero, which means it doesn't explain any variation
rmse(test_set_location$review_scores_location,
     predict(lm_reg,test_set_location[,c("latitude","longitude"),drop=FALSE])) 
# Testing RMSE = 0.4326827  
par(mfrow=c(2,2))
plot(lm_reg$finalModel) 
# implies residual is not normally distributed,thus confirmed that this model doesn't explain.

# Model selection
regfit_full = regsubsets(review_scores_location~. , data=training_set_location)
summary_full = summary(regfit_full)
plot(summary_full$rss,type="b")

# LINEAR REGRESSION WITH LONGITUDE ONLY

lm_reg_onlyl = train(review_scores_location ~ longitude,
                     data = training_set_location, method = 'lm',
                     trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
                     preProcess = c('center','scale'))

rmse(test_set_location$review_scores_location,
     predict(lm_reg_onlyl,test_set_location[,c("latitude","longitude"),drop=FALSE]))
#Test RMSE = 0.4326636
par(mfrow=c(2,2))
plot(lm_reg_onlyl$finalModel) # still pretty bad


# LINEAR REGRESSION ON LOG OF REVIEW SCORES

lm2_reg = train(log(review_scores_location+1) ~ latitude + longitude, 
                # Since log0 is undefined, we will add 1 to each score to avoid it.
                data = training_set_location, method = 'lm',
                trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
                preProcess = c('center','scale'))
lm2_reg
summary(lm2_reg$finalModel)
rmse(log(test_set_location$review_scores_location+1),predict(lm2_reg,test_set_location[,c("latitude","longitude"),drop=FALSE]))
plot(lm2_reg$finalModel)
# Training RMSE = 0.09642302 , Test RMSE = 0.1012406
# plot still bad - heavy tails

# POLYNOMIAL REGRESSION

poly_reg = cv.polywog(review_scores_location ~ scale(latitude) + scale(longitude),
                      data = training_set_location, degrees.cv = 1:20,
                      nfolds = 10,thresh = 1e-4)
poly_reg$degree.min # the best degree is 7
poly_reg$polywog.fit
poly_reg_model<- bootPolywog(poly_reg$polywog.fit, nboot = 3)
rmse(test_set_location$review_scores_location,
     predict(poly_reg_model,test_set_location[,c("latitude","longitude"),drop=FALSE])) 

rmse(training_set_location$review_scores_location,
     predict(poly_reg_model,training_set_location[,c("latitude","longitude"),drop=FALSE]))
plot(poly_reg_model)
# Training RMSE = 0.426977, Test RMSE = 0.4149603 


# DECISION TREE REGRESSION

tree_location <- tree (review_scores_location~. , data=training_set_location)
summary(tree_location)
plot (tree_location)
text (tree_location , pretty = 0)

# Cross-validation to choose the best tree
cp.grid = expand.grid(.cp = (0:10)*0.001)
tree_reg = train(review_scores_location ~ latitude + longitude,
                 data = training_set_location, method = 'rpart',
                 trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
                 tuneGrid = cp.grid )
#tree_reg 
best.tree = tree_reg$finalModel

prp(best.tree) # the best cp is 0.001                
mean ((predict (best.tree , newdata =training_set_location) - training_set_location$review_scores_location)^2) 
mean ((predict (best.tree , newdata =test_set_location) - test_set_location$review_scores_location)^2)  
#  training data: MSE = 0.1700948; RMSE = 0.4124255 ;testing data: MSE = 0.1804107; RMSE = 0.4247478


# SVM REGRESSION : RADIAL KERNEL

#svm_reg_linear = train(review_scores_location ~ latitude + longitude,
#                data = training_set, method = 'svmRadial',
#                trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
#                preProcess = c('center','scale'),
#                 tuneGrid = expand.grid(C = seq(0, 1, length = 10),sigma = c(0.1,0.2)))
# Since data set is very large over 40000 instance, when I run the code for tuning parameter it doesn't work.

# fit in radial kernel 
plot(training_set_location[,1:2],col=training_set_location$review_scores_location+3,asp=0) 

# EPS REGRESSION: no control of number of support vectors

svmfit1 = svm(review_scores_location~. ,data=training_set_location, kernel="radial" ,scale=TRUE) # needs around 5 minutes to run
summary(svmfit1) 
rmse(training_set_location$review_scores_location,predict(svmfit1,training_set_location[,1:2])) 
rmse(test_set_location$review_scores_location,predict(svmfit1,test_set_location[,1:2]))
# Testing set RMSE = 0.4260663 ; Training set RMSE = 0.4383655

# SVM REGRESSION : LINEAR KERNEL

svmfit2 = svm(review_scores_location~. ,data=training_set_location, kernel="linear" ,scale=TRUE)
rmse(training_set_location$review_scores_location,predict(svmfit2,training_set_location[,1:2]))
rmse(test_set_location$review_scores_location,predict(svmfit2,test_set_location[,1:2]))
# testing set RMSE =0.4393 ; training set RMSE = 0.4517757 

# SVM REGRESSION : POLYNOMIAL KERNEL

svmfit3 = svm(review_scores_location~. ,data=training_set_location, kernel="polynomial" ,scale=TRUE)
rmse(training_set_location$review_scores_location,predict(svmfit3,training_set_location[,1:2]))
rmse(test_set_location$review_scores_location,predict(svmfit3,test_set_location[,1:2]))
# testing set RMSE =  0.4401525 # training set RMSE =0.4525119

## KNN REGRESSION

tuneGrid = expand.grid(k = seq(1,59, by = 2))

#Warning - the next line of code takes about 8 minutes to execute
knn_reg_loc = train(review_scores_location ~ latitude + longitude,
                data = training_set_location, method = 'knn',
                preProcess = c('center','scale'),
                trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
                tuneGrid = tuneGrid)

knn_results_loc = knn_reg_loc$results #lowest testing RMSE for k = 59
knn_results_loc
plot(knn_reg_loc)
varImp(knn_reg_loc) #it is taking longitude to be the most important variable of importance, as seen above

pred_knn_loc = predict(knn_reg_loc, newdata = test_set_location)
RMSE(pred_knn_loc,test_set_location$review_scores_location)
# Test RMSE =0.4201638  

                                        
## RANDOM FOREST REGRESSION

set.seed(1)

# create a copy of the dataset for Random forest without columns with too many factors, as this prevent RF from running                                       
training_set_rf = subset(training_set, select = -c(host_location,host_neighbourhood, property_type))
test_set_rf = subset(test_set, select = -c(host_location,host_neighbourhood, property_type))
                                        
# I don't include the scores for other categories in the training
rf_exclude_from_train = c("review_scores_rating","review_scores_accuracy","review_scores_cleanliness","review_scores_checkin","review_scores_communication","review_scores_value")
# train the random forest                                        
rf_loc = randomForest(review_scores_location ~. , data = training_set_rf |> select(-rf_exclude_from_train), sampsize=5000)

rf_loc #not too much variation is explained by the model
plot(rf_loc$mse,xlab = "tree count", ylab = "mse")# mse is minimized way before the 5000th tree

#names(which(colSums(is.na(test_set_rf)) > 0))

test_predictions_rf_loc = predict(rf_loc,test_set_rf[,-c("review_scores_location")])
test_predictions_rf_loc = unname(test_predictions_rf_loc)

# train rmse = 0.3551543
training_predictions_rf_loc = predict(rf_loc,training_set_rf[,-c("review_scores_location")])
training_predictions_rf_loc = unname(training_predictions_rf_loc)
rmse(training_set_rf$review_scores_location,training_predictions_rf_loc)

# test rmse = 0.4121174
rmse(test_set_rf$review_scores_location,test_predictions_rf_loc)
#importance(rf_comm)

## BAGGING MODEL
# same as random forest but uses all of the columns per iteration                                        
rf_bagging_loc = randomForest(review_scores_location ~. , data = training_set_rf |> select(-rf_exclude_from_train), sampsize=5000, mtry=(ncol(training_set_rf)-1)) 
rf_bagging_loc
test_predictions_bag_loc = predict(rf_bagging_loc,test_set_rf[,-c("review_scores_location")])
test_predictions_bag_loc = unname(test_predictions_bag_loc)

training_predictions_bag_loc = predict(rf_bagging_loc,training_set_rf[,-c("review_scores_location")])
training_predictions_bag_loc = unname(training_predictions_bag_loc)
# training rmse = 0.3531969
rmse(training_set_rf$review_scores_location,training_predictions_bag_loc)

# test rmse = 0.4136218
rmse(test_set_rf$review_scores_location,test_predictions_bag_loc)

## Boosting Model 
set.seed(1)

# 80% of the data is used for training and 20% is set aside for validation which allowed me
# to check the best number of trees and tree depth for the boosting model                                        
boost_loc = gbm(review_scores_location ~. , data = training_set_rf |> select(-rf_exclude_from_train),train.fraction=0.8, distribution="gaussian", n.trees=180, interaction.depth=3)
yhat_boost_loc = predict(boost_loc, test_set_rf[,-c("review_scores_location")], n.trees=180)
# test rmse = 0.415215
rmse(test_set_rf$review_scores_location,yhat_boost_loc)
plot(boost_loc$train.error,xlab = "tree count", ylab = "loss")
# I plot the valid error to see when overfitting starts
plot(boost_loc$valid.error,xlab = "tree count", ylab = "validation loss")
                                        
training_yhat_boost_loc = predict(boost_loc, training_set_rf[,-c("review_scores_location")], n.trees=180)
# training rmse = 0.3892829
rmse(training_set_rf$review_scores_location,training_yhat_boost_loc)

# ********************************** PREDICTING COMMUNICATION REVIEWS ***********************************

airbnb_communication = airbnb |> select(-host_acceptance_rate,-host_neighbourhood,
                                        -host_location, -property_type, -neighbourhood_cleansed,
                                        -review_scores_rating, - review_scores_accuracy,
                                        -review_scores_cleanliness, -review_scores_checkin,
                                        -review_scores_location, -review_scores_value)
training_set_communication = training_set |> select(-host_acceptance_rate,-host_neighbourhood,
                                                    -host_location, -property_type, -neighbourhood_cleansed,
                                                    -review_scores_rating, - review_scores_accuracy,
                                                    -review_scores_cleanliness, -review_scores_checkin,
                                                    -review_scores_location, -review_scores_value)
test_set_communication = test_set |> select(-host_acceptance_rate,-host_neighbourhood,
                                            -host_location, -property_type, -neighbourhood_cleansed,
                                            -review_scores_rating, - review_scores_accuracy,
                                            -review_scores_cleanliness, -review_scores_checkin,
                                            -review_scores_location, -review_scores_value)


# ***** Decision Tree Model ******

tree_communication <- tree(review_scores_communication ~ ., data=training_set_communication)
summary(tree_communication)
plot (tree_communication)
text (tree_communication , pretty = 0)

pred_comm_tree = predict(tree_communication, newdata = test_set_communication)
RMSE(pred_comm_tree,test_set_communication$review_scores_communication)
# Test RMSE = 0.4417633

# Cross-validation to choose the best tree
training_set_communication = training_set_communication %>% mutate_if(sapply(training_set_communication, is.factor), as.numeric)
test_set_communication = test_set_communication %>% mutate_if(sapply(test_set_communication, is.factor), as.numeric)

cp.grid = expand.grid(.cp = (0:10)*0.001)
tree_reg = train(review_scores_communication ~ . ,
                 data = training_set_communication, method = 'rpart',
                 trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
                 tuneGrid = cp.grid )
tree_reg 
best.tree.communication = tree_reg$finalModel
prp(best.tree.communication) # the best cp is 0.004               
rmse (training_set_communication$review_scores_communication,predict (best.tree , newdata =training_set_communication))
rmse (test_set_communication$review_scores_communication,predict (best.tree , newdata =test_set_communication)) 
#  training data:RMSE = 0.4450848 ;testing data:  RMSE = 0.4383787

# ****** KNN Model *******

tuneGrid = expand.grid(k = seq(1,59, by = 2))
knn_reg_comm = train(review_scores_communication ~ calculated_host_listings_count + number_of_reviews +
                     availability_90 + host_is_superhost + join_time,
                data = training_set_communication, method = 'knn',
                preProcess = c('center','scale'),
                trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
                tuneGrid = tuneGrid)

knn_results_comm = knn_reg_comm$results
knn_results_comm 
plot(knn_reg_comm)
varImp(knn_reg_comm) 
# Varibles of importance -
# join_time                       100.00
# host_is_superhost                76.88
# availability_90                  31.20
# calculated_host_listings_count   13.39
# number_of_reviews                 0.00


pred_knn_comm = predict(knn_reg_comm, newdata = test_set_communication)
RMSE(pred_knn_comm,test_set_communication$review_scores_communication)
# Test RMSE = 0.4395576 , Training RMSE = 0.4515547 (lowest for k = 59)


# ****** Linear regression Model *******
hist(training_set_communication$review_scores_communication)
lm_full = lm(review_scores_communication~.,data =training_set_communication1)
summary(lm_full)
# can explain 10% variance
par(mfrow=c(2,2))
plot(lm_full)# heavy tails residuals are not normally distributed
# training rmse = 0.451088; testing rmse = 0.4371724
rmse(training_set_communication1$review_scores_communication,predict(lm_full,training_set_communication1))
rmse(test_set_communication1$review_scores_communication,predict(lm_full,test_set_communication1))


# ********************************** PREDICTING CHECKIN REVIEWS ***********************************
##clean data for check in score
# we select acceptance rate, response time, super host, price, instant bookable as attributes
set.seed(1)
check_in = airbnb[, c("host_acceptance_rate","host_response_time","host_is_superhost","price","instant_bookable","review_scores_checkin")]
training_set_checkin = training_set[,c("host_acceptance_rate","host_response_time","host_is_superhost","price","instant_bookable","review_scores_checkin")]
test_set_checkin = test_set[,c("host_acceptance_rate","host_response_time","host_is_superhost","price","instant_bookable","review_scores_checkin")]
summary(check_in)

# EDA
continuous_var = training_set[,c("host_acceptance_rate","price","review_scores_checkin")]
corrplot(cor(continuous_var), method = "circle", diag = FALSE)
# shows price and acceptance rate both are slightly negatively correlated with check in score;
# almost no relationship between attributes
training_set %>% ggplot(aes(y=review_scores_checkin, x=factor(host_response_time)))+ geom_boxplot() 
training_set %>% ggplot(aes(y=review_scores_checkin, x=factor(host_is_superhost)))+ geom_boxplot() 
training_set %>% ggplot(aes(y=review_scores_checkin, x=factor(instant_bookable)))+ geom_boxplot() 

##linear regression
# convert them factor into numeric 
training_set_checkin = training_set_checkin %>% mutate_if(sapply(training_set_checkin, is.factor), as.numeric)
test_set_checkin = test_set_checkin %>% mutate_if(sapply(test_set_checkin, is.factor), as.numeric)
lm_reg = train( review_scores_checkin~.,
                data =training_set_checkin ,
                method = 'lm',
                trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
                preProcess = c('center','scale'))
lm_reg$finalModel
summary(lm_reg$finalModel)
# all variables are significant,
par(mfrow=c(2,2))
plot(lm_reg$finalModel) # residual plots are bad.
# RMSE for training set:  0.4706284; RMSE for test set is 0.4553869
rmse(training_set_checkin1$review_scores_checkin,predict(lm_reg,training_set_checkin1[,-c("review_scores_checkin"),drop=FALSE]))
rmse(test_set_checkin1$review_scores_checkin,predict(lm_reg,test_set_checkin1[,-c("review_scores_checkin"),drop=FALSE]))

## Decision tree regression
tree_checkin<- tree (review_scores_checkin~. , data=training_set_checkin)
summary(tree_checkin)
plot (tree_checkin)
text (tree_checkin , pretty = 0)
# RMSE in training set = 0.4729908; RMSE in testing set = 0.4584032
mean ((predict (tree_checkin , newdata =training_set_checkin) - training_set_checkin$review_scores_checkin)^2) 
mean ((predict (tree_checkin , newdata =test_set_checkin) - test_set_checkin$review_scores_checkin)^2)
# This model only calculates on superhost: if it is superhost, then it would be higher score

# use cv for a more detailed subtree
cp.grid = expand.grid(.cp = (0:10)*0.001)
tree_reg_checkin = train(review_scores_checkin~.,
                 data = training_set_checkin, method = 'rpart',
                 trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
                 tuneGrid = cp.grid )
tree_reg_checkin
best.tree.checkin = tree_reg_checkin$finalModel
prp(best.tree.checkin) # the best cp is 0.002  

mean ((predict (best.tree.checkin , newdata =training_set_checkin) - training_set_checkin$review_scores_checkin)^2) 
mean ((predict (best.tree.checkin , newdata =test_set_checkin) - test_set_checkin$review_scores_checkin)^2) 
# Testing set RMSE = 0.2026227, training set RMSE = 0.2190281;

# ****** KNN Model *******
tuneGrid = expand.grid(k = seq(1,59, by = 2))
knn_reg_checkin = train(review_scores_checkin ~ instant_bookable + price +
                        host_is_superhost + host_response_time ,
                     data = training_set_checkin, method = 'knn',
                     preProcess = c('center','scale'),
                     trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
                     tuneGrid = tuneGrid)

knn_results_checkin = knn_reg_checkin$results
knn_results_checkin
plot(knn_reg_checkin)
varImp(knn_reg_checkin) 
# Varibles of importance -
# host_is_superhost  100.000
# instant_bookable    30.351
# price                8.057
# host_response_time   0.000


pred_knn_checkin = predict(knn_reg_checkin, newdata = test_set_checkin)
RMSE(pred_knn_checkin,test_set_checkin$review_scores_checkin)
# Test RMSE = 0.4504275 , Training RMSE = 0.4700467 (lowest for k = 59)


#log transform check in score perform still not good

# SVM Regression: Radial Kernel

# fit in radial kernel 
# EPS-REGRESSION: no control of number of support vectors
svmfit1 = svm(review_scores_checkin~. ,data=training_set_checkin, kernel="radial" ,scale=TRUE) # needs around 5 minutes to run
summary(svmfit1) 

rmse(training_set_checkin$review_scores_checkin,predict(svmfit1,training_set_checkin)) 
rmse(test_set_checkin$review_scores_checkin,predict(svmfit1,test_set_checkin))

# Testing set RMSE = 0.4731905, Training set RMSE = 0.4919486


# SVM Regression: Linear Kernel

svmfit2 = svm(review_scores_checkin~. ,data=training_set_checkin, kernel="linear" ,scale=TRUE)
rmse(training_set_checkin$review_scores_checkin,predict(svmfit2,training_set_checkin))
rmse(test_set_checkin$review_scores_checkin,predict(svmfit2,test_set_checkin))

# testing set RMSE = 0.4849525,  training set RMSE =  0.5011387

# ********************************** PREDICTING RATING REVIEWS ****************************************

training_set_rating = training_set[,-c("host_location","host_neighbourhood",
                                       "neighbourhood_cleansed","property_type",
                                       "room_type","review_scores_accuracy",
                                       "review_scores_communication","review_scores_location",
                                       "review_scores_value","review_scores_checkin",
                                       "review_scores_cleanliness")]

test_set_rating = test_set[,-c("host_location","host_neighbourhood",
                               "neighbourhood_cleansed","property_type",
                               "room_type","review_scores_accuracy",
                               "review_scores_communication","review_scores_location",
                               "review_scores_value","review_scores_checkin",
                               "review_scores_cleanliness")]

hist(training_set_rating$review_scores_rating) # not normal distributed log doesn't work
summary(training_set_rating )
training_set_rating = training_set_rating %>% mutate_if(sapply(training_set_rating, is.factor), as.numeric)
test_set_rating = test_set_rating %>% mutate_if(sapply(test_set_rating, is.factor), as.numeric)

# linear regression
lm_full = lm(review_scores_rating~.,data =training_set_rating)
summary(lm_full)
# can explain 10% variance
par(mfrow=c(2,2))
plot(lm_full)# heavy tails residuals are not normally distributed

rmse(training_set_rating$review_scores_rating,predict(lm_full,training_set_rating))
rmse(test_set_rating$review_scores_rating,predict(lm_full,test_set_rating))

# training rmse = 0.4978857; testing rmse = 0.4829659
# rank-deficient caused by the colinearity in the amenities dummy variables.
# we extract distinct characters and count them. \


#log lm reg[not selected]
lm_log = lm(log(review_scores_rating+1)~.,data =training_set_rating)
summary(lm_log) # reduce the r square
par(mfrow=c(2,2))
plot(lm_log)


#decision tree
par(mfrow=c(1,1))
tree_rating<- tree (review_scores_rating~. , data=training_set_rating)
summary(tree_rating)
plot (tree_rating)
text (tree_rating , pretty = 0)

rmse(training_set_rating$review_scores_rating,predict (tree_rating ,newdata=training_set_rating)) 
rmse (test_set_rating$review_scores_rating,predict (tree_rating , newdata =test_set_rating))

# training set rmse = 0.5079681;testing set rmse=0.4915408;

#pruning - Needed?
# cv_results = cv.tree(tree_rating)
# plot(cv_results$size,cv_results$dev,type="b")
# indicesOrderedByDev = order(cv_results$dev,decreasing = FALSE)
# indexForLowestDev = indicesOrderedByDev[1]
# best_size = cv_results$size[indexForLowestDev] # k=4
# more detailed subtrees

cp.grid = expand.grid(.cp = (0:10)*0.001)
tree_reg_rating = train(review_scores_rating~.,
                 data = training_set_rating, method = 'rpart',
                 trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
                 tuneGrid = cp.grid )
tree_reg_rating
best.tree.rating = tree_reg_rating$finalModel
prp(best.tree.rating) # the best cp is  0.004

rmse (training_set_rating$review_scores_rating,predict (best.tree.rating , newdata =training_set_rating)) 
rmse (test_set_rating$review_scores_rating, predict (best.tree.rating , newdata =test_set_rating))

# RMSE for test set = 0.4823962, RMSE for training set = 0.4982808;


# ****** KNN Model *******
tuneGrid = expand.grid(k = seq(1,59, by = 2))
knn_reg_rating = train(review_scores_rating ~ host_is_superhost + calculated_host_listings_count +
                       number_of_reviews + availability_90 +
                       counts_amenities,
                        data = training_set_rating, method = 'knn',
                        preProcess = c('center','scale'),
                        trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
                        tuneGrid = tuneGrid)

knn_results_rating= knn_reg_rating$results
knn_results_rating
plot(knn_reg_rating)
varImp(knn_reg_rating) 
# Variables of importance -
# host_is_superhost              100.000
# counts_amenities                53.404
# availability_90                  8.962
# calculated_host_listings_count   6.485
# number_of_reviews                0.000


pred_knn_rating = predict(knn_reg_rating, newdata = test_set_rating)
RMSE(pred_knn_rating,test_set_rating$review_scores_rating)
# Test RMSE = 0.4885923 , Training RMSE = 0.5050278 (lowest for k = 59)



# ********************************** PREDICTING ACCURACY REVIEWS *****************************************

training_set_accuracy = training_set[,-c("host_location","host_neighbourhood",
                                         "neighbourhood_cleansed","property_type",
                                         "room_type","review_scores_rating",
                                         "review_scores_communication","review_scores_location",
                                         "review_scores_value","review_scores_checkin",
                                         "review_scores_cleanliness")]

test_set_accuracy = test_set[,-c("host_location","host_neighbourhood",
                                 "neighbourhood_cleansed","property_type",
                                 "room_type","review_scores_rating",
                                 "review_scores_communication","review_scores_location",
                                 "review_scores_value","review_scores_checkin",
                                 "review_scores_cleanliness")]

hist(training_set_accuracy$review_scores_accuracy) # not normal distributed log doesn't work
summary(training_set_accuracy )
training_set_accuracy = training_set_accuracy %>% mutate_if(sapply(training_set_accuracy, is.factor), as.numeric)
test_set_accuracy = test_set_accuracy %>% mutate_if(sapply(test_set_accuracy, is.factor), as.numeric)

# linear regression
lm_full = lm(review_scores_accuracy~.,data =training_set_accuracy)
summary(lm_full)
# can explain 10% variance
par(mfrow=c(2,2))
plot(lm_full)# heavy tails residuals are not normally distributed

rmse(training_set_accuracy$review_scores_accuracy,predict(lm_full,training_set_accuracy))
rmse(test_set_accuracy$review_scores_accuracy,predict(lm_full,test_set_accuracy))

# training rmse = 0.4896081; testing rmse = 0.4779199
# rank-deficient caused by the colinearity in the amenities dummy variables.
# we extract distinct characters and count them. \


#log lm reg[not selected]
lm_log = lm(log(review_scores_accuracy+1)~.,data =training_set_accuracy)
summary(lm_log) # reduce the r square
par(mfrow=c(2,2))
plot(lm_log)


#decision tree
par(mfrow=c(1,1))
tree_rating<- tree (review_scores_accuracy~. , data=training_set_accuracy)
summary(tree_rating)
plot (tree_rating)
text (tree_rating , pretty = 0)

rmse(training_set_accuracy$review_scores_accuracy,predict (tree_rating ,newdata=training_set_accuracy)) 
rmse (test_set_accuracy$review_scores_accuracy,predict (tree_rating , newdata =test_set_accuracy))

# training set RMSE = 0.4975439; testing set RMSE=0.484337

#pruning - Needed?
# cv_results = cv.tree(tree_rating)
# plot(cv_results$size,cv_results$dev,type="b")
# indicesOrderedByDev = order(cv_results$dev,decreasing = FALSE)
# indexForLowestDev = indicesOrderedByDev[1]
# best_size = cv_results$size[indexForLowestDev] # k=4
# more detailed subtrees

cp.grid = expand.grid(.cp = (0:10)*0.001)
tree_reg_accuracy = train(review_scores_accuracy~.,
                          data = training_set_accuracy, method = 'rpart',
                          trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
                          tuneGrid = cp.grid )
tree_reg_accuracy
best.tree.accuracy = tree_reg_accuracy$finalModel
prp(best.tree.accuracy) # the best cp is  0.005

rmse (training_set_accuracy$review_scores_accuracy,predict (best.tree.accuracy , newdata =training_set_accuracy)) 
rmse (test_set_accuracy$review_scores_accuracy, predict (best.tree.accuracy , newdata =test_set_accuracy))

# RMSE for test set = 0.4789592, RMSE for training set = 0.4903011;


# ****** KNN Model *******
tuneGrid = expand.grid(k = seq(1,59, by = 2))
knn_reg_accuracy = train(review_scores_accuracy ~ host_is_superhost + calculated_host_listings_count +
                           number_of_reviews + availability_90 +
                           counts_amenities,
                         data = training_set_accuracy, method = 'knn',
                         preProcess = c('center','scale'),
                         trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
                         tuneGrid = tuneGrid)

knn_results_accuracy= knn_reg_accuracy$results
knn_results_accuracy
plot(knn_reg_accuracy)
varImp(knn_reg_accuracy) 
# Variables of importance -



pred_knn_accuracy = predict(knn_reg_accuracy, newdata = test_set_accuracy)
RMSE(pred_knn_accuracy,test_set_accuracy$review_scores_accuracy)
# Test RMSE = 0.4885923 , Training RMSE = 0.5050278 (lowest for k = 59)


# ********************************** PREDICTING CLEANLINESS REVIEWS **************************************

training_set_cleanliness = training_set[,-c("host_location","host_neighbourhood",
                                            "neighbourhood_cleansed","property_type",
                                            "room_type","review_scores_accuracy",
                                            "review_scores_communication","review_scores_location",
                                            "review_scores_value","review_scores_checkin",
                                            "review_scores_rating")]

test_set_cleanliness = test_set[,-c("host_location","host_neighbourhood",
                                    "neighbourhood_cleansed","property_type",
                                    "room_type","review_scores_accuracy",
                                    "review_scores_communication","review_scores_location",
                                    "review_scores_value","review_scores_checkin",
                                    "review_scores_rating")]

hist(training_set_cleanliness$review_scores_cleanliness) # not normal distributed log doesn't work
summary(training_set_cleanliness )
training_set_cleanliness = training_set_cleanliness %>% mutate_if(sapply(training_set_cleanliness, is.factor), as.numeric)
test_set_cleanliness = test_set_cleanliness %>% mutate_if(sapply(test_set_cleanliness, is.factor), as.numeric)

# linear regression
lm_full = lm(review_scores_cleanliness~.,data =training_set_cleanliness)
summary(lm_full)
# can explain 10% variance
par(mfrow=c(2,2))
plot(lm_full)# heavy tails residuals are not normally distributed

rmse(training_set_cleanliness$review_scores_cleanliness,predict(lm_full,training_set_cleanliness))
rmse(test_set_cleanliness$review_scores_cleanliness,predict(lm_full,test_set_cleanliness))

# training rmse = 0.4978857; testing rmse = 0.4829659
# rank-deficient caused by the colinearity in the amenities dummy variables.
# we extract distinct characters and count them. \


#log lm reg[not selected]
lm_log = lm(log(review_scores_cleanliness+1)~.,data =training_set_cleanliness)
summary(lm_log) # reduce the r square
par(mfrow=c(2,2))
plot(lm_log)


#decision tree
par(mfrow=c(1,1))
tree_cleanliness<- tree (review_scores_cleanliness~. , data=training_set_cleanliness)
summary(tree_cleanliness)
plot (tree_cleanliness)
text (tree_cleanliness , pretty = 0)

rmse(training_set_cleanliness$review_scores_cleanliness,predict (tree_cleanliness ,newdata=training_set_cleanliness)) 
rmse (test_set_cleanliness$review_scores_cleanliness,predict (tree_cleanliness , newdata =test_set_cleanliness))

# training set rmse = 0.5079681;testing set rmse=0.4915408;

#pruning - Needed?
# cv_results = cv.tree(tree_cleanliness)
# plot(cv_results$size,cv_results$dev,type="b")
# indicesOrderedByDev = order(cv_results$dev,decreasing = FALSE)
# indexForLowestDev = indicesOrderedByDev[1]
# best_size = cv_results$size[indexForLowestDev] # k=4
# more detailed subtrees

cp.grid = expand.grid(.cp = (0:10)*0.001)
tree_reg_cleanliness = train(review_scores_cleanliness~.,
                             data = training_set_cleanliness, method = 'rpart',
                             trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
                             tuneGrid = cp.grid )
tree_reg_cleanliness
best.tree.cleanliness = tree_reg_cleanliness$finalModel
prp(best.tree.cleanliness) # the best cp is  0.004

rmse (training_set_cleanliness$review_scores_cleanliness,predict (best.tree.cleanliness , newdata =training_set_cleanliness)) 
rmse (test_set_cleanliness$review_scores_cleanliness, predict (best.tree.cleanliness , newdata =test_set_cleanliness))

# RMSE for test set = 0.4823962, RMSE for training set = 0.4982808;


# ****** KNN Model *******
tuneGrid = expand.grid(k = seq(1,59, by = 2))
knn_reg_cleanliness = train(review_scores_cleanliness ~ host_is_superhost + calculated_host_listings_count +
                              number_of_reviews + availability_90 +
                              counts_amenities,
                            data = training_set_cleanliness, method = 'knn',
                            preProcess = c('center','scale'),
                            trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
                            tuneGrid = tuneGrid)

knn_results_cleanliness= knn_reg_cleanliness$results
knn_results_cleanliness
plot(knn_reg_cleanliness)
varImp(knn_reg_cleanliness) 
# Variables of importance -
# host_is_superhost              100.000
# counts_amenities                53.404
# availability_90                  8.962
# calculated_host_listings_count   6.485
# number_of_reviews                0.000


pred_knn_cleanliness = predict(knn_reg_cleanliness, newdata = test_set_cleanliness)
RMSE(pred_knn_cleanliness,test_set_cleanliness$review_scores_cleanliness)
# Test RMSE = 0.4885923 , Training RMSE = 0.5050278 (lowest for k = 59)



# ********************************** PREDICTING VALUE REVIEWS ********************************************

training_set_value = training_set[,-c("host_location","host_neighbourhood",
                                      "neighbourhood_cleansed","property_type",
                                      "room_type","review_scores_accuracy",
                                      "review_scores_communication","review_scores_location",
                                      "review_scores_value","review_scores_checkin",
                                      "review_scores_rating")]

test_set_value = test_set[,-c("host_location","host_neighbourhood",
                              "neighbourhood_cleansed","property_type",
                              "room_type","review_scores_accuracy",
                              "review_scores_communication","review_scores_location",
                              "review_scores_value","review_scores_checkin",
                              "review_scores_rating")]

hist(training_set_value$review_scores_value) # not normal distributed log doesn't work
summary(training_set_value )
training_set_value = training_set_value %>% mutate_if(sapply(training_set_value, is.factor), as.numeric)
test_set_value = test_set_value %>% mutate_if(sapply(test_set_value, is.factor), as.numeric)

# linear regression
lm_full = lm(review_scores_value~.,data =training_set_value)
summary(lm_full)
# can explain 10% variance
par(mfrow=c(2,2))
plot(lm_full)# heavy tails residuals are not normally distributed

rmse(training_set_value$review_scores_value,predict(lm_full,training_set_value))
rmse(test_set_value$review_scores_value,predict(lm_full,test_set_value))

# training rmse = 0.4978857; testing rmse = 0.4829659
# rank-deficient caused by the colinearity in the amenities dummy variables.
# we extract distinct characters and count them. \


#log lm reg[not selected]
lm_log = lm(log(review_scores_value+1)~.,data =training_set_value)
summary(lm_log) # reduce the r square
par(mfrow=c(2,2))
plot(lm_log)


#decision tree
par(mfrow=c(1,1))
tree_value<- tree (review_scores_value~. , data=training_set_value)
summary(tree_value)
plot (tree_value)
text (tree_value , pretty = 0)

rmse(training_set_value$review_scores_value,predict (tree_value ,newdata=training_set_value)) 
rmse (test_set_value$review_scores_value,predict (tree_value , newdata =test_set_value))

# training set rmse = 0.5079681;testing set rmse=0.4915408;

#pruning - Needed?
# cv_results = cv.tree(tree_value)
# plot(cv_results$size,cv_results$dev,type="b")
# indicesOrderedByDev = order(cv_results$dev,decreasing = FALSE)
# indexForLowestDev = indicesOrderedByDev[1]
# best_size = cv_results$size[indexForLowestDev] # k=4
# more detailed subtrees

cp.grid = expand.grid(.cp = (0:10)*0.001)
tree_reg_value = train(review_scores_value~.,
                       data = training_set_value, method = 'rpart',
                       trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
                       tuneGrid = cp.grid )
tree_reg_value
best.tree.value = tree_reg_value$finalModel
prp(best.tree.value) # the best cp is  0.004

rmse (training_set_value$review_scores_value,predict (best.tree.value , newdata =training_set_value)) 
rmse (test_set_value$review_scores_value, predict (best.tree.value , newdata =test_set_value))

# RMSE for test set = 0.4823962, RMSE for training set = 0.4982808;


# ****** KNN Model *******
tuneGrid = expand.grid(k = seq(1,59, by = 2))
knn_reg_value = train(review_scores_value ~ host_is_superhost + calculated_host_listings_count +
                        number_of_reviews + availability_90 +
                        counts_amenities,
                      data = training_set_value, method = 'knn',
                      preProcess = c('center','scale'),
                      trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
                      tuneGrid = tuneGrid)

knn_results_value= knn_reg_value$results
knn_results_value
plot(knn_reg_value)
varImp(knn_reg_value) 
# Variables of importance -
# host_is_superhost              100.000
# counts_amenities                53.404
# availability_90                  8.962
# calculated_host_listings_count   6.485
# number_of_reviews                0.000


pred_knn_value = predict(knn_reg_value, newdata = test_set_value)
RMSE(pred_knn_value,test_set_value$review_scores_value)
# Test RMSE = 0.4885923 , Training RMSE = 0.5050278 (lowest for k = 59)


# *************************************** END OF PROJECT ***************************************
