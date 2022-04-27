library(data.table)
library(dplyr)
library(caret)
library(ggplot2)
library(ModelMetrics)

#path = '/Users/admin/Downloads/listings.csv'
airbnb = fread(input = "listings.csv")
#airbnb = fread(input = path)

# PASTE DATA CLEANING CODE HERE BEFORE RUNNING

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


#Since the columns where response time was NA, the response rate was also NA, hence we equate it to 0
airbnb$host_response_rate[is.na(airbnb$host_response_rate)] = 0 

#converting price column to numeric type
airbnb$price = as.numeric(gsub("\\$", "", airbnb$price)) 

#imputing values for Price by Room_type (351 values)
training_set[,price := replace(price, is.na(price), median(price, na.rm=TRUE)),
             by=.(room_type,neighbourhood_cleansed)]


# Remove Irrelevant columns with too many missing values that cant be imputed
airbnb = airbnb[!is.na(airbnb$host_is_superhost) & 
                !is.na(airbnb$host_total_listings_count) &
                !is.na(airbnb$host_identity_verified)&
                !is.na(airbnb$host_location)]


library(tree)

set.seed(1)

airbnb_communication = airbnb |> select(-host_acceptance_rate,-host_neighbourhood,
                                        -host_location, -property_type, -neighbourhood_cleansed,
                                        -review_scores_rating, - review_scores_accuracy,
                                        -review_scores_cleanliness, -review_scores_checkin,
                                        -review_scores_location, -review_scores_value)



colSums(is.na(airbnb_communication)) # no missing value
summary(airbnb_communication)


# Divide into test dataset 30% and training dataset 70%
test_set_indices = sample(1:nrow(airbnb_communication),round(0.3*nrow(airbnb_communication)),replace = FALSE)
training_set = airbnb_communication[-test_set_indices,]
test_set = airbnb_communication[test_set_indices,]

# **** Imputing values on the training dataset ***********

#imputing values for bathroom by Room_type (168 values)
training_set[,bathrooms := replace(bathrooms, is.na(bathrooms), 
                                 median(bathrooms, na.rm=TRUE)),by=.(room_type)]

#imputing values for beds by Room_type (2188 values)
training_set[,beds := replace(beds, is.na(beds), 
                            median(beds, na.rm=TRUE)),by=.(room_type)]

#imputing values for bedrooms by Room_type (2520 values)
training_set[,bedrooms := replace(bedrooms, is.na(bedrooms),median(bedrooms, na.rm=TRUE)),by=.(room_type)]


# Convert all Character type variables to Factors
training_set = training_set %>% mutate_if(sapply(training_set, is.character), as.factor)
test_set = test_set %>% mutate_if(sapply(test_set, is.character), as.factor)



# ***** Decision Tree Model ******

tree_model = tree(review_scores_communication ~. , data = training_set)
tree_model
plot(tree_model)
text(tree_model, pretty = 0)

tree_pred = predict(tree_model, test_set)
mean((tree_pred - test_set$review_scores_communication)^2) # Test MSE = 0.0839 and RMSE = 0.2897


# ****** KNN Model *******

tuneGrid = expand.grid(k = seq(1,59, by = 2))
knn_reg = train(review_scores_communication ~ calculated_host_listings_count + availability_90,
                data = training_set, method = 'knn',
                preProcess = c('center','scale'),
                trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
                tuneGrid = tuneGrid)

knn_results = knn_reg$results # No result for RMSE, too many ties
