# ***** Exploratory Data Analysis ******
library(data.table)

#path = '/Users/admin/Downloads/listings.csv'
airbnb = fread(input = "listings.csv")
#airbnb = fread(input = path)

# ***** Cleaning the Data ******

blank_cols = c("neighbourhood_group_cleansed","calendar_updated","license")
# We will keep the bathroom column for now, as we can extract that data from bathrooms_text

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

# removes listings with no stays or corrupted/incomplete reviews
airbnb = airbnb[!is.na(airbnb$review_scores_location) & !is.na(airbnb$review_scores_checkin) & !is.na(airbnb$review_scores_cleanliness) & !is.na(airbnb$review_scores_value),]

# turns all NA join times into the mean join time
airbnb$join_time[which(is.na(airbnb$join_time))] = mean(airbnb$join_time[which(!is.na(airbnb$join_time))])

# count number of verification ways
#install.packages("BBmisc")
library(BBmisc)
verification = airbnb [,c("host_verifications")]
counts_verification = c()
for (i in 1: dim(verification)[1]){
  a = toString(verification[i])
  counts_verification[i] = length(explode( a,','))
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

# ******** KNN Regression ************

airbnb_knn = airbnb

#imputing values for bathroom by Room_type (168 values)
airbnb_knn[,bathrooms := replace(bathrooms, is.na(bathrooms), 
                                 median(bathrooms, na.rm=TRUE)),by=.(room_type)]

#imputing values for beds by Room_type (2188 values)
airbnb_knn[,beds := replace(beds, is.na(beds), 
                                 median(beds, na.rm=TRUE)),by=.(room_type)]

#imputing values for bedrooms by Room_type (2520 values)
airbnb_knn[,bedrooms := replace(bedrooms, is.na(bedrooms), 
                                 median(bedrooms, na.rm=TRUE)),by=.(room_type)]

#converting price column to numeric type
airbnb_knn$price = as.numeric(gsub("\\$", "", airbnb_knn$price)) 

#imputing values for bedrooms by Room_type (351 values)
airbnb_knn[,price := replace(price, is.na(price), 
                                median(price, na.rm=TRUE)),by=.(room_type,neighbourhood_cleansed)]


# removing columns not important for prediction of target variables
airbnb_knn = airbnb_knn |> select(-host_location,-host_response_time,-host_response_rate,
                                  -host_acceptance_rate,-host_is_superhost,-host_neighbourhood,
                                  -host_total_listings_count,-calculated_host_listings_count,
                                  -calculated_host_listings_count_entire_homes,
                                  -calculated_host_listings_count_private_rooms,
                                  -calculated_host_listings_count_shared_rooms,
                                  -amenities,-host_verifications, -review_scores_accuracy,
                                  -review_scores_cleanliness,-review_scores_checkin,
                                  -review_scores_communication,
                                  #-review_scores_location)
                                  -review_scores_value)

# Convert all Character type variables to Factors
airbnb_knn <- airbnb_knn %>%
  mutate_if(sapply(airbnb_knn, is.character), as.factor)


library(caret)
airbnb_knn = na.omit(airbnb_knn) #removing 39 missing values

set.seed(1)

training_partition = createDataPartition(airbnb_knn$review_scores_rating, p = 0.7, list = FALSE)


train_knn = airbnb_knn[training_partition,]
test_knn = airbnb_knn[-training_partition,]

set.seed(1)

tuneGrid = expand.grid(k = seq(1,59, by = 2))
knn_reg = train(review_scores_location ~ latitude + longitude + neighbourhood_cleansed,
                data = train_knn, method = 'knn',
                preProcess = c('center','scale'),
                trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
                tuneGrid = tuneGrid)

knn_results = knn_reg$results

library(ggplot2)

knn_results |> ggplot(aes(x = k, y =  RMSE)) + geom_point() + geom_line()

plot(knn_reg)
varImp(knn_reg)

pred_knn = predict(knn_reg, newdata = test_knn)
RMSE(pred_knn,test_knn$review_scores_location)
#Therefore, we get an RMSE value of 0.4054 
#for KNN regression on Location with latitude and longitude



# *** More Data Cleaning ***


# Clean verifications & amenities and count unique
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

#### Another way to split amentites cols ###
# Extract amenities from airbnb data set
airbnb_amenities = airbnb [,c("amenities")]

# Generate a long list of all strings below this cols; Run slowly-around 3 minutes
install.packages("BBmisc")
library(BBmisc)
total_amenities = c()
for ( i in 1: dim(airbnb_amenities)[1]){
  a = airbnb_amenities[i,]
  a = toString(a)
  a = explode( a,',')
  c =  gsub("\\[","",a)
  c =  gsub("\\]","",c)
  c = gsub('"',' ',c)
  total_amenities = c(total_amenities,c)
}

# Select top 10 most frequent cases
table1 = data.frame (total_amenities)
w = table(total_amenities)
w1 = data.frame (w)
w2 <- w1[order(w1$Freq, decreasing = TRUE), ]
top_10 = w2$total_amenities[1:10]

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