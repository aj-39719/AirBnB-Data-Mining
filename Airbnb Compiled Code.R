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
#install.packages("polywog")
#install.packages("rpart.plot")
#install.packages("kernlab")
#install.packages("ggthemes")
#install.packages("RColorBrewer")
#install.packages("ggridges")
#install.packages("cowplot")
library(RColorBrewer)
library(ggridges)
library(cowplot)
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
library(leaps)

path = '/Users/admin/Downloads/listings.csv'
airbnb = fread(input = "listings.csv")
airbnb = fread(input = path)

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
airbnb$price_calc = as.numeric(gsub("\\$", "", airbnb$price)) 

#imputing mean values by group
airbnb$price_calc[is.na(airbnb$price_calc)] = ave(airbnb$price_calc, airbnb$room_type, 
                                        FUN = function(x) 
                                          mean(x, na.rm = TRUE))[c(which(is.na(airbnb$price_calc)))]

#calculating average room price
mean_room_type <- aggregate(list(average_price = airbnb$price_calc), 
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

top_10_neighbourhood <- aggregate(list(airbnb$price_calc), list(airbnb$neighbourhood_cleansed), mean)
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

e + coord_polar(clip = "off",direction = 1)+
  scale_fill_manual(values = getPalette(colourCount))+
  aes(x=reorder(neighbourhood,Average_price_per_neighborhood))+
  xlab(" Neighbourhood ") +
  ylab("") +
  theme(axis.text.x = element_text(angle=20))


f <- ggplot(data = top_expensive,
            mapping = aes(x = neighbourhood, 
                          y = Average_price_per_neighborhood)) +
  geom_bar(stat = "identity",
           mapping = aes(fill = neighbourhood),
           alpha = .8, size = 0) +
  geom_label(mapping = aes(label = round(Average_price_per_neighborhood, 1)),
             size = 2.5, fill = "#F5FFFA", fontface = "bold") +
  theme_minimal() +
  ggtitle("Top 10 expensive neighborhoods") +
  tema2



f  + coord_polar(clip = "off",direction = -1)+
  scale_fill_manual(values = getPalette(colourCount))+
  aes(x=reorder(neighbourhood,Average_price_per_neighborhood))+
  xlab(" Neighbourhood ") +
  ylab("") +
  theme(axis.text.x = element_text(angle=12))

# removing column created for data analysis
airbnb = subset(airbnb, select = -c(price_calc))

# ********************************* DATA CLEANING FOR PREDICTION **********************************


# removes listings with no stays or corrupted/incomplete reviews
airbnb = airbnb[!is.na(airbnb$review_scores_location) & 
                !is.na(airbnb$review_scores_checkin) & 
                !is.na(airbnb$review_scores_cleanliness) & 
                !is.na(airbnb$review_scores_value),]

# turns all NA join times into the mean join time
airbnb$join_time[which(is.na(airbnb$join_time))] = mean(airbnb$join_time[which(!is.na(airbnb$join_time))])

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
keepT90 = names(summary)[1:90] # top 90 most frequent amenities
keepT90 = keepT90[-c(29,32,38,74)] # remove duplicate shampoo washer Hot water heating

# keep only top 90 amenities and discard the rest
for (i in 1:length(clean_amenities)) {
  temp_index = clean_amenities[[i]][1:length(clean_amenities[[i]])] %in% keepT90
  clean_amenities[[i]] = clean_amenities[[i]][temp_index]
}

# Another way to split amenites cols :- 

# Extract amenities from airbnb data set
airbnb_amenities = airbnb [,c("amenities")]

# Generate a long list of all strings below this cols; Run slowly-around 3 minutes
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


# Since the columns where response time was NA, the response rate was also NA, hence we equate it to 0
airbnb$host_response_rate[is.na(airbnb$host_response_rate)] = 0 

# Converting price column to numeric type
airbnb$price = as.numeric(gsub("\\$", "", airbnb$price)) 

# Imputing values for Price by Room_type and neighborhood (351 values)
airbnb[,price := replace(price, is.na(price), median(price, na.rm=TRUE)),
             by=.(room_type,neighbourhood_cleansed)]


# Remove irrelevant columns with too many missing values that cant be imputed
airbnb = airbnb[!is.na(airbnb$host_is_superhost) & 
                  !is.na(airbnb$host_total_listings_count) &
                  !is.na(airbnb$host_identity_verified)&
                  !is.na(airbnb$host_location)]




# ********************************* TRAINING AND TESTING SPLIT **********************************

set.seed(1)

# Removing columns with more than 32 factors (Decision Tree Requirement)
airbnb = subset(airbnb, select = -c(host_location,host_neighbourhood, 
                                    neighbourhood_cleansed, property_type))

test_set_indices = sample(1:nrow(airbnb),round(0.3*nrow(airbnb)),replace = FALSE)
training_set = airbnb[-test_set_indices,]
test_set = airbnb[test_set_indices,]

# Removing spaces from column names
names(training_set) <- gsub(" ", "_", names(training_set))
names(test_set) <- gsub(" ", "_", names(test_set))

# IMPUTING VALUES ON TRAINING SET :-

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



# ********************************** PREDICTING LOCATION REVIEWS *************************************

set.seed(1)
airbnb_location = airbnb[,c("latitude","longitude","review_scores_location")]
colMeans(is.na(airbnb_location)) # no missing value
summary(airbnb_location)


# EDA
hist(training_set$latitude, main = "Histogram of latitude", xlab = "latitude") 
# looks like normal distribution

hist(training_set$longitude,main = "Histogram of longitude", xlab = "longitude")
# looks like normal distribution

hist(training_set$review_scores_location,main = "Histogram of location score", xlab = "location score")
# skewed to the right, needs some transformation

log_location = log(training_set$review_scores_location)# taking logarithm
hist(log_location,main = "Histogram of log(location score)", xlab = "log(location score)")
# still not normal

pairs(airbnb_location) 
corrplot(cor(airbnb_location), method = "circle", diag = FALSE,type = "upper")

# 1) latitude is positively correlated with longitude
# 2) latitude does not correlated with location score
# 3) longitude is slightly negatively correlated with location score


# LINEAR REGRESSION MODEL

lm_reg = train(review_scores_location ~ latitude + longitude,
               data = training_set, method = 'lm',
               trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
               preProcess = c('center','scale'))
lm_reg$finalModel
# Result is consistent with correlation table, showing that latitude does not play a significant role 
# in the model and longitude is negatively related to the location score.
# linear model is location score = -0.026123*scale(longitude)+0.002919*scale(latitude)+4.746319
# it is not a good model since r square is almost zero, which means it doesn't explain any variation
# Training RMSE = 0.4341842

rmse(test_set$review_scores_location,
     predict(lm_reg,test_set[,c("latitude","longitude"),drop=FALSE])) 
# Testing RMSE = 0.4009447   
par(mfrow=c(2,2))
plot(lm_reg$finalModel) 
# implies residual is not normally distributed,thus confirmed that this model doesn't explain.

# Model selection
regfit_full = regsubsets(review_scores_location~. , data=training_set)
summary_full = summary(regfit_full)
plot(summary_full$rss,type="b")

# LINEAR REGRESSION WITH LONGITUDE ONLY

lm_reg_onlyl = train(review_scores_location ~ longitude,
                     data = training_set, method = 'lm',
                     trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
                     preProcess = c('center','scale'))

rmse(test_set$review_scores_location,predict(lm_reg_onlyl,test_set[,c("latitude","longitude"),drop=FALSE]))
# Training RMSE = 0.4340705, Test RMSE = 0.4009162

par(mfrow=c(2,2))
plot(lm_reg_onlyl$finalModel) # still pretty bad

# LINEAR REGRESSION ON LOG OF REVIEW SCORES

lm2_reg = train(log(review_scores_location+1) ~ latitude + longitude, 
                # Since log0 is undefined, we will add 1 to each score to avoid it.
                data = training_set, method = 'lm',
                trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
                preProcess = c('center','scale'))
lm2_reg
summary(lm2_reg$finalModel)
rmse(log(test_set$review_scores_location+1),predict(lm2_reg,test_set[,c("latitude","longitude"),drop=FALSE]))
plot(lm2_reg$finalModel)
# Training RMSE = 0.1017434, Test RMSE =  0.08886645 
# plot still bad - heavy tails

# POLYNOMIAL REGRESSION

poly_reg = cv.polywog(review_scores_location ~ scale(latitude) + scale(longitude),
                      data = training_set, degrees.cv = 1:20,
                      nfolds = 10,thresh = 1e-4)
poly_reg$degree.min # the best degree is 6
poly_reg$polywog.fit
poly_reg_model<- bootPolywog(poly_reg$polywog.fit, nboot = 3)
rmse(test_set$review_scores_location,
     predict(poly_reg_model,test_set[,c("latitude","longitude"),drop=FALSE])) 

rmse(training_set$review_scores_location,
     predict(poly_reg_model,training_set[,c("latitude","longitude"),drop=FALSE]))
plot(poly_reg_model)
# Training RMSE = 0.4283884, Test RMSE = 0.3961075 


# DECISION TREE REGRESSION

tree_location <- tree(review_scores_location ~ . -review_scores_rating                       
                      -review_scores_accuracy                    
                      -review_scores_cleanliness      
                      -review_scores_checkin                      
                      -review_scores_communication             
                      -review_scores_location  
                      -review_scores_value
                      , data=training_set)
summary(tree_location)
plot (tree_location)
text (tree_location , pretty = 0)

# Cross-validation to choose the best tree
cp.grid = expand.grid(.cp = (0:10)*0.001)
tree_reg = train(review_scores_location ~ latitude + longitude,
                 data = training_set, method = 'rpart',
                 trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
                 tuneGrid = cp.grid )
tree_reg 
best.tree = tree_reg$finalModel

prp(best.tree) # the best cp is 0.001                
mean ((predict (best.tree , newdata =training_set) - training_set$review_scores_location)^2) 
mean ((predict (best.tree , newdata =test_set) - test_set$review_scores_location)^2)  
#  training data: MSE = 0.1781331; RMSE = 0.4220582 ;testing data: MSE = 0.1378898; RMSE = 0.3713352


# SVM REGRESSION : RADIAL KERNEL

#svm_reg_linear = train(review_scores_location ~ latitude + longitude,
#                data = training_set, method = 'svmRadial',
#                trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
#                preProcess = c('center','scale'),
#                 tuneGrid = expand.grid(C = seq(0, 1, length = 10),sigma = c(0.1,0.2)))
# Since data set is very large over 40000 instance, when I run the code for tuning parameter it doesn't work.

# fit in radial kernel 
plot(training_set[,1:2],col=training_set$review_scores_location+3,asp=0) 

# EPS REGRESSION: no control of number of support vectors

svmfit1 = svm(review_scores_location~. ,data=training_set, kernel="radial" ,scale=TRUE) # needs around 5 minutes to run
summary(svmfit1) 
rmse(training_set$review_scores_location,predict(svmfit1,training_set[,1:2])) 
rmse(test_set$review_scores_location,predict(svmfit1,test_set[,1:2]))
# Testing set RMSE = 0.4064776 ; Training set RMSE = 0.4404114

# SVM REGRESSION : LINEAR KERNEL

svmfit2 = svm(review_scores_location~. ,data=training_set, kernel="linear" ,scale=TRUE)
rmse(training_set$review_scores_location,predict(svmfit2,training_set[,1:2]))
rmse(test_set$review_scores_location,predict(svmfit2,test_set[,1:2]))
# testing set RMSE =0.4192826 ; training set RMSE = 0.4533505

# SVM REGRESSION : POLYNOMIAL KERNEL

svmfit3 = svm(review_scores_location~. ,data=training_set, kernel="polynomial" ,scale=TRUE)
rmse(training_set$review_scores_location,predict(svmfit3,training_set[,1:2]))
rmse(test_set$review_scores_location,predict(svmfit3,test_set[,1:2]))
# testing set RMSE =  0.4199995 # training set RMSE = 0.4537708


# KNN REGRESSION

tuneGrid = expand.grid(k = seq(1,59, by = 2))

#Warning - the next line of code takes about 8 minutes to execute
knn_reg = train(review_scores_location ~ latitude + longitude,
                data = training_set, method = 'knn',
                preProcess = c('center','scale'),
                trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
                tuneGrid = tuneGrid)

knn_results = knn_reg$results #lowest testing RMSE for k = 59
knn_results
plot(knn_reg)
varImp(knn_reg) #it is taking longitude to be the most important variable of importance, as seen above

pred_knn = predict(knn_reg, newdata = test_set)
RMSE(pred_knn,test_set$review_scores_location)
# Test RMSE = 0.4201638 ; Training RMSE = 0.4088177



# ********************************** PREDICTING COMMUNICATION REVIEWS ***********************************

airbnb_communication = airbnb |> select(-host_acceptance_rate,-host_neighbourhood,
                                        -host_location, -property_type, -neighbourhood_cleansed,
                                        -review_scores_rating, - review_scores_accuracy,
                                        -review_scores_cleanliness, -review_scores_checkin,
                                        -review_scores_location, -review_scores_value)



colSums(is.na(airbnb_communication)) # no missing value


# ***** Decision Tree Model ******
tree_communication <- tree(review_scores_communication ~ . -review_scores_rating                       
                      -review_scores_accuracy                    
                      -review_scores_cleanliness      
                      -review_scores_checkin                      
                      -review_scores_communication             
                      -review_scores_location  
                      -review_scores_value
                      , data=training_set)
summary(tree_communication)
plot (tree_communication)
text (tree_communication , pretty = 0)

pred_comm_tree = predict(tree_communication, newdata = test_set)
RMSE(pred_comm_tree,test_set$review_scores_communication)
# Test RMSE = 0.4422002



# Cross-validation to choose the best tree
cp.grid = expand.grid(.cp = (0:10)*0.001)
tree_reg = train(review_scores_communication ~ . -review_scores_rating                       
                 -review_scores_accuracy                    
                 -review_scores_cleanliness      
                 -review_scores_checkin                      
                 -review_scores_communication             
                 -review_scores_location  
                 -review_scores_value,
                 data = training_set, method = 'rpart',
                 trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
                 tuneGrid = cp.grid )
tree_reg 
best.tree = tree_reg$finalModel

prp(best.tree) # the best cp is 0.001                
mean ((predict (best.tree , newdata =training_set) - training_set$review_scores_location)^2) 
mean ((predict (best.tree , newdata =test_set) - test_set$review_scores_location)^2)  
#  training data: MSE = 0.1781331; RMSE = 0.4220582 ;testing data: MSE = 0.1378898; RMSE = 0.3713352



# ****** KNN Model *******

tuneGrid = expand.grid(k = seq(1,59, by = 2))
knn_reg = train(review_scores_communication ~ calculated_host_listings_count + availability_90,
                data = training_set, method = 'knn',
                preProcess = c('center','scale'),
                trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
                tuneGrid = tuneGrid)

knn_results = knn_reg$results # No result for RMSE, too many ties


# ********************************** PREDICTING CHECKIN REVIEWS ***********************************
##clean data for check in score
# we select acceptance rate, response time, super host, price, instant bookable as attributes
set.seed(1)
check_in = airbnb[, c("host_acceptance_rate","host_response_time","host_is_superhost","price","instant_bookable","review_scores_checkin")]
colMeans(is.na(check_in))
# missing value col:host_acceptance_rate, host_response_time , host_is_superhost
# 45% and 49% missing rate in acceptance rate and response rate respectively
summary(check_in)

# host reSponse time: categorical; present NA as a new category
table(check_in$host_response_time)
# combine small cases into a large group: NA,quick,slow
check_in$host_response_time[is.na(check_in$host_response_time)] = "NA"
check_in$host_response_time[check_in$host_response_time == "within an hour"|check_in$host_response_time == "within a few hours"] = "quick"
check_in$host_response_time[check_in$host_response_time == "within a day"|check_in$host_response_time == "a few days or more"] = "slow"


# host_is_superhost; present na as a new category
table(check_in$host_is_superhost)# small portion of missing value may carry important information
check_in$host_is_superhost[is.na(check_in$host_is_superhost)] = "NA"
check_in$host_is_superhost[check_in$host_is_superhost == "t"] = 1
check_in$host_is_superhost[check_in$host_is_superhost == "f"] = 0

# price
check_in$price=gsub("\\$", "", check_in$price)
check_in$price=gsub(",", "", check_in$price)
check_in$price = as.numeric(check_in$price )

#instant bookable
check_in$instant_bookable[check_in$instant_bookable == "t"] = 1
check_in$instant_bookable[check_in$instant_bookable == "f"] = 0

#host_acceptance_rate
check_in$host_acceptance_rate = (as.numeric(gsub("%","",check_in$host_acceptance_rate)))/100
#impute unknown by the median in training set
test_set_indices = sample(1:nrow(check_in),round(0.3*nrow(check_in)),replace = FALSE)
training_set = check_in[-test_set_indices,]
test_set = check_in[test_set_indices,]
summary(training_set) # median = 0.910 
training_set$host_acceptance_rate[is.na(training_set$host_acceptance_rate)] = 0.910
test_set$host_acceptance_rate[is.na(test_set$host_acceptance_rate)] = 0.910
# convert all character into factor
library(dplyr)
training_set = training_set %>% mutate_if(sapply(training_set, is.character), as.factor)
test_set = test_set %>% mutate_if(sapply(test_set, is.character), as.factor)
# check
colMeans(is.na(training_set))
colMeans(is.na(test_set))
summary(training_set)
summary(test_set)

# EDA
continuous_var = training_set[,c("host_acceptance_rate","price","review_scores_checkin")]
corrplot(cor(continuous_var), method = "circle", diag = FALSE)
# shows price and acceptance rate both are slightlt negtively correlated with check in score;
# almost no relationship between attributes
training_set %>% ggplot(aes(y=review_scores_checkin, x=factor(host_response_time)))+ geom_boxplot() 
training_set %>% ggplot(aes(y=review_scores_checkin, x=factor(host_is_superhost)))+ geom_boxplot() 
training_set %>% ggplot(aes(y=review_scores_checkin, x=factor(instant_bookable)))+ geom_boxplot() 

##linear regression
# convert them factor into numeric 
training_set1 = training_set %>% mutate_if(sapply(training_set, is.factor), as.numeric)
test_set1 = test_set %>% mutate_if(sapply(test_set, is.factor), as.numeric)
lm_reg = train( review_scores_checkin~.,
                data =training_set1 ,
                method = 'lm',
                trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
                preProcess = c('center','scale'))
lm_reg$finalModel
summary(lm_reg$finalModel)
# all variables are significant,
par(mfrow=c(2,2))
plot(lm_reg$finalModel) # residual plots are bad.
# rmse for training set:  0.4749543; rmse for test set is 0.4459939
rmse(training_set1$review_scores_checkin,predict(lm_reg,training_set1[,-c("review_scores_checkin"),drop=FALSE]))
rmse(test_set1$review_scores_checkin,predict(lm_reg,test_set1[,-c("review_scores_checkin"),drop=FALSE]))

## Decision tree regression
library(rpart.plot)
tree_checkin<- tree (review_scores_checkin~. , data=training_set)
summary(tree_checkin )
plot (tree_checkin)
text (tree_checkin , pretty = 0)
# rmse in training set = 0.4778013; rmse in testing set is 0.4478906
mean ((predict (tree_checkin , newdata =training_set) - training_set$review_scores_checkin)^2) 
mean ((predict (tree_checkin , newdata =test_set) - test_set$review_scores_checkin)^2)
# only care about superhost: if it is superhost, then it would be higher score
# use cv for a more detailed subtree
cp.grid = expand.grid(.cp = (0:10)*0.001)
tree_reg = train(review_scores_checkin~.,
                 data = training_set1, method = 'rpart',
                 trControl = trainControl(method = 'repeatedcv' , number = 10, repeats = 3),
                 tuneGrid = cp.grid )
tree_reg 
best.tree = tree_reg$finalModel
prp(best.tree) # the best cp is 0.001  
# rmse for training set is 0.4719312;
mean ((predict (best.tree , newdata =training_set1) - training_set1$review_scores_checkin)^2) 
# rmse for training set is 0.4440508
mean ((predict (best.tree , newdata =test_set1) - test_set1$review_scores_checkin)^2) 

#log transform check in score perform still not good

## SVM Regression: choose radial kernel
# fit in radial kernel 
# eps-regression: no control of number of support vectors
svmfit1 = svm(review_scores_checkin~. ,data=training_set1, kernel="radial" ,scale=TRUE) # needs around 5 minutes to run
summary(svmfit1) 
# training set rmse = 0.4959456
rmse(training_set1$review_scores_checkin,predict(svmfit1,training_set1)) 
# Testing set rmse = 0.4646491
rmse(test_set1$review_scores_checkin,predict(svmfit1,test_set1))

# fit in linear kernal svm regression
svmfit2 = svm(review_scores_checkin~. ,data=training_set1, kernel="linear" ,scale=TRUE)
# training set rmse = 0.5046351
rmse(training_set1$review_scores_checkin,predict(svmfit2,training_set1))
# testing set rmse = 0.4731245
rmse(test_set1$review_scores_checkin,predict(svmfit2,test_set1))


# *********************************** PREDICTING RATING REVIEWS **********************************




# ********************************** PREDICTING ACCURACY REVIEWS *********************************




# ********************************** PREDICTING VALUE REVIEWS ************************************




# ********************************** PREDICTING CLEANLINESS REVIEWS ***********************************
