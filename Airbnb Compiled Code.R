# ******************************* IMPORTING DATA AND LIBRARIES ********************************** 
#install.packages("BBmisc")
#install.packages("data.table")'#install.packages("caret")
#install.packages("ggplot2")
#install.packages("corrplot")
#install.packages("tree")
#install.packages("e1071")
#install.packages("leaps")
#install.packages("ModelMetrics")
# install.packages("polywog")
# install.packages("rpart.plot")
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

path = '/Users/admin/Downloads/listings.csv'
#airbnb = fread(input = "listings.csv")
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


# ********************************** PRELIMINARY DATA CLEANING *********************************


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



# **************************** EXPLORATORY DATA ANALYSIS OF RAW DATA *******************************


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
keepT90 = names(summary)[1:90] # top 90 most frequent ammenities
keepT90 = keepT90[-c(29,32,38,74)] # remove duplicate shampoo washer Hot water heating

# keep only top 90 amenities and discard the rest
for (i in 1:length(clean_amenities)) {
  temp_index = clean_amenities[[i]][1:length(clean_amenities[[i]])] %in% keepT90
  clean_amenities[[i]] = clean_amenities[[i]][temp_index]
}

# Another way to split amentites cols :- 

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



# Encode t/f to binary variable 1/0;
airbnb$host_is_superhost[airbnb$host_is_superhost == "t"] = 1
airbnb$host_is_superhost[airbnb$host_is_superhost == "f"] = 0
airbnb$host_has_profile_pic[airbnb$host_has_profile_pic == "t"] = 1
airbnb$host_has_profile_pic[airbnb$host_has_profile_pic == "f"] = 0
airbnb$host_identity_verified[airbnb$host_identity_verified == "t"] = 1
airbnb$host_identity_verified[airbnb$host_identity_verified == "f"] = 0



# ********************************** PREDICTING LOCATION REVIEWS ****************************




