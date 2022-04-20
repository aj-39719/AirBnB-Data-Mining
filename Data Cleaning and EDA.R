# ***** Exploratory Data Analysis ******
library(data.table)

path = '/Users/admin/Downloads/listings.csv'
#airbnb = fread(input = "listings.csv")
airbnb = fread(input = path)

# Check the structure of the table
str(airbnb)

# Check NA values per column
colMeans(is.na(airbnb))

# we can also use this command instead : sapply(airbnb, function(x) sum(is.na(x)))


# Check NA values as a percentage of total data
hist(colMeans(is.na(airbnb)),
     labels = TRUE,
     col = "darkblue",
     main = "NA Values as a percentage of Data",
     xlab = "Mean NA Values",
     border = "white",
     ylim = c(0,65))


# Lets plot the numeric data in our dataset, to get a clearer picture of the data
str(airbnb)


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

# We remove rows where total listings by a host = 0 
#airbnb = airbnb[-which(airbnb$host_total_listings_count==0)]

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



