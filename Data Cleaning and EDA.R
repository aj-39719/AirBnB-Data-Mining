# ***** Exploratory Data Analysis ******

airbnb = fread(input = "listings.csv")

# Check the structure of the table
str(airbnb)

# Check NA values per column
colSums(is.na(airbnb))

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

cols_to_be_removed = c("neighbourhood_group_cleansed","calendar_updated","license")
# We will keep the bathroom for now, as we plan on extractinf that data from bathrooms_text

# Removing Blank columns, note how the variables in our dataframe drop from 74 to 71
airbnb = subset(airbnb, select = !(names(airbnb) %in% cols_to_be_removed))

# Lets remove columns not required for our analysis 
more_cols_removed = c("id","listing_url","scrape_id","last_scraped","name","description","neighborhood_overview",
                      "picture_url","host_id","host_url","host_name","host_since","host_about","host_thumbnail_url",
                      "host_picture_url","calendar_last_scraped")

airbnb = subset(airbnb, select = !(names(airbnb) %in% more_cols_removed))


