# AirBnB-Data-Mining

Notes/ideas:

Proposed change:
host_listings_count is a duplicate, neighbourhood_cleansed is a better version of neighbourhood which for the most part just says London, bathrooms is blank,
the other columns I propose to drop are either duplicates, or when not duplicates thee information such as maximum_minimum_nights is about
their entire property portfolio, which we don't care about as we need information about the property in question. 
Lastly I remove all rows of properties without a single past stay.
###
even_more_cols_removed = c("host_listings_count","neighbourhood","bathrooms","minimum_minimum_nights","maximum_minimum_nights","minimum_maximum_nights","maximum_maximum_nights","minimum_nights_avg_ntm","maximum_nights_avg_ntm")

airbnb = subset(airbnb, select = !(names(airbnb) %in% even_more_cols_removed))

airbnb = airbnb[-which(airbnb$host_total_listings_count==0)]
###
