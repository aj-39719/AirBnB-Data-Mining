# ***** Exploratory Data Analysis ******
library(data.table)
library(dplyr)
library(ggplot2)
install.packages('ggthemes')
library(ggthemes)
library(RColorBrewer)
install.packages("ggridges")
library(ggridges)
install.packages("cowplot")
library(cowplot)

# path = '/Users/admin/Downloads/listings.csv'
airbnb = fread(input = "listings.csv")
# airbnb = fread(input = path)


# Check NA values as a percentage of total data
hist(colMeans(is.na(airbnb)),
     labels = TRUE,
     col = "darkblue",
     main = "NA Values as a percentage of Data",
     xlab = "Mean NA Values",
     border = "white",
     ylim = c(0,65))


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
  #axis.text.y = element_text(size = 10, face = "bold"),
  #axis.title.x = element_text(size = 11),
  #axis.title.y = element_text(size = 11),
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
