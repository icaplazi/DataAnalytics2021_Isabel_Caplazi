# Wind Energy Set

# download data set and export as csv file
# call wind electricity dataset as csv
Wind <- read.csv("C:/users/caplam/Downloads/UNdata_WindElec/UNdata_WindElec.csv")
View(Wind) #footnotes need to be filtered
Wind <- filter(Wind, Commodity...Transaction == "Electricity - total wind production")
View(Wind)

colnames(Wind) # column names
head(Wind) # gives first 6 rows
summary(Wind) 

# call necessary packages
library(dplyr) # dplyr package for filter function
library(ggplot2) # calls ggplot package


##### Australia #####
w_Australia <- filter(Wind, Country.or.Area == "Australia") # creation of Australia data set
View(w_Australia)
fivenum(w_Australia$Quantity,na.rm = TRUE)
mean(w_Australia$Quantity) # Australia Quantity column mean
median((w_Australia$Quantity)) # Australia Quantity column median
y <- table(w_Australia$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Australia Quantity mode(s)

# EDA
barplot(table(w_Australia$Quantity)) # gives the frequency of each production
plot1 <- ggplot(w_Australia, aes(Year, Quantity))
print(plot1 + geom_point(size=3))


##### Brazil #####
w_Brazil <- filter(Wind, Country.or.Area == "Brazil") # creation of Brazil data set
View(w_Brazil)
fivenum(w_Brazil$Quantity,na.rm = TRUE)
mean(w_Brazil$Quantity) # Brazil Quantity column mean
median((w_Brazil$Quantity)) # Brazil Quantity column median
y <- table(w_Brazil$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Brazil Quantity mode(s)

# EDA
barplot(table(w_Brazil$Quantity)) # gives the frequency of each production
plot2 <- ggplot(w_Brazil, aes(Year, Quantity))
print(plot2 + geom_point(size=3))


##### China #####
w_China <- filter(Wind, Country.or.Area == "China") # creation of China data set
View(w_China)
fivenum(w_China$Quantity,na.rm = TRUE)
mean(w_China$Quantity) # China Quantity column mean
median((w_China$Quantity)) # China Quantity column median
y <- table(w_China$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # China Quantity mode(s)

# EDA
barplot(table(w_China$Quantity)) # gives the frequency of each production
plot3 <- ggplot(w_China, aes(Year, Quantity))
print(plot3 + geom_point(size=3))


##### Germany #####
w_Germany <- filter(Wind, Country.or.Area == "Germany") # creation of Germany data set
View(w_Germany)
fivenum(w_Germany$Quantity,na.rm = TRUE)
mean(w_Germany$Quantity) # Germany Quantity column mean
median((w_Germany$Quantity)) # Germany Quantity column median
y <- table(w_Germany$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Germany Quantity mode(s)

# EDA
barplot(table(w_Germany$Quantity)) # gives the frequency of each production
plot4 <- ggplot(w_Germany, aes(Year, Quantity))
print(plot4 + geom_point(size=3))


##### India #####
w_India <- filter(Wind, Country.or.Area == "India") # creation of India data set
View(w_India)
fivenum(w_India$Quantity,na.rm = TRUE)
mean(w_India$Quantity) # India Quantity column mean
median((w_India$Quantity)) # India Quantity column median
y <- table(w_India$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # India Quantity mode(s)

# EDA
barplot(table(w_India$Quantity)) # gives the frequency of each production
plot5 <- ggplot(w_India, aes(Year, Quantity))
print(plot5 + geom_point(size=3))


##### Mexico #####
w_Mexico <- filter(Wind, Country.or.Area == "Mexico") # creation of Mexico data set
View(w_Mexico)
fivenum(w_Mexico$Quantity,na.rm = TRUE)
mean(w_Mexico$Quantity) # Mexico Quantity column mean
median((w_Mexico$Quantity)) # Mexico Quantity column median
y <- table(w_Mexico$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Mexico Quantity mode(s)

# EDA
barplot(table(w_Mexico$Quantity)) # gives the frequency of each production
plot6 <- ggplot(w_Mexico, aes(Year, Quantity))
print(plot6 + geom_point(size=3))


##### Russian Federation #####
w_Russian_Fed <- filter(Wind, Country.or.Area == "Russian Federation") # creation of Russian Federation data set
View(w_Russian_Fed)
fivenum(w_Russian_Fed$Quantity,na.rm = TRUE)
mean(w_Russian_Fed$Quantity) # Russian Federation Quantity column mean
median((w_Russian_Fed$Quantity)) # Russian Federation Quantity column median
y <- table(w_Russian_Fed$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Russian Federation Quantity mode(s)

# EDA
barplot(table(w_Russian_Fed$Quantity)) # gives the frequency of each production
plot7 <- ggplot(w_Russian_Fed, aes(Year, Quantity))
print(plot7 + geom_point(size=3))


##### United States #####
w_United_States <- filter(Wind, Country.or.Area == "United States") # creation of United States data set
View(w_United_States)
fivenum(w_United_States$Quantity,na.rm = TRUE)
mean(w_United_States$Quantity) # United States Quantity column mean
median((w_United_States$Quantity)) # United States Quantity column median
y <- table(w_United_States$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # United States Quantity mode(s)

# EDA
barplot(table(w_United_States$Quantity)) # gives the frequency of each production
plot8 <- ggplot(w_United_States, aes(Year, Quantity))
print(plot8 + geom_point(size=3))


######### Yearly analysis ##########
### 2019 ###
w_y2019 <- filter(Wind, Year == "2019") # creation of 2019 data set
View(w_y2019)
fivenum(w_y2019$Quantity,na.rm = TRUE)
mean(w_y2019$Quantity) # 2019 Quantity column mean
median((w_y2019$Quantity)) # 2019 Quantity column median
y <- table(w_y2019$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # 2019 Quantity mode(s)

# EDA
barplot(table(w_y2019$Quantity)) # gives the frequency of each production
plot9 <- ggplot(w_y2019, aes(Country.or.Area, Quantity))
print(plot9 + geom_point(size=3))


### 2018 ###
w_y2018 <- filter(Wind, Year == "2018") # creation of 2018 data set
View(w_y2018)
fivenum(w_y2018$Quantity,na.rm = TRUE)
mean(w_y2018$Quantity) # 2018 Quantity column mean
median((w_y2018$Quantity)) # 2018 Quantity column median
y <- table(w_y2018$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # 2018 Quantity mode(s)

# EDA
barplot(table(w_y2018$Quantity)) # gives the frequency of each production
plot10 <- ggplot(w_y2018, aes(Country.or.Area, Quantity))
print(plot10 + geom_point(size=3))


### 2017 ###
w_y2017 <- filter(Wind, Year == "2017") # creation of 2017 data set
View(w_y2017)
fivenum(w_y2017$Quantity,na.rm = TRUE)
mean(w_y2017$Quantity) # 2017 Quantity column mean
median((w_y2017$Quantity)) # 2017 Quantity column median
y <- table(w_y2017$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # 2017 Quantity mode(s)

# EDA
barplot(table(w_y2017$Quantity)) # gives the frequency of each production
plot11 <- ggplot(w_y2017, aes(Country.or.Area, Quantity))
print(plot11 + geom_point(size=3))


### 2016 ###
w_y2016 <- filter(Wind, Year == "2016") # creation of 2016 data set
View(w_y2016)
fivenum(w_y2016$Quantity,na.rm = TRUE)
mean(w_y2016$Quantity) # 2016 Quantity column mean
median((w_y2016$Quantity)) # 2016 Quantity column median
y <- table(w_y2016$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # 2016 Quantity mode(s)

# EDA
barplot(table(w_y2016$Quantity)) # gives the frequency of each production
plot11 <- ggplot(w_y2016, aes(Country.or.Area, Quantity))
print(plot11 + geom_point(size=3))



######### Overall Analysis of yearly wind data #########
Countries <- filter(Wind, Country.or.Area %in% c("Australia", "Brazil",
                    "China", "Germany", "India", "Mexico", "Russian Federation",
                    "United States")) # creation of specific countries data set

World_Wind <- ggplot(Wind, aes(Country.or.Area, Quantity, color = Year))
print(World_Wind + geom_point(size=3))

World_Wind_yr <- ggplot(Wind, aes(Country.or.Area, Year, color = Quantity))
print(World_Wind_yr + geom_point(size=3))

Countries_Wind <- ggplot(Countries, aes(Country.or.Area, Quantity, color = Year))
print(Countries_Wind + geom_point(size=3))

Countries_Wind_yr <- ggplot(Countries, aes(Country.or.Area, Year, color = Quantity))
print(Countries_Wind_yr + geom_point(size=3))


### classifications ###
# breaking down quantity column
# takes quantities and organizes them into ranges
summary(Wind$Quantity)
categ_all <- as.numeric(Wind$Quantity)
categ_all <- cut(categ_all, br = c(0,100,5000,50000,150000,400000), labels = c("Lowest Production",'Semi-Low Production','Middle Production','Semi-High Production','Highest Production'))
categ_all <- as.factor(categ_all)
summary(categ_all)

World_Wind_cat <- ggplot(Wind, aes(Country.or.Area, Quantity, color = categ_all))
print(World_Wind_cat + geom_point(size=3) +
        theme(axis.text.x=element_text(angle=90, size=8, vjust=0.5)))

World_Wind_yr_cat <- ggplot(Wind, aes(Country.or.Area, Year, color = categ_all))
print(World_Wind_yr_cat + geom_point(size=3))

summary(Countries$Quantity)
categ_countries <- as.numeric(Countries$Quantity)
categ_countries <- cut(categ_countries, br = c(0,100,5000,50000,150000,370000), labels = c("Lowest Production",'Semi-Low Production','Middle Production','Semi-High Production','Highest Production'))
categ_countries <- as.factor(categ_countries)
summary(categ_countries)

Countries_Wind_yr_cat <- ggplot(Countries, aes(Country.or.Area, Quantity, color = categ_countries))
print(Countries_Wind_yr_cat + geom_point(size=3))


### linear regression ###
Wind_year <- Wind$Year
Wind_prod <- Wind$Quantity
Wind_countries <- Wind$Country.or.Area

lmWind_prod <- lm(Wind_prod ~ Wind_year) # linear regression of year and quantity
lmWind_prod
summary(lmWind_prod) # summary of regression line, R^2 = 0.02028

lm_Wind <- lm(Wind_prod ~ Wind_year + Wind_countries) # linear regression of year and quantity and country
lm_Wind
summary(lm_Wind) # summary of regression line, R^2 = 0.5357

lm_Wind_yr <- lm(Wind_year ~ Wind_prod + Wind_countries) # linear regression of year and quantity and country
lm_Wind_yr
summary(lm_Wind_yr) # summary of regression line, R^2 = 0.3544


# lin reg by country
lm_Wind_Australia <- lm(w_Australia$Quantity ~ w_Australia$Year) # linear regression of year and quantity
lm_Wind_Australia
summary(lm_Wind_Australia) # summary of regression line, R^2 = 0.8342

lm_Wind_Brazil <- lm(w_Brazil$Quantity ~ w_Brazil$Year) # linear regression of year and quantity
lm_Wind_Brazil
summary(lm_Wind_Brazil) # summary of regression line, R^2 = 0.8407

lm_Wind_China <- lm(w_China$Quantity ~ w_China$Year) # linear regression of year and quantity
lm_Wind_China
summary(lm_Wind_China) # summary of regression line, R^2 = 0.9535

lm_Wind_Germany <- lm(w_Germany$Quantity ~ w_Germany$Year) # linear regression of year and quantity
lm_Wind_Germany
summary(lm_Wind_Germany) # summary of regression line, R^2 = 0.8639 

lm_Wind_India <- lm(w_India$Quantity ~ w_India$Year) # linear regression of year and quantity
lm_Wind_India
summary(lm_Wind_India) # summary of regression line, R^2 = 0.7672 

lm_Wind_Mexico <- lm(w_Mexico$Quantity ~ w_Mexico$Year) # linear regression of year and quantity
lm_Wind_Mexico
summary(lm_Wind_Mexico) # summary of regression line, R^2 = 0.5618 

lm_Wind_Russian_Fed <- lm(w_Russian_Fed$Quantity ~ w_Russian_Fed$Year) # linear regression of year and quantity
lm_Wind_Russian_Fed
summary(lm_Wind_Russian_Fed) # summary of regression line, R^2 = 0.5058 

lm_Wind_United_States <- lm(w_United_States$Quantity ~ w_United_States$Year) # linear regression of year and quantity
lm_Wind_United_States
summary(lm_Wind_United_States) # summary of regression line, R^2 = 0.7558


### clustering ###
# enable geocoding and static maps to create ggmap using Google API
# have to register using individual API key through google
# register_google(key = "[your key]")

Wind_Countryavg <- aggregate(x = Wind$Quantity, by = list(Wind$Country.or.Area), FUN = mean)
View(Wind_Countryavg)
Wind_Countryavg <- rename(Wind_Countryavg, Country = Group.1)
Wind_Countryavg <- rename(Wind_Countryavg, "Quantity Avg, kW-hr" = x)
View(Wind_Countryavg)
# can use this line if you have a billing account attached to your API
# locations <- mutate_geocode(Wind_Countryavg, Group.1) # creates latitude and longitude for each location
Wind_Countryavg$latitude <- c(28.849, -36.136, 40.5595, 12.508, -26.307,
                          47.474, 40.506, 26.078, 24.414, 53.542,
                          50.795, 27.504, -17.036, 12.1784, 44.376,
                          -9.9158, 18.435, 42.816, 16.6001, 60.466,
                          -27.543, 35.205, 3.974, 10.0724, 45.582, 
                          22.027, 12.1571, 35.017, 49.818, 56.051,
                          19.0616, -1.136, 26.245, 15.558, 58.789,
                          8.296, 62.145, -51.773, -17.892, 63.626,
                          46.293, -17.658, 13.527, 42.055, 50.841,
                          39.736, 16.215, 13.428, 15.5527, 4.556, 
                          14.714, 46.915, 64.853, 22.060, -1.265,
                          31.675, 53.297, 30.899, 43.542, 18.121,
                          35.983, 31.176, 47.890, 0.220, 36.815, 
                          42.561, 56.890, 55.459, 49.775, -0.346,
                          35.889, 14.674, 19.909, -20.255, 23.455,
                          6.874, 42.858, 39.610, -22.478, 28.127,
                          52.091, -21.448, -42.793, 12.774, 41.642,
                          61.443, 25.751, 29.515, 8.676, -6.073,
                          -9.9104, 11.642, 52.587, 40.083, 18.231,
                          -21.147, 47.361, -21.147, 45.659, 61.598,
                          -13.663, 24.255, 44.212, -4.655, 48.796,
                          46.022, -28.839, 40.088, 7.449, -15.9655,
                          17.314, 47.020, 63.768, 46.928, 16.158,
                          -21.1996, 34.455, 39.308, 49.377, 54.115, 
                          40.479, -32.896, 42.244, -15.461, 7.444,
                          14.24)
Wind_Countryavg$longitude <- c(2.4591, -65.123, 44.683, -69.967, 135.095,
                         14.2996, 48.060, 50.546, 90.154, 28.193,
                         4.532, 90.386, -64.6199, -68.2385, 17.781,
                         -52.573, -64.622, 25.310, -24.171, -108.224,
                         -70.178, 100.793, -73.11, -83.998, 15.976, 
                         -79.117, -68.932, 33.238, 15.165, 9.904,
                         -70.411, -78.201, 29.852, 38.818, 25.722,
                         40.306, -6.974, -59.160, 177.988, 26.533,
                         2.4731, -149.434, -15.38, 43.597, 10.405,
                         22.164, -61.642, 144.75, -90.304, -58.99,
                         -86.982, 19.305, -18.237, 78.395, 115.158,
                         54.334, -7.8811, 34.843, 12.154, -77.339,
                         138.729, 36.581, 66.979, 37.825, 127.972,
                         20.915, 25.6303, 23.927, 6.1332, 73.135,
                         14.446, -61.012, -10.461, 57.598, -102.43,
                         158.23, 19.228, -6.416, 17.2969, 84.083,
                         5.4755, 165.577, 172.313, -84.988, 21.712,
                         9.2286, 94.529, 68.985, -80.274, 144.078,
                         -75.413, 122.976, 19.108, -8.029, -66.455,
                         55.538, 28.487, 55.538, 24.776, 93.3004,
                        -172.427, 44.566, 20.755, 55.459, 19.445,
                        14.679, 24.78196, -3.085, 80.746, -5.7115,
                        -62.723, -56.302, 16.7001, 8.1687, 101.846,
                        -175.1958, 9.5931, 35.3537, 30.777, -2.0976,
                        -99.505, -55.897, 63.949, 167.012, -65.907,
                        108.44)
View(Wind_Countryavg)

library(cluster)
set.seed(100)
clusters <- kmeans(Wind_Countryavg[,2], 8)
Wind_Countryavg$Cluster <- as.factor(clusters$cluster)
head(Wind_Countryavg)
print(clusters) # prints data set

library(ggmap)
library(ggrepel)
WorldMap <- get_stamenmap(c(left = -135, bottom = -60, right = 180, top = 70), zoom = 4, maptype = c("terrain-background"), color = "bw")
ggmap(WorldMap) + geom_point(data = Wind_Countryavg, aes(x = longitude[], y = latitude[], colour = as.factor(Cluster)), size = 3) +
  ggtitle("Wind Electricity KMean Cluster by Quantity") +
  geom_text_repel(data = Wind_Countryavg, aes(x = longitude, y = latitude, label = Country), size = 3)

