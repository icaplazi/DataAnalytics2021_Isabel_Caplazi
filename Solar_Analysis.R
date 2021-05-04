# Solar Energy Set

# download data set and export as csv file
# call wind electricity dataset as csv
Solar <- read.csv("C:/users/caplam/Downloads/UNdata_SolarElec/UNdata_SolarElec.csv")
View(Solar) 
Solar <- filter(Solar, Commodity...Transaction == "Electricity - total solar production")
View(Solar)
colnames(Solar) # column names 
head(Solar) # gives first 6 rows
summary(Solar) 

# call necessary packages
library(dplyr) # dplyr package for filter function
library(ggplot2) # calls ggplot package


##### Australia #####
s_Australia <- filter(Solar, Country.or.Area == "Australia") # creation of Australia data set
View(s_Australia)
fivenum(s_Australia$Quantity,na.rm = TRUE)
mean(s_Australia$Quantity) # Australia Quantity column mean
median((s_Australia$Quantity)) # Australia Quantity column median
y <- table(s_Australia$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Australia Quantity mode(s)

# EDA
barplot(table(s_Australia$Quantity)) # gives the frequency of each production
plot1 <- ggplot(s_Australia, aes(Year, Quantity))
print(plot1 + geom_point(size=3))


##### Brazil #####
s_Brazil <- filter(Solar, Country.or.Area == "Brazil") # creation of Brazil data set
View(s_Brazil)
fivenum(s_Brazil$Quantity,na.rm = TRUE)
mean(s_Brazil$Quantity) # Brazil Quantity column mean
median((s_Brazil$Quantity)) # Brazil Quantity column median
y <- table(s_Brazil$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Brazil Quantity mode(s)

# EDA
barplot(table(s_Brazil$Quantity)) # gives the frequency of each production
plot2 <- ggplot(s_Brazil, aes(Year, Quantity))
print(plot2 + geom_point(size=3))


##### China #####
s_China <- filter(Solar, Country.or.Area == "China") # creation of China data set
View(s_China)
fivenum(s_China$Quantity,na.rm = TRUE)
mean(s_China$Quantity) # China Quantity column mean
median((s_China$Quantity)) # China Quantity column median
y <- table(s_China$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # China Quantity mode(s)

# EDA
barplot(table(s_China$Quantity)) # gives the frequency of each production
plot3 <- ggplot(s_China, aes(Year, Quantity))
print(plot3 + geom_point(size=3))


##### Germany #####
s_Germany <- filter(Solar, Country.or.Area == "Germany") # creation of Germany data set
View(s_Germany)
fivenum(s_Germany$Quantity,na.rm = TRUE)
mean(s_Germany$Quantity) # Germany Quantity column mean
median((s_Germany$Quantity)) # Germany Quantity column median
y <- table(s_Germany$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Germany Quantity mode(s)

# EDA
barplot(table(s_Germany$Quantity)) # gives the frequency of each production
plot4 <- ggplot(s_Germany, aes(Year, Quantity))
print(plot4 + geom_point(size=3))


##### India #####
s_India <- filter(Solar, Country.or.Area == "India") # creation of India data set
View(s_India)
fivenum(s_India$Quantity,na.rm = TRUE)
mean(s_India$Quantity) # India Quantity column mean
median((s_India$Quantity)) # India Quantity column median
y <- table(s_India$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # India Quantity mode(s)

# EDA
barplot(table(s_India$Quantity)) # gives the frequency of each production
plot5 <- ggplot(s_India, aes(Year, Quantity))
print(plot5 + geom_point(size=3))


##### Mexico #####
s_Mexico <- filter(Solar, Country.or.Area == "Mexico") # creation of Mexico data set
View(s_Mexico)
fivenum(s_Mexico$Quantity,na.rm = TRUE)
mean(s_Mexico$Quantity) # Mexico Quantity column mean
median((s_Mexico$Quantity)) # Mexico Quantity column median
y <- table(s_Mexico$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Mexico Quantity mode(s)

# EDA
barplot(table(s_Mexico$Quantity)) # gives the frequency of each production
plot6 <- ggplot(s_Mexico, aes(Year, Quantity))
print(plot6 + geom_point(size=3))


##### Russian Federation #####
s_Russian_Fed <- filter(Solar, Country.or.Area == "Russian Federation") # creation of Russian Federation data set
View(s_Russian_Fed)
fivenum(s_Russian_Fed$Quantity,na.rm = TRUE)
mean(s_Russian_Fed$Quantity) # Russian Federation Quantity column mean
median((s_Russian_Fed$Quantity)) # Russian Federation Quantity column median
y <- table(s_Russian_Fed$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Russian Federation Quantity mode(s)

# EDA
barplot(table(s_Russian_Fed$Quantity)) # gives the frequency of each production
plot7 <- ggplot(s_Russian_Fed, aes(Year, Quantity))
print(plot7 + geom_point(size=3))


##### United States #####
s_United_States <- filter(Solar, Country.or.Area == "United States") # creation of United States data set
View(s_United_States)
fivenum(s_United_States$Quantity,na.rm = TRUE)
mean(s_United_States$Quantity) # United States Quantity column mean
median((s_United_States$Quantity)) # United States Quantity column median
y <- table(s_United_States$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # United States Quantity mode(s)

# EDA
barplot(table(s_United_States$Quantity)) # gives the frequency of each production
plot8 <- ggplot(s_United_States, aes(Year, Quantity))
print(plot8 + geom_point(size=3))

######### Yearly analysis ##########
### 2019 ###
s_y2019 <- filter(Solar, Year == "2019") # creation of 2019 data set
View(s_y2019)
fivenum(s_y2019$Quantity,na.rm = TRUE)
mean(s_y2019$Quantity) # 2019 Quantity column mean
median((s_y2019$Quantity)) # 2019 Quantity column median
y <- table(s_y2019$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # 2019 Quantity mode(s)

# EDA
barplot(table(s_y2019$Quantity)) # gives the frequency of each production
plot9 <- ggplot(s_y2019, aes(Country.or.Area, Quantity))
print(plot9 + geom_point(size=3))


### 2018 ###
s_y2018 <- filter(Solar, Year == "2018") # creation of 2018 data set
View(s_y2018)
fivenum(s_y2018$Quantity,na.rm = TRUE)
mean(s_y2018$Quantity) # 2018 Quantity column mean
median((s_y2018$Quantity)) # 2018 Quantity column median
y <- table(s_y2018$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # 2018 Quantity mode(s)

# EDA
barplot(table(s_y2018$Quantity)) # gives the frequency of each production
plot10 <- ggplot(s_y2018, aes(Country.or.Area, Quantity))
print(plot10 + geom_point(size=3))


### 2017 ###
s_y2017 <- filter(Solar, Year == "2017") # creation of 2017 data set
View(s_y2017)
fivenum(s_y2017$Quantity,na.rm = TRUE)
mean(s_y2017$Quantity) # 2017 Quantity column mean
median((s_y2017$Quantity)) # 2017 Quantity column median
y <- table(s_y2017$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # 2017 Quantity mode(s)

# EDA
barplot(table(s_y2017$Quantity)) # gives the frequency of each production
plot11 <- ggplot(s_y2017, aes(Country.or.Area, Quantity))
print(plot11 + geom_point(size=3))


### 2016 ###
s_y2016 <- filter(Solar, Year == "2016") # creation of 2016 data set
View(s_y2016)
fivenum(s_y2016$Quantity,na.rm = TRUE)
mean(s_y2016$Quantity) # 2016 Quantity column mean
median((s_y2016$Quantity)) # 2016 Quantity column median
y <- table(s_y2016$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # 2016 Quantity mode(s)

# EDA
barplot(table(s_y2016$Quantity)) # gives the frequency of each production
plot11 <- ggplot(s_y2016, aes(Country.or.Area, Quantity))
print(plot11 + geom_point(size=3))



######### Overall Analysis of yearly wind data #########
Countries <- filter(Solar, Country.or.Area %in% c("Australia", "Brazil",
                                                 "China", "Germany", "India", "Mexico", "Russian Federation",
                                                 "United States")) # creation of specific countries data set

World_Solar <- ggplot(Solar, aes(Country.or.Area, Quantity, color = Year))
print(World_Solar + geom_point(size=3))

World_Solar_yr <- ggplot(Solar, aes(Country.or.Area, Year, color = Quantity))
print(World_Solar_yr + geom_point(size=3))

Countries_Solar <- ggplot(Countries, aes(Country.or.Area, Quantity, color = Year))
print(Countries_Solar + geom_point(size=3))

Countries_Solar_yr <- ggplot(Countries, aes(Country.or.Area, Year, color = Quantity))
print(Countries_Solar_yr + geom_point(size=3))


### classifications ###
# breaking down quantity column
# takes quantities and organizes them into ranges
summary(Solar$Quantity)
categ_all <- as.numeric(Solar$Quantity)
categ_all <- cut(categ_all, br = c(0,100,5000,50000,150000,370000), labels = c("Lowest Production",'Semi-Low Production','Middle Production','Semi-High Production','Highest Production'))
categ_all <- as.factor(categ_all)
summary(categ_all)

World_Solar_cat <- ggplot(Solar, aes(Country.or.Area, Quantity, color = categ_all))
print(World_Solar_cat + geom_point(size=3) +
  theme(axis.text.x=element_text(angle=90, size=8, vjust=0.5)))

World_Solar_yr_cat <- ggplot(Solar, aes(Country.or.Area, Year, color = categ_all))
print(World_Solar_yr_cat + geom_point(size=3))

summary(Countries$Quantity)
categ_countries <- as.numeric(Countries$Quantity)
categ_countries <- cut(categ_countries, br = c(0,100,5000,50000,150000,370000), labels = c("Lowest Production",'Semi-Low Production','Middle Production','Semi-High Production','Highest Production'))
categ_countries <- as.factor(categ_countries)
summary(categ_countries)

Countries_Solar_yr_cat <- ggplot(Countries, aes(Country.or.Area, Quantity, color = categ_countries))
print(Countries_Solar_yr_cat + geom_point(size=3))


### linear regression ###
Solar_year <- Solar$Year
Solar_prod <- Solar$Quantity
Solar_countries <- Solar$Country.or.Area

lmSolar_prod <- lm(Solar_prod ~ Solar_year) # linear regression of year and quantity
lmSolar_prod
summary(lmSolar_prod) # summary of regression line, R^2 = 0.01151

lm_Solar <- lm(Solar_prod ~ Solar_year + Solar_countries) # linear regression of year and quantity and country
lm_Solar
summary(lm_Solar) # summary of regression line, R^2 = 0.4068

lm_Solar_yr <- lm(Solar_year ~ Solar_prod + Solar_countries) # linear regression of year and quantity and country
lm_Solar_yr
summary(lm_Solar_yr) # summary of regression line, R^2 = 0.4123


# lin reg by country
lm_Solar_Australia <- lm(s_Australia$Quantity ~ s_Australia$Year) # linear regression of year and quantity
lm_Solar_Australia
summary(lm_Solar_Australia) # summary of regression line, R^2 = 0.5793

lm_Solar_Brazil <- lm(s_Brazil$Quantity ~ s_Brazil$Year) # linear regression of year and quantity
lm_Solar_Brazil
summary(lm_Solar_Brazil) # summary of regression line, R^2 = 0.6307

lm_Solar_China <- lm(s_China$Quantity ~ s_China$Year) # linear regression of year and quantity
lm_Solar_China
summary(lm_Solar_China) # summary of regression line, R^2 = 0.8979

lm_Solar_Germany <- lm(s_Germany$Quantity ~ s_Germany$Year) # linear regression of year and quantity
lm_Solar_Germany
summary(lm_Solar_Germany) # summary of regression line, R^2 = 0.7142

lm_Solar_India <- lm(s_India$Quantity ~ s_India$Year) # linear regression of year and quantity
lm_Solar_India
summary(lm_Solar_India) # summary of regression line, R^2 = 0.8229 

lm_Solar_Mexico <- lm(s_Mexico$Quantity ~ s_Mexico$Year) # linear regression of year and quantity
lm_Solar_Mexico
summary(lm_Solar_Mexico) # summary of regression line, R^2 = 0.2851 

lm_Solar_Russian_Fed <- lm(s_Russian_Fed$Quantity ~ s_Russian_Fed$Year) # linear regression of year and quantity
lm_Solar_Russian_Fed
summary(lm_Solar_Russian_Fed) # summary of regression line, R^2 =  0.9879

lm_Solar_United_States <- lm(s_United_States$Quantity ~ s_United_States$Year) # linear regression of year and quantity
lm_Solar_United_States
summary(lm_Solar_United_States) # summary of regression line, R^2 = 0.453 


### clustering ###
# enable geocoding and static maps to create ggmap using Google API
# have to register using individual API key through google
# register_google(key = "[your key]")

Solar_Countryavg <- aggregate(x = Solar$Quantity, by = list(Solar$Country.or.Area), FUN = mean)
View(Solar_Countryavg)
Solar_Countryavg <- rename(Solar_Countryavg, Country = Group.1)
Solar_Countryavg <- rename(Solar_Countryavg, "Quantity Avg, kW-hr" = x)
View(Solar_Countryavg)
# can use this line if you have a billing account attached to your API
# locations <- mutate_geocode(Solar_Countryavg, Group.1) # creates latitude and longitude for each location
Solar_Countryavg$latitude <- c(33.523, 28.849, -14.302, 42.545, -12.766, 
                               18.223, 17.088, -36.136, 40.5595, -26.307, 47.474, 40.506,
                               26.076, 24.414, 13.176, 53.542, 50.795, 9.842, #15
                               27.504, -17.036, 12.1784, 44.376, -28.585, -9.9158, 18.435, 4.468, 42.816, 
                               12.560, -3.411, 16.6001, 12.887, 5.294, 
                               60.466, 6.538, -27.543, 35.205, 3.974, #30
                               -2.2137, -21.237, 10.0724, 45.582, 22.027, 12.1571, 35.0170, 49.818, 
                               -2.952, 56.051, 19.0616, #40
                               -1.136, 26.245, 13.799, 8.296, 58.789, 
                               -26.469, 15.558, -17.892, 
                               63.626, 46.293, 4.0947, -17.658, -0.4177, #55
                               13.527, 50.841, 8.149, 
                               39.736, 12.108, 16.215, 13.428, 15.5527, 12.154, 
                               4.556, 19.167, 14.714, 46.915, #70
                               22.060, -1.265, 31.675, 33.434,
                               30.899, 43.542, 18.121, 35.983, 
                               31.176, 47.890, 1.852, 40.285, 36.815, #85
                               42.561, 29.348, 19.364, 56.890, 34.033, 
                               6.537, 27.8397, 47.1499, 55.459, 49.775, 
                               -20.224, 4.1279, -0.346, 18.498, 35.889, 7.417, 14.674, #100
                               19.909, -20.255, -12.801, 23.455, 6.874, 42.858, 
                               39.610, -17.753, 21.816, -22.478, -0.5295, 28.127, 
                               52.091, -21.448, -42.793, 12.774,  17.881, 9.3102, -19.057, #115
                               41.642, 61.443, 27.0714, 25.751, 29.515, 7.476,
                               8.676, -6.073, -9.9104, 11.642, 
                               52.587, 40.083, 18.231, -21.147, 47.361,
                               -21.147, 45.659, 61.598, -1.9103, -13.663, #135
                               0.278, 24.255, 15.212, 44.212, -4.655, 8.475, 1.355, 48.796, 
                               46.022, -9.452, -28.839, 6.771, 40.088, 7.449, 
                               -15.9655, 17.314, 13.872, 13.2465, 31.795, 3.9695, 63.768, 
                               46.928, 16.158, -8.815, #155
                               7.222, -21.191, 10.432, 34.455, 39.308, 21.797, -8.618, 1.247,
                               49.377, 23.554, 54.115, -6.179, 40.479, 17.739, -32.896, 
                               42.244, -15.461, 7.444, 14.24,
                               -14.288, 15.608, -13.892)
Solar_Countryavg$longitude <- c(65.407, 2.4591, -170.72, 1.586, 17.464, 
                                -63.041, -61.796, -65.123, 44.683, 135.095, 14.2996, 48.060, 
                                50.543, 90.154, -59.555, 28.193, 4.532, 2.3496, #15
                                90.386, -64.6199, -68.2385, 17.781, 23.706, -52.573, -64.622, 114.596, 25.310, 
                                -1.745, 29.897, -24.171, 104.867, 12.452,
                                -108.224, 19.967, -70.178, 100.793, -73.11, #30
                                23.5836, -159.777, -83.998, 15.976, -79.117, -68.932, 33.238, 15.165, 
                                23.339, 9.904, -70.411, #40
                                -78.201, 29.852, -89.040, 40.306, 25.722, 
                                31.495, 38.818, 177.988,
                                26.533, 2.4731, -53.111, -149.434, 11.767, #55
                                -15.380, 10.405, -1.111, 
                                22.164, -61.682, -61.642, 144.750, -90.304, -14.894, 
                                -58.99, -72.292, -86.982, 19.305, #70 
                                78.395, 115.158, 54.334, 43.812, 
                                34.843, 12.154, -77.339, 138.729, 
                                36.581, 66.979, -157.37, 127.171, 127.972, #85
                                20.915, 47.392, 102.788, 25.6303, 35.841, 
                                -9.478, 17.266, 9.554, 23.927, 6.1332, 
                                46.398, 102.262, 73.135, -2.131, 14.446, 168.737, -61.012, #100
                                -10.461, 57.598, 45.453, -102.43, 158.23, 19.228, 
                                -6.416, 35.687, 96.438, 17.2969, 166.933, 84.083, 
                                5.4755, 165.577, 172.313, -84.988, 9.910, 7.957, -169.859, #115
                                21.712, 9.2286, 55.895, 94.529, 68.985, 134.551,
                                -80.274, 144.078, -75.413, 122.976,
                                19.108, -8.029, -66.455, 55.538, 28.487, 
                                55.538, 24.776, 93.3004, 30.022, -172.427, #135
                                6.608, 44.566, -14.696, 20.755, 55.459, -11.871, 103.814, 19.445, 
                                14.679, 159.874, 24.78196, 30.227, -3.085, 80.746,
                                -5.7115, -62.723, -60.976, -61.192, 35.1898, -55.901, 16.7001, 
                                8.1687, 101.846, 125.761, #155
                                1.169, -175.199, -61.261, 9.5931, 35.3537, -71.851, 179.084, 32.3104, 
                                30.777, 54.106, -2.0976, 34.951, -99.505, -64.767, -55.897,
                                63.949, 167.012, -65.907, 108.44, 
                                -178.12, 47.475, 25.838)
View(Solar_Countryavg)

library(cluster)
set.seed(100)
clusters <- kmeans(Solar_Countryavg[,2], 8)
Solar_Countryavg$Cluster <- as.factor(clusters$cluster)
head(Solar_Countryavg)
print(clusters) # prints data set

library(ggmap)
library(ggrepel)
WorldMap <- get_stamenmap(c(left = -135, bottom = -60, right = 180, top = 70), zoom = 4, maptype = c("terrain-background"), color = "bw")
ggmap(WorldMap) + geom_point(data = Solar_Countryavg, aes(x = longitude[], y = latitude[], colour = as.factor(Cluster)), size = 3) +
  ggtitle("Solar Electricity KMean Cluster by Quantity") +
  geom_text_repel(data = Solar_Countryavg, aes(x = longitude, y = latitude, label = Country), size = 3)

