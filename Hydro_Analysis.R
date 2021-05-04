# Hydro Energy Set

# download data set and export as csv file
# call Hydro electricity dataset as csv
Hydro <- read.csv("C:/users/caplam/Downloads/UNdata_Hydro/UNdata_Hydro.csv")
View(Hydro) # footnotes at bottom need to be filtered out
Hydro <- filter(Hydro, Commodity...Transaction == "Electricity - total hydro production")
View(Hydro)
colnames(Hydro) # column names
head(Hydro) # gives first 6 rows
summary(Hydro) 

# call necessary packages
library(dplyr) # dplyr package for filter function
library(ggplot2) # calls ggplot package


##### Australia #####
h_Australia <- filter(Hydro, Country.or.Area == "Australia") # creation of Australia data set
View(h_Australia)
fivenum(h_Australia$Quantity,na.rm = TRUE)
mean(h_Australia$Quantity) # Australia Quantity column mean
median((h_Australia$Quantity)) # Australia Quantity column median
y <- table(h_Australia$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Australia Quantity mode(s)

# EDA
barplot(table(h_Australia$Quantity)) # gives the frequency of each production
plot1 <- ggplot(h_Australia, aes(Year, Quantity))
print(plot1 + geom_point(size=3))


##### Brazil #####
h_Brazil <- filter(Hydro, Country.or.Area == "Brazil") # creation of Brazil data set
View(h_Brazil)
fivenum(h_Brazil$Quantity,na.rm = TRUE)
mean(h_Brazil$Quantity) # Brazil Quantity column mean
median((h_Brazil$Quantity)) # Brazil Quantity column median
y <- table(h_Brazil$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Brazil Quantity mode(s)

# EDA
barplot(table(h_Brazil$Quantity)) # gives the frequency of each production
plot2 <- ggplot(h_Brazil, aes(Year, Quantity))
print(plot2 + geom_point(size=3))


##### China #####
h_China <- filter(Hydro, Country.or.Area == "China") # creation of China data set
View(h_China)
fivenum(h_China$Quantity,na.rm = TRUE)
mean(h_China$Quantity) # China Quantity column mean
median((h_China$Quantity)) # China Quantity column median
y <- table(h_China$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # China Quantity mode(s)

# EDA
barplot(table(h_China$Quantity)) # gives the frequency of each production
plot3 <- ggplot(h_China, aes(Year, Quantity))
print(plot3 + geom_point(size=3))


##### Germany #####
h_Germany <- filter(Hydro, Country.or.Area == "Germany") # creation of Germany data set
View(h_Germany)
fivenum(h_Germany$Quantity,na.rm = TRUE)
mean(h_Germany$Quantity) # Germany Quantity column mean
median((h_Germany$Quantity)) # Germany Quantity column median
y <- table(h_Germany$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Germany Quantity mode(s)

# EDA
barplot(table(h_Germany$Quantity)) # gives the frequency of each production
plot4 <- ggplot(h_Germany, aes(Year, Quantity))
print(plot4 + geom_point(size=3))


##### India #####
h_India <- filter(Hydro, Country.or.Area == "India") # creation of India data set
View(h_India)
fivenum(h_India$Quantity,na.rm = TRUE)
mean(h_India$Quantity) # India Quantity column mean
median((h_India$Quantity)) # India Quantity column median
y <- table(h_India$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # India Quantity mode(s)

# EDA
barplot(table(h_India$Quantity)) # gives the frequency of each production
plot5 <- ggplot(h_India, aes(Year, Quantity))
print(plot5 + geom_point(size=3))


##### Mexico #####
h_Mexico <- filter(Hydro, Country.or.Area == "Mexico") # creation of Mexico data set
View(h_Mexico)
fivenum(h_Mexico$Quantity,na.rm = TRUE)
mean(h_Mexico$Quantity) # Mexico Quantity column mean
median((h_Mexico$Quantity)) # Mexico Quantity column median
y <- table(h_Mexico$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Mexico Quantity mode(s)

# EDA
barplot(table(h_Mexico$Quantity)) # gives the frequency of each production
plot6 <- ggplot(h_Mexico, aes(Year, Quantity))
print(plot6 + geom_point(size=3))


##### Russian Federation #####
h_Russian_Fed <- filter(Hydro, Country.or.Area == "Russian Federation") # creation of Russian Federation data set
View(h_Russian_Fed)
fivenum(h_Russian_Fed$Quantity,na.rm = TRUE)
mean(h_Russian_Fed$Quantity) # Russian Federation Quantity column mean
median((h_Russian_Fed$Quantity)) # Russian Federation Quantity column median
y <- table(h_Russian_Fed$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Russian Federation Quantity mode(s)

# EDA
barplot(table(h_Russian_Fed$Quantity)) # gives the frequency of each production
plot7 <- ggplot(h_Russian_Fed, aes(Year, Quantity))
print(plot7 + geom_point(size=3))


##### United States #####
h_United_States <- filter(Hydro, Country.or.Area == "United States") # creation of United States data set
View(h_United_States)
fivenum(h_United_States$Quantity,na.rm = TRUE)
mean(h_United_States$Quantity) # United States Quantity column mean
median((h_United_States$Quantity)) # United States Quantity column median
y <- table(h_United_States$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # United States Quantity mode(s)

# EDA
barplot(table(h_United_States$Quantity)) # gives the frequency of each production
plot8 <- ggplot(h_United_States, aes(Year, Quantity))
print(plot8 + geom_point(size=3))


######### Yearly analysis ##########
### 2019 ###
h_y2019 <- filter(Hydro, Year == "2019") # creation of 2019 data set
View(h_y2019)
fivenum(h_y2019$Quantity,na.rm = TRUE)
mean(h_y2019$Quantity) # 2019 Quantity column mean
median((h_y2019$Quantity)) # 2019 Quantity column median
y <- table(h_y2019$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # 2019 Quantity mode(s)

# EDA
barplot(table(h_y2019$Quantity)) # gives the frequency of each production
plot9 <- ggplot(h_y2019, aes(Country.or.Area, Quantity))
print(plot9 + geom_point(size=3))


### 2018 ###
h_y2018 <- filter(Hydro, Year == "2018") # creation of 2018 data set
View(h_y2018)
fivenum(h_y2018$Quantity,na.rm = TRUE)
mean(h_y2018$Quantity) # 2018 Quantity column mean
median((h_y2018$Quantity)) # 2018 Quantity column median
y <- table(h_y2018$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # 2018 Quantity mode(s)

# EDA
barplot(table(h_y2018$Quantity)) # gives the frequency of each production
plot10 <- ggplot(h_y2018, aes(Country.or.Area, Quantity))
print(plot10 + geom_point(size=3))


### 2017 ###
h_y2017 <- filter(Hydro, Year == "2017") # creation of 2017 data set
View(h_y2017)
fivenum(h_y2017$Quantity,na.rm = TRUE)
mean(h_y2017$Quantity) # 2017 Quantity column mean
median((h_y2017$Quantity)) # 2017 Quantity column median
y <- table(h_y2017$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # 2017 Quantity mode(s)

# EDA
barplot(table(h_y2017$Quantity)) # gives the frequency of each production
plot11 <- ggplot(h_y2017, aes(Country.or.Area, Quantity))
print(plot11 + geom_point(size=3))


### 2016 ###
h_y2016 <- filter(Hydro, Year == "2016") # creation of 2016 data set
View(h_y2016)
fivenum(h_y2016$Quantity,na.rm = TRUE)
mean(h_y2016$Quantity) # 2016 Quantity column mean
median((h_y2016$Quantity)) # 2016 Quantity column median
y <- table(h_y2016$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # 2016 Quantity mode(s)

# EDA
barplot(table(h_y2016$Quantity)) # gives the frequency of each production
plot11 <- ggplot(h_y2016, aes(Country.or.Area, Quantity))
print(plot11 + geom_point(size=3))



######### Overall Analysis of yearly Hydro data #########
Countries <- filter(Hydro, Country.or.Area %in% c("Australia", "Brazil",
                                                    "China", "Germany", "India", "Mexico", "Russian Federation",
                                                    "United States")) # creation of specific countries data set

World_Hydro <- ggplot(Hydro, aes(Country.or.Area, Quantity, color = Year))
print(World_Hydro + geom_point(size=3) +
        theme(axis.text.x=element_text(angle=90, size=8, vjust=0.5)))

World_Hydro_yr <- ggplot(Hydro, aes(Country.or.Area, Year, color = Quantity))
print(World_Hydro_yr + geom_point(size=3) +
        theme(axis.text.x=element_text(angle=90, size=8, vjust=0.5)))

Countries_Hydro <- ggplot(Countries, aes(Country.or.Area, Quantity, color = Year))
print(Countries_Hydro + geom_point(size=3) +
        theme(axis.text.x=element_text(angle=60, size=8, vjust=0.5)))

Countries_Hydro_yr <- ggplot(Countries, aes(Country.or.Area, Year, color = Quantity))
print(Countries_Hydro_yr + geom_point(size=3) +
        theme(axis.text.x=element_text(angle=60, size=8, vjust=0.5)))


### classifications ###
# breaking down quantity column
# takes quantities and organizes them into ranges
summary(Hydro$Quantity)
categ_all <- as.numeric(Hydro$Quantity)
categ_all <- cut(categ_all, br = c(0,100,5000,50000,200000,1250000), labels = c("Lowest Production",'Semi-Low Production','Middle Production','Semi-High Production','Highest Production'))
categ_all <- as.factor(categ_all)
summary(categ_all)

World_Hydro_cat <- ggplot(Hydro, aes(Country.or.Area, Quantity, color = categ_all))
print(World_Hydro_cat + geom_point(size=3) +
      theme(axis.text.x=element_text(angle=90, size=8, vjust=0.5)))

World_Hydro_yr_cat <- ggplot(Hydro, aes(Country.or.Area, Year, color = categ_all))
print(World_Hydro_yr_cat + geom_point(size=3))

summary(Countries$Quantity)
categ_countries <- as.numeric(Countries$Quantity)
categ_countries <- cut(categ_countries, br = c(0,100,5000,50000,200000,1250000), labels = c("Lowest Production",'Semi-Low Production','Middle Production','Semi-High Production','Highest Production'))
categ_countries <- as.factor(categ_countries)
summary(categ_countries)

Countries_Hydro_yr_cat <- ggplot(Countries, aes(Country.or.Area, Quantity, color = categ_countries))
print(Countries_Hydro_yr_cat + geom_point(size=3))


### linear regression ###
Hydro_year <- Hydro$Year
Hydro_prod <- Hydro$Quantity
Hydro_countries <- Hydro$Country.or.Area

lmHydro_prod <- lm(Hydro_prod ~ Hydro_year) # linear regression of year and quantity
lmHydro_prod
summary(lmHydro_prod) # summary of regression line, R^2 = 0.001945

lm_Hydro <- lm(Hydro_prod ~ Hydro_year + Hydro_countries) # linear regression of year and quantity and country
lm_Hydro
summary(lm_Hydro) # summary of regression line, R^2 = 0.8045 

lm_Hydro_yr <- lm(Hydro_year ~ Hydro_prod + Hydro_countries) # linear regression of year and quantity and country
lm_Hydro_yr
summary(lm_Hydro_yr) # summary of regression line, R^2 = 0.02652 


lm_Hydro_Australia <- lm(h_Australia$Quantity ~ h_Australia$Year) # linear regression of year and quantity
lm_Hydro_Australia
summary(lm_Hydro_Australia) # summary of regression line, R^2 = -0.00325

lm_Hydro_Brazil <- lm(h_Brazil$Quantity ~ h_Brazil$Year) # linear regression of year and quantity
lm_Hydro_Brazil
summary(lm_Hydro_Brazil) # summary of regression line, R^2 = 0.8526 

lm_Hydro_China <- lm(h_China$Quantity ~ h_China$Year) # linear regression of year and quantity
lm_Hydro_China
summary(lm_Hydro_China) # summary of regression line, R^2 = 0.8866 

lm_Hydro_Germany <- lm(h_Germany$Quantity ~ h_Germany$Year) # linear regression of year and quantity
lm_Hydro_Germany
summary(lm_Hydro_Germany) # summary of regression line, R^2 = 0.2423 

lm_Hydro_India <- lm(h_India$Quantity ~ h_India$Year) # linear regression of year and quantity
lm_Hydro_India
summary(lm_Hydro_India) # summary of regression line, R^2 = 0.7945 

lm_Hydro_Mexico <- lm(h_Mexico$Quantity ~ h_Mexico$Year) # linear regression of year and quantity
lm_Hydro_Mexico
summary(lm_Hydro_Mexico) # summary of regression line, R^2 = 0.3213 

lm_Hydro_Russian_Fed <- lm(h_Russian_Fed$Quantity ~ h_Russian_Fed$Year) # linear regression of year and quantity
lm_Hydro_Russian_Fed
summary(lm_Hydro_Russian_Fed) # summary of regression line, R^2 =  0.2501 

lm_Hydro_United_States <- lm(h_United_States$Quantity ~ h_United_States$Year) # linear regression of year and quantity
lm_Hydro_United_States
summary(lm_Hydro_United_States) # summary of regression line, R^2 = -0.0266 


### clustering ###
# enable geocoding and static maps to create ggmap using Google API
# have to register using individual API key through google
# register_google(key = "[your key]")

Hydro_Countryavg <- aggregate(x = Hydro$Quantity, by = list(Hydro$Country.or.Area), FUN = mean)
View(Hydro_Countryavg)
Hydro_Countryavg <- rename(Hydro_Countryavg, Country = Group.1)
Hydro_Countryavg <- rename(Hydro_Countryavg, "Quantity Avg, kW-hr" = x)
View(Hydro_Countryavg)
# can use this line if you have a billing account attached to your API
# locations <- mutate_geocode(Hydro_Countryavg, Group.1) # creates latitude and longitude for each location
Hydro_Countryavg$latitude <- c(33.523, 40.957, 28.849, 42.545, -12.766, 
                               -36.136, 40.5595, -26.307, 47.474, 40.506,
                               24.414, 53.542, 50.795, 17.078, 9.842,
                               27.504, -17.036, 44.376, -9.9158, 42.816, 
                               12.560, -3.411, 7.494, 12.887, 5.294, 
                               60.466, 6.538, -27.543, 35.205, 3.974,
                               -2.2137, 10.0724, 45.582, 22.027, 49.818, 
                               48.971, -2.952, 56.051, 15.461, 19.0616,
                               -1.136, 26.245, 13.799, 1.634, 58.789, 
                               -26.469, 15.558, 8.296, 62.145, -17.892, 
                               63.626, 46.293, 4.0947, -17.658, -0.4177, 
                               42.055, 52.942, 50.841, 50.409, 8.149, 
                               39.736, 69.634, 16.215, 15.5527, 10.639, 
                               4.556, 19.167, 14.714, 46.915, 64.853, 
                               22.060, -1.265, 31.675, 33.434, 53.297,
                               54.244, 30.899, 43.542, 18.121, 35.983, 
                               31.176, 47.890, 0.220, 40.285, 36.815, 
                               42.561, 41.778, 19.364, 56.890, 34.033, 
                               -29.475, 6.537, 47.1499, 55.459, 49.775, 
                               -20.224, -13.209, 4.1279, 18.498, 14.674, 
                               19.909, -20.255, 23.455, 6.874, 42.858, 
                               39.610, -17.753, 21.816, -22.478, 28.127, 
                               52.091, -21.448, -42.793, 12.774, 9.3102, 
                               41.642, 61.443, 25.751, 9.661, 29.515, 
                               8.676, -6.073, -22.926, -9.9104, 11.642, 
                               52.587, 40.083, 18.231, -21.147, 47.361,
                               -21.147, 45.659, 61.598, -1.9103, -13.663, 
                               0.278, 44.212, 42.824, 8.475, 48.796, 
                               46.022, -9.452, -28.839, 40.088, 7.449, 
                               13.2465, 15.623, 9.132, 3.9695, 63.768, 
                               46.928, 34.884, 38.785, 16.158, -8.815,
                               7.222, 34.455, 39.308, 39.389, 1.247,
                               49.377, 54.115, -6.179, 40.479, -32.896, 
                               46.6500, 42.244, -15.461, 7.444, 14.24,
                               -14.288, 45.011, -13.892, -18.595)
Hydro_Countryavg$longitude <- c(65.407, 20.032, 2.4591, 1.586, 17.464, 
                                -65.123, 44.683, 135.095, 14.2996, 48.060, 
                                90.154, 28.193, 4.532, -88.714, 2.3496, 
                                90.386, -64.6199, 17.781, -52.573, 25.310, 
                                -1.745, 29.897, -5.573, 104.867, 12.452,
                                -108.224, 19.967, -70.178, 100.793, -73.11, 
                                23.5836, -83.998, 15.976, -79.117, 15.165, 
                                18.158, 23.339, 9.904, -61.349, -70.411, 
                                -78.201, 29.852, -89.040, 10.481, 25.722, 
                                31.495, 38.818, 40.306, -6.974, 177.988,
                                26.533, 2.4731, -53.111, -149.434, 11.767, 
                                43.597, 13.459, 10.405, 7.0433, -1.111, 
                                22.164, -41.882, -61.642, -90.304, -10.728, 
                                -58.99, -72.292, -86.982, 19.305, -18.237, 
                                78.395, 115.158, 54.334, 43.812, -7.8811,
                                -4.516, 34.843, 12.154, -77.339, 138.729, 
                                36.581, 66.979, 37.825, 127.171, 127.972,
                                20.915, 74.797, 102.788, 25.6303, 35.841, 
                                28.288, -9.478, 9.554, 23.927, 6.1332, 
                                46.398, 34.147, 102.262, -2.131, -61.012, 
                                -10.461, 57.598, -102.43, 158.23, 19.228, 
                                -6.416, 35.687, 96.438, 17.2969, 84.083, 
                                5.4755, 165.577, 172.313, -84.988, 7.957, 
                                21.712, 9.2286, 94.529, 9.661, 68.985, 
                                -80.274, 144.078, -58.507, -75.413, 122.976,
                                19.108, -8.029, -66.455, 55.538, 28.487, 
                                55.538, 24.776, 93.3004, 30.022, -172.427, 
                                6.608, 20.755, 19.259, -11.871, 19.445, 
                                14.679, 159.874, 24.78196, -3.085, 80.746,
                                -61.192, 30.282, 29.597, -55.901, 16.7001, 
                                8.1687, 38.395, 71.059, 101.846, 125.761, 
                                1.169, 9.5931, 35.3537, 58.887, 32.3104, 
                                30.777, -2.0976, 34.951, -99.505, -55.897,
                                58.003, 63.949, 167.012, -65.907, 108.44, 
                                -178.12, 19.071, 25.838, 29.464)
View(Hydro_Countryavg)

library(cluster)
set.seed(100)
clusters <- kmeans(Hydro_Countryavg[,2], 8)
Hydro_Countryavg$Cluster <- as.factor(clusters$cluster)
head(Hydro_Countryavg)
print(clusters) # prints data set

library(ggmap)
library(ggrepel)
WorldMap <- get_stamenmap(c(left = -135, bottom = -60, right = 180, top = 70), zoom = 4, maptype = c("terrain-background"), color = "bw")
ggmap(WorldMap) + geom_point(data = Hydro_Countryavg, aes(x = longitude[], y = latitude[], colour = as.factor(Cluster)), size = 3) +
  ggtitle("Hydro Electricity KMean Cluster by Quantity") +
  geom_text_repel(data = Hydro_Countryavg, aes(x = longitude, y = latitude, label = Country), size = 3)
