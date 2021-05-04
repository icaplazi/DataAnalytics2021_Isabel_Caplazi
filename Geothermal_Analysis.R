# Geothermal Energy Set

# download data set and export as csv file
# call Geothermal electricity dataset as csv
Geo <- read.csv("C:/users/caplam/Downloads/UNdata_Geothermal/UNdata_Geothermal.csv")
View(Geo) # footnotes at bottom need to be filtered out
Geo <- filter(Geo, Commodity...Transaction == "Electricity - total geothermal production")
View(Geo)
colnames(Geo) # column names
head(Geo) # gives first 6 rows
summary(Geo) 

# call necessary packages
library(dplyr) # dplyr package for filter function
library(ggplot2) # calls ggplot package


##### Australia #####
g_Australia <- filter(Geo, Country.or.Area == "Australia") # creation of Australia data set
View(g_Australia)
fivenum(g_Australia$Quantity,na.rm = TRUE)
mean(g_Australia$Quantity) # Australia Quantity column mean
median((g_Australia$Quantity)) # Australia Quantity column median
y <- table(g_Australia$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Australia Quantity mode(s)

# EDA
barplot(table(g_Australia$Quantity)) # gives the frequency of each production
plot1 <- ggplot(g_Australia, aes(Year, Quantity))
print(plot1 + geom_point(size=3))


##### Brazil #####
g_Brazil <- filter(Geo, Country.or.Area == "Brazil") # creation of Brazil data set
View(g_Brazil)
fivenum(g_Brazil$Quantity,na.rm = TRUE)
mean(g_Brazil$Quantity) # Brazil Quantity column mean
median((g_Brazil$Quantity)) # Brazil Quantity column median
y <- table(g_Brazil$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Brazil Quantity mode(s)

# EDA
barplot(table(g_Brazil$Quantity)) # gives the frequency of each production
plot2 <- ggplot(g_Brazil, aes(Year, Quantity))
print(plot2 + geom_point(size=3))


##### China #####
g_China <- filter(Geo, Country.or.Area == "China") # creation of China data set
View(g_China)
fivenum(g_China$Quantity,na.rm = TRUE)
mean(g_China$Quantity) # China Quantity column mean
median((g_China$Quantity)) # China Quantity column median
y <- table(g_China$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # China Quantity mode(s)

# EDA
barplot(table(g_China$Quantity)) # gives the frequency of each production
plot3 <- ggplot(g_China, aes(Year, Quantity))
print(plot3 + geom_point(size=3))


##### Germany #####
g_Germany <- filter(Geo, Country.or.Area == "Germany") # creation of Germany data set
View(g_Germany)
fivenum(g_Germany$Quantity,na.rm = TRUE)
mean(g_Germany$Quantity) # Germany Quantity column mean
median((g_Germany$Quantity)) # Germany Quantity column median
y <- table(g_Germany$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Germany Quantity mode(s)

# EDA
barplot(table(g_Germany$Quantity)) # gives the frequency of each production
plot4 <- ggplot(g_Germany, aes(Year, Quantity))
print(plot4 + geom_point(size=3))


##### India #####
g_India <- filter(Geo, Country.or.Area == "India") # creation of India data set
View(g_India)
fivenum(g_India$Quantity,na.rm = TRUE)
mean(g_India$Quantity) # India Quantity column mean
median((g_India$Quantity)) # India Quantity column median
y <- table(g_India$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # India Quantity mode(s)

# EDA
barplot(table(g_India$Quantity)) # gives the frequency of each production
plot5 <- ggplot(g_India, aes(Year, Quantity))
print(plot5 + geom_point(size=3))


##### Mexico #####
g_Mexico <- filter(Geo, Country.or.Area == "Mexico") # creation of Mexico data set
View(g_Mexico)
fivenum(g_Mexico$Quantity,na.rm = TRUE)
mean(g_Mexico$Quantity) # Mexico Quantity column mean
median((g_Mexico$Quantity)) # Mexico Quantity column median
y <- table(g_Mexico$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Mexico Quantity mode(s)

# EDA
barplot(table(g_Mexico$Quantity)) # gives the frequency of each production
plot6 <- ggplot(g_Mexico, aes(Year, Quantity))
print(plot6 + geom_point(size=3))


##### Russian Federation #####
g_Russian_Fed <- filter(Geo, Country.or.Area == "Russian Federation") # creation of Russian Federation data set
View(g_Russian_Fed)
fivenum(g_Russian_Fed$Quantity,na.rm = TRUE)
mean(g_Russian_Fed$Quantity) # Russian Federation Quantity column mean
median((g_Russian_Fed$Quantity)) # Russian Federation Quantity column median
y <- table(g_Russian_Fed$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Russian Federation Quantity mode(s)

# EDA
barplot(table(g_Russian_Fed$Quantity)) # gives the frequency of each production
plot7 <- ggplot(g_Russian_Fed, aes(Year, Quantity))
print(plot7 + geom_point(size=3))


##### United States #####
g_United_States <- filter(Geo, Country.or.Area == "United States") # creation of United States data set
View(g_United_States)
fivenum(g_United_States$Quantity,na.rm = TRUE)
mean(g_United_States$Quantity) # United States Quantity column mean
median((g_United_States$Quantity)) # United States Quantity column median
y <- table(g_United_States$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # United States Quantity mode(s)

# EDA
barplot(table(g_United_States$Quantity)) # gives the frequency of each production
plot8 <- ggplot(g_United_States, aes(Year, Quantity))
print(plot8 + geom_point(size=3))


######### Yearly analysis ##########
### 2019 ###
g_y2019 <- filter(Geo, Year == "2019") # creation of 2019 data set
View(g_y2019)
fivenum(g_y2019$Quantity,na.rm = TRUE)
mean(g_y2019$Quantity) # 2019 Quantity column mean
median((g_y2019$Quantity)) # 2019 Quantity column median
y <- table(g_y2019$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # 2019 Quantity mode(s)

# EDA
barplot(table(g_y2019$Quantity)) # gives the frequency of each production
plot9 <- ggplot(g_y2019, aes(Country.or.Area, Quantity))
print(plot9 + geom_point(size=3))


### 2018 ###
g_y2018 <- filter(Geo, Year == "2018") # creation of 2018 data set
View(g_y2018)
fivenum(g_y2018$Quantity,na.rm = TRUE)
mean(g_y2018$Quantity) # 2018 Quantity column mean
median((g_y2018$Quantity)) # 2018 Quantity column median
y <- table(g_y2018$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # 2018 Quantity mode(s)

# EDA
barplot(table(g_y2018$Quantity)) # gives the frequency of each production
plot10 <- ggplot(g_y2018, aes(Country.or.Area, Quantity))
print(plot10 + geom_point(size=3))


### 2017 ###
g_y2017 <- filter(Geo, Year == "2017") # creation of 2017 data set
View(g_y2017)
fivenum(g_y2017$Quantity,na.rm = TRUE)
mean(g_y2017$Quantity) # 2017 Quantity column mean
median((g_y2017$Quantity)) # 2017 Quantity column median
y <- table(g_y2017$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # 2017 Quantity mode(s)

# EDA
barplot(table(g_y2017$Quantity)) # gives the frequency of each production
plot11 <- ggplot(g_y2017, aes(Country.or.Area, Quantity))
print(plot11 + geom_point(size=3))


### 2016 ###
g_y2016 <- filter(Geo, Year == "2016") # creation of 2016 data set
View(g_y2016)
fivenum(g_y2016$Quantity,na.rm = TRUE)
mean(g_y2016$Quantity) # 2016 Quantity column mean
median((g_y2016$Quantity)) # 2016 Quantity column median
y <- table(g_y2016$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # 2016 Quantity mode(s)

# EDA
barplot(table(g_y2016$Quantity)) # gives the frequency of each production
plot11 <- ggplot(g_y2016, aes(Country.or.Area, Quantity))
print(plot11 + geom_point(size=3))



######### Overall Analysis of yearly Hydro data #########
Countries <- filter(Geo, Country.or.Area %in% c("Australia", "Brazil",
                                                  "China", "Germany", "India", "Mexico", "Russian Federation",
                                                  "United States")) # creation of specific countries data set

World_Geo <- ggplot(Geo, aes(Country.or.Area, Quantity, color = Year))
print(World_Geo + geom_point(size=3) +
        theme(axis.text.x=element_text(angle=90, size=8, vjust=0.5)))

World_Geo_yr <- ggplot(Geo, aes(Country.or.Area, Year, color = Quantity))
print(World_Geo_yr + geom_point(size=3) +
        theme(axis.text.x=element_text(angle=90, size=8, vjust=0.5)))

Countries_Geo <- ggplot(Countries, aes(Country.or.Area, Quantity, color = Year))
print(Countries_Geo + geom_point(size=3) +
        theme(axis.text.x=element_text(angle=60, size=8, vjust=0.5)))

Countries_Geo_yr <- ggplot(Countries, aes(Country.or.Area, Year, color = Quantity))
print(Countries_Geo_yr + geom_point(size=3) +
        theme(axis.text.x=element_text(angle=60, size=8, vjust=0.5)))


### classifications ###
# breaking down quantity column
# takes quantities and organizes them into ranges
summary(Geo$Quantity)
categ_all <- as.numeric(Geo$Quantity)
categ_all <- cut(categ_all, br = c(0,100,5000,50000,20000,120000), labels = c("Lowest Production",'Semi-Low Production','Middle Production','Semi-High Production','Highest Production'))
categ_all <- as.factor(categ_all)
summary(categ_all)

World_Geo_cat <- ggplot(Geo, aes(Country.or.Area, Quantity, color = categ_all))
print(World_Geo_cat + geom_point(size=3) +
        theme(axis.text.x=element_text(angle=90, size=8, vjust=0.5)))

World_Geo_yr_cat <- ggplot(Geo, aes(Country.or.Area, Year, color = categ_all))
print(World_Geo_yr_cat + geom_point(size=3))

summary(Countries$Quantity)
categ_countries <- as.numeric(Countries$Quantity)
categ_countries <- cut(categ_countries, br = c(0,100,5000,50000,200000,1250000), labels = c("Lowest Production",'Semi-Low Production','Middle Production','Semi-High Production','Highest Production'))
categ_countries <- as.factor(categ_countries)
summary(categ_countries)

Countries_Geo_yr_cat <- ggplot(Countries, aes(Country.or.Area, Quantity, color = categ_countries))
print(Countries_Geo_yr_cat + geom_point(size=3))


### linear regression ###
Geo_year <- Geo$Year
Geo_prod <- Geo$Quantity
Geo_countries <- Geo$Country.or.Area

lmGeo_prod <- lm(Geo_prod ~ Geo_year) # linear regression of year and quantity
lmGeo_prod
summary(lmGeo_prod) # summary of regression line, R^2 = 0.01246 

lm_Geo <- lm(Geo_prod ~ Geo_year + Geo_countries) # linear regression of year and quantity and country
lm_Geo
summary(lm_Geo) # summary of regression line, R^2 = 0.9262 

lm_Geo_yr <- lm(Geo_year ~ Geo_prod + Geo_countries) # linear regression of year and quantity and country
lm_Geo_yr
summary(lm_Geo_yr) # summary of regression line, R^2 = 0.417 


# lin reg by country (Australia has no nuclear power production)
lm_Geo_Australia <- lm(g_Australia$Quantity ~ g_Australia$Year) # linear regression of year and quantity
lm_Geo_Australia
summary(lm_Geo_Australia) # summary of regression line, R^2 = 0.2633  

#lm_Geo_Brazil <- lm(g_Brazil$Quantity ~ g_Brazil$Year) # linear regression of year and quantity
#lm_Geo_Brazil
#summary(lm_Geo_Brazil) # summary of regression line, R^2 = 0.8526 

#lm_Geo_China <- lm(g_China$Quantity ~ g_China$Year) # linear regression of year and quantity
#lm_Geo_China
#summary(lm_Geo_China) # summary of regression line, R^2 = 0.8866 

lm_Geo_Germany <- lm(g_Germany$Quantity ~ g_Germany$Year) # linear regression of year and quantity
lm_Geo_Germany
summary(lm_Geo_Germany) # summary of regression line, R^2 =  0.8871

#lm_Geo_India <- lm(g_India$Quantity ~ g_India$Year) # linear regression of year and quantity
#lm_Geo_India
#summary(lm_Geo_India) # summary of regression line, R^2 = 0.7945 

lm_Geo_Mexico <- lm(g_Mexico$Quantity ~ g_Mexico$Year) # linear regression of year and quantity
lm_Geo_Mexico
summary(lm_Geo_Mexico) # summary of regression line, R^2 = 0.1383 

lm_Geo_Russian_Fed <- lm(g_Russian_Fed$Quantity ~ g_Russian_Fed$Year) # linear regression of year and quantity
lm_Geo_Russian_Fed
summary(lm_Geo_Russian_Fed) # summary of regression line, R^2 =  0.7648  

lm_Geo_United_States <- lm(g_United_States$Quantity ~ g_United_States$Year) # linear regression of year and quantity
lm_Geo_United_States
summary(lm_Geo_United_States) # summary of regression line, R^2 = 0.3999 


### clustering ###
# enable geocoding and static maps to create ggmap using Google API
register_google(key = "AIzaSyBqCaIO6Enk-WuFjmSLnJuWcGm89iyPN5w")

Geo_Countryavg <- aggregate(x = Geo$Quantity, by = list(Geo$Country.or.Area), FUN = mean)
View(Geo_Countryavg)
Geo_Countryavg <- rename(Geo_Countryavg, Country = Group.1)
Geo_Countryavg <- rename(Geo_Countryavg, "Quantity Avg, kW-hr" = x)
View(Geo_Countryavg)
# can use this line if you have a billing account attached to your API
# locations <- mutate_geocode(Geo_Countryavg, Group.1) # creates latitude and longitude for each location
Geo_Countryavg$latitude <- c(-26.307, 47.474, -27.543, 10.0724, 45.582, 
                             13.780, 8.296, 15.558, 46.293, 50.841,
                             16.215, 15.5527, 14.714, 46.915, 64.853, 
                             -1.265, 43.542, 35.983, 0.220, 23.455, 
                             -42.793, 12.774, 25.751, -6.073, 11.642,
                             40.083, 61.598, 16.158, 39.308, 40.479, 
                             46.650, 14.24)
Geo_Countryavg$longitude <- c(135.095, 14.2996, -70.178, -83.998, 15.976, 
                              -88.884, 40.306, 38.818, 2.4731, 10.405, 
                              -61.642,-90.304, -86.982, 19.305, -18.237, 
                              115.158, 12.154, 138.729, 37.825, -102.43, 
                              172.313, -84.988, 94.529, 144.078, 122.976,
                              -8.029, 93.3004, 101.846, 35.3537, -99.505, 
                              58.003, 108.44)
View(Geo_Countryavg)

library(cluster)
set.seed(100)
clusters <- kmeans(Geo_Countryavg[,2], 8)
Geo_Countryavg$Cluster <- as.factor(clusters$cluster)
head(Geo_Countryavg)
print(clusters) # prints data set

library(ggmap)
library(ggrepel)
WorldMap <- get_stamenmap(c(left = -135, bottom = -60, right = 180, top = 70), zoom = 4, maptype = c("terrain-background"), color = "bw")
ggmap(WorldMap) + geom_point(data = Geo_Countryavg, aes(x = longitude[], y = latitude[], colour = as.factor(Cluster)), size = 3) +
  ggtitle("Geothermal Electricity KMean Cluster by Quantity") +
  geom_text_repel(data = Geo_Countryavg, aes(x = longitude, y = latitude, label = Country), size = 3)

