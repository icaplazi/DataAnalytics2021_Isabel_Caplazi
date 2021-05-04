# Nuclear Energy Set

# download data set and export as csv file
# call Nuclear electricity dataset as csv
Nuclear <- read.csv("C:/users/caplam/Downloads/UNdata_Nuclear/UNdata_Nuclear.csv")
View(Nuclear) 
Nuclear <- filter(Nuclear, Commodity...Transaction == "Electricity - total nuclear production")
View(Nuclear)
colnames(Nuclear) # column names
head(Nuclear) # gives first 6 rows
summary(Nuclear) 

# call necessary packages
library(dplyr) # dplyr package for filter function
library(ggplot2) # calls ggplot package


##### Australia #####
n_Australia <- filter(Nuclear, Country.or.Area == "Australia") # creation of Australia data set
View(n_Australia)
fivenum(n_Australia$Quantity,na.rm = TRUE)
mean(n_Australia$Quantity) # Australia Quantity column mean
median((n_Australia$Quantity)) # Australia Quantity column median
y <- table(n_Australia$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Australia Quantity mode(s)

# EDA
barplot(table(n_Australia$Quantity)) # gives the frequency of each production
plot1 <- ggplot(n_Australia, aes(Year, Quantity))
print(plot1 + geom_point(size=3))


##### Brazil #####
n_Brazil <- filter(Nuclear, Country.or.Area == "Brazil") # creation of Brazil data set
View(n_Brazil)
fivenum(n_Brazil$Quantity,na.rm = TRUE)
mean(n_Brazil$Quantity) # Brazil Quantity column mean
median((n_Brazil$Quantity)) # Brazil Quantity column median
y <- table(n_Brazil$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Brazil Quantity mode(s)

# EDA
barplot(table(n_Brazil$Quantity)) # gives the frequency of each production
plot2 <- ggplot(n_Brazil, aes(Year, Quantity))
print(plot2 + geom_point(size=3))


##### China #####
n_China <- filter(Nuclear, Country.or.Area == "China") # creation of China data set
View(n_China)
fivenum(n_China$Quantity,na.rm = TRUE)
mean(n_China$Quantity) # China Quantity column mean
median((n_China$Quantity)) # China Quantity column median
y <- table(n_China$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # China Quantity mode(s)

# EDA
barplot(table(n_China$Quantity)) # gives the frequency of each production
plot3 <- ggplot(n_China, aes(Year, Quantity))
print(plot3 + geom_point(size=3))


##### Germany #####
n_Germany <- filter(Nuclear, Country.or.Area == "Germany") # creation of Germany data set
View(n_Germany)
fivenum(n_Germany$Quantity,na.rm = TRUE)
mean(n_Germany$Quantity) # Germany Quantity column mean
median((n_Germany$Quantity)) # Germany Quantity column median
y <- table(n_Germany$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Germany Quantity mode(s)

# EDA
barplot(table(n_Germany$Quantity)) # gives the frequency of each production
plot4 <- ggplot(n_Germany, aes(Year, Quantity))
print(plot4 + geom_point(size=3))


##### India #####
n_India <- filter(Nuclear, Country.or.Area == "India") # creation of India data set
View(n_India)
fivenum(n_India$Quantity,na.rm = TRUE)
mean(n_India$Quantity) # India Quantity column mean
median((n_India$Quantity)) # India Quantity column median
y <- table(n_India$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # India Quantity mode(s)

# EDA
barplot(table(n_India$Quantity)) # gives the frequency of each production
plot5 <- ggplot(n_India, aes(Year, Quantity))
print(plot5 + geom_point(size=3))


##### Mexico #####
n_Mexico <- filter(Nuclear, Country.or.Area == "Mexico") # creation of Mexico data set
View(n_Mexico)
fivenum(n_Mexico$Quantity,na.rm = TRUE)
mean(n_Mexico$Quantity) # Mexico Quantity column mean
median((n_Mexico$Quantity)) # Mexico Quantity column median
y <- table(n_Mexico$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Mexico Quantity mode(s)

# EDA
barplot(table(n_Mexico$Quantity)) # gives the frequency of each production
plot6 <- ggplot(n_Mexico, aes(Year, Quantity))
print(plot6 + geom_point(size=3))


##### Russian Federation #####
n_Russian_Fed <- filter(Nuclear, Country.or.Area == "Russian Federation") # creation of Russian Federation data set
View(n_Russian_Fed)
fivenum(n_Russian_Fed$Quantity,na.rm = TRUE)
mean(n_Russian_Fed$Quantity) # Russian Federation Quantity column mean
median((n_Russian_Fed$Quantity)) # Russian Federation Quantity column median
y <- table(n_Russian_Fed$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Russian Federation Quantity mode(s)

# EDA
barplot(table(n_Russian_Fed$Quantity)) # gives the frequency of each production
plot7 <- ggplot(n_Russian_Fed, aes(Year, Quantity))
print(plot7 + geom_point(size=3))


##### United States #####
n_United_States <- filter(Nuclear, Country.or.Area == "United States") # creation of United States data set
View(n_United_States)
fivenum(n_United_States$Quantity,na.rm = TRUE)
mean(n_United_States$Quantity) # United States Quantity column mean
median((n_United_States$Quantity)) # United States Quantity column median
y <- table(n_United_States$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # United States Quantity mode(s)

# EDA
barplot(table(n_United_States$Quantity)) # gives the frequency of each production
plot8 <- ggplot(n_United_States, aes(Year, Quantity))
print(plot8 + geom_point(size=3))


######### Yearly analysis ##########
### 2019 ###
n_y2019 <- filter(Nuclear, Year == "2019") # creation of 2019 data set
View(n_y2019)
fivenum(n_y2019$Quantity,na.rm = TRUE)
mean(n_y2019$Quantity) # 2019 Quantity column mean
median((n_y2019$Quantity)) # 2019 Quantity column median
y <- table(n_y2019$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # 2019 Quantity mode(s)

# EDA
barplot(table(n_y2019$Quantity)) # gives the frequency of each production
plot9 <- ggplot(n_y2019, aes(Country.or.Area, Quantity))
print(plot9 + geom_point(size=3))


### 2018 ###
n_y2018 <- filter(Nuclear, Year == "2018") # creation of 2018 data set
View(n_y2018)
fivenum(n_y2018$Quantity,na.rm = TRUE)
mean(n_y2018$Quantity) # 2018 Quantity column mean
median((n_y2018$Quantity)) # 2018 Quantity column median
y <- table(n_y2018$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # 2018 Quantity mode(s)

# EDA
barplot(table(n_y2018$Quantity)) # gives the frequency of each production
plot10 <- ggplot(n_y2018, aes(Country.or.Area, Quantity))
print(plot10 + geom_point(size=3))


### 2017 ###
n_y2017 <- filter(Nuclear, Year == "2017") # creation of 2017 data set
View(n_y2017)
fivenum(n_y2017$Quantity,na.rm = TRUE)
mean(n_y2017$Quantity) # 2017 Quantity column mean
median((n_y2017$Quantity)) # 2017 Quantity column median
y <- table(n_y2017$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # 2017 Quantity mode(s)

# EDA
barplot(table(n_y2017$Quantity)) # gives the frequency of each production
plot11 <- ggplot(n_y2017, aes(Country.or.Area, Quantity))
print(plot11 + geom_point(size=3))


### 2016 ###
n_y2016 <- filter(Nuclear, Year == "2016") # creation of 2016 data set
View(n_y2016)
fivenum(n_y2016$Quantity,na.rm = TRUE)
mean(n_y2016$Quantity) # 2016 Quantity column mean
median((n_y2016$Quantity)) # 2016 Quantity column median
y <- table(n_y2016$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # 2016 Quantity mode(s)

# EDA
barplot(table(n_y2016$Quantity)) # gives the frequency of each production
plot11 <- ggplot(n_y2016, aes(Country.or.Area, Quantity))
print(plot11 + geom_point(size=3))



######### Overall Analysis of yearly nuclear data #########
Countries <- filter(Nuclear, Country.or.Area %in% c("Australia", "Brazil",
                                                 "China", "Germany", "India", "Mexico", "Russian Federation",
                                                 "United States")) # creation of specific countries data set

World_Nuclear <- ggplot(Nuclear, aes(Country.or.Area, Quantity, color = Year))
print(World_Nuclear + geom_point(size=3) +
        theme(axis.text.x=element_text(angle=90, size=8, vjust=0.5)))

World_Nuclear_yr <- ggplot(Nuclear, aes(Country.or.Area, Year, color = Quantity))
print(World_Nuclear_yr + geom_point(size=3) +
        theme(axis.text.x=element_text(angle=90, size=8, vjust=0.5)))

Countries_Nuclear <- ggplot(Countries, aes(Country.or.Area, Quantity, color = Year))
print(Countries_Nuclear + geom_point(size=3) +
        theme(axis.text.x=element_text(angle=60, size=8, vjust=0.5)))

Countries_Nuclear_yr <- ggplot(Countries, aes(Country.or.Area, Year, color = Quantity))
print(Countries_Nuclear_yr + geom_point(size=3) +
      theme(axis.text.x=element_text(angle=60, size=8, vjust=0.5)))


### classifications ###
# breaking down quantity column
# takes quantities and organizes them into ranges
summary(Nuclear$Quantity)
categ_all <- as.numeric(Nuclear$Quantity)
categ_all <- cut(categ_all, br = c(0,100,5000,50000,200000,850000), labels = c("Lowest Production",'Semi-Low Production','Middle Production','Semi-High Production','Highest Production'))
categ_all <- as.factor(categ_all)
summary(categ_all)

World_Nuclear_cat <- ggplot(Nuclear, aes(Country.or.Area, Quantity, color = categ_all))
print(World_Nuclear_cat + geom_point(size=3) +
        theme(axis.text.x=element_text(angle=90, size=8, vjust=0.5)))

World_Nuclear_yr_cat <- ggplot(Nuclear, aes(Country.or.Area, Year, color = categ_all))
print(World_Nuclear_yr_cat + geom_point(size=3))

summary(Countries$Quantity)
categ_countries <- as.numeric(Countries$Quantity)
categ_countries <- cut(categ_countries, br = c(0,100,5000,50000,200000,850000), labels = c("Lowest Production",'Semi-Low Production','Middle Production','Semi-High Production','Highest Production'))
categ_countries <- as.factor(categ_countries)
summary(categ_countries)

Countries_Nuclear_yr_cat <- ggplot(Countries, aes(Country.or.Area, Quantity, color = categ_countries))
print(Countries_Nuclear_yr_cat + geom_point(size=3))


### linear regression ###
Nuclear_year <- Nuclear$Year
Nuclear_prod <- Nuclear$Quantity
Nuclear_countries <- Nuclear$Country.or.Area

lmNuclear_prod <- lm(Nuclear_prod ~ Nuclear_year) # linear regression of year and quantity
lmNuclear_prod
summary(lmNuclear_prod) # summary of regression line, R^2 = -0.0009566 

lm_Nuclear <- lm(Nuclear_prod ~ Nuclear_year + Nuclear_countries) # linear regression of year and quantity and country
lm_Nuclear
summary(lm_Nuclear) # summary of regression line, R^2 = 0.9585 

lm_Nuclear_yr <- lm(Nuclear_year ~ Nuclear_prod + Nuclear_countries) # linear regression of year and quantity and country
lm_Nuclear_yr
summary(lm_Nuclear_yr) # summary of regression line, R^2 = 0.03144 


# lin reg by country (Australia has no nuclear power production)
#lm_Nuclear_Australia <- lm(n_Australia$Quantity ~ n_Australia$Year) # linear regression of year and quantity
#lm_Nuclear_Australia
#summary(lm_Nuclear_Australia) # summary of regression line

lm_Nuclear_Brazil <- lm(n_Brazil$Quantity ~ n_Brazil$Year) # linear regression of year and quantity
lm_Nuclear_Brazil
summary(lm_Nuclear_Brazil) # summary of regression line, R^2 = 0.824 

lm_Nuclear_China <- lm(n_China$Quantity ~ n_China$Year) # linear regression of year and quantity
lm_Nuclear_China
summary(lm_Nuclear_China) # summary of regression line, R^2 = 0.7617 

lm_Nuclear_Germany <- lm(n_Germany$Quantity ~ n_Germany$Year) # linear regression of year and quantity
lm_Nuclear_Germany
summary(lm_Nuclear_Germany) # summary of regression line, R^2 = 0.6308 

lm_Nuclear_India <- lm(n_India$Quantity ~ n_India$Year) # linear regression of year and quantity
lm_Nuclear_India
summary(lm_Nuclear_India) # summary of regression line, R^2 = 0.8971 

lm_Nuclear_Mexico <- lm(n_Mexico$Quantity ~ n_Mexico$Year) # linear regression of year and quantity
lm_Nuclear_Mexico
summary(lm_Nuclear_Mexico) # summary of regression line, R^2 = 0.5451 

lm_Nuclear_Russian_Fed <- lm(n_Russian_Fed$Quantity ~ n_Russian_Fed$Year) # linear regression of year and quantity
lm_Nuclear_Russian_Fed
summary(lm_Nuclear_Russian_Fed) # summary of regression line, R^2 = 0.938 

lm_Nuclear_United_States <- lm(n_United_States$Quantity ~ n_United_States$Year) # linear regression of year and quantity
lm_Nuclear_United_States
summary(lm_Nuclear_United_States) # summary of regression line, R^2 = 0.7857 


### clustering ###
# enable geocoding and static maps to create ggmap using Google API
# have to register using individual API key through google
# register_google(key = "[your key]")
register_google(key = "AIzaSyBqCaIO6Enk-WuFjmSLnJuWcGm89iyPN5w")

Nuclear_Countryavg <- aggregate(x = Nuclear$Quantity, by = list(Nuclear$Country.or.Area), FUN = mean)
View(Nuclear_Countryavg)
Nuclear_Countryavg <- rename(Nuclear_Countryavg, Country = Group.1)
Nuclear_Countryavg <- rename(Nuclear_Countryavg, "Quantity Avg, kW-hr" = x)
View(Nuclear_Countryavg)
# can use this line if you have a billing account attached to your API
# locations <- mutate_geocode(Nuclear_Countryavg, Group.1) # creates latitude and longitude for each location
Nuclear_Countryavg$latitude <- c(-36.136, 40.5595, 50.795, 9.9158, 42.816, 
                                 60.466, 35.205, 49.818, 48.971, 63.626, 
                                 46.293, 52.942, 50.841, 50.409, 46.915, 
                                 22.060, 31.675, 35.983, 36.815, 55.459, 
                                 23.455, 52.091, 25.751, 29.515, 45.659,
                                 61.598, 48.796, 46.022, -28.839, 40.088, 
                                 63.768, 46.928, 49.377, 54.115, 40.479,
                                 46.6500, 45.011)
Nuclear_Countryavg$longitude <- c(-65.123, 44.683, 4.532, -52.573, 25.310,
                                  -108.224, 100.793, 15.165, 18.158, 26.533,
                                  2.4731, 13.459, 10.405, 7.0433, 19.305, 
                                  78.395, 54.334, 138.729, 127.972, 23.927,
                                  -102.43, 5.4755, 94.529, 68.985, 24.776, 
                                  93.3004, 19.445, 14.679, 24.78196, -3.085,
                                  16.7001, 8.1687, 30.777, -2.0976, -99.505,
                                  58.003, 19.071)
View(Nuclear_Countryavg)

library(cluster)
set.seed(100)
clusters <- kmeans(Nuclear_Countryavg[,2], 8)
Nuclear_Countryavg$Cluster <- as.factor(clusters$cluster)
head(Nuclear_Countryavg)
print(clusters) # prints data set

library(ggmap)
library(ggrepel)
WorldMap <- get_stamenmap(c(left = -135, bottom = -60, right = 180, top = 70), zoom = 4, maptype = c("terrain-background"), color = "bw")
ggmap(WorldMap) + geom_point(data = Nuclear_Countryavg, aes(x = longitude[], y = latitude[], colour = as.factor(Cluster)), size = 3) +
  ggtitle("Nuclear Electricity KMean Cluster by Quantity") +
  geom_text_repel(data = Nuclear_Countryavg, aes(x = longitude, y = latitude, label = Country), size = 3)
