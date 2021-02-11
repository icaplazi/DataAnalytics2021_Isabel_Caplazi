# Creating a dataframe

days <- c('Mon', 'Tue', 'Wed', 'Thur', 'Fri', 'Sat', 'Sun') 
temp <- c(28,30.5,32,31.2,29.3,27.9,26.4)
snowed <- c('T','T','F','F','T','T','F') # whether it snowed that day
help("data.frame")
RPI_Weather_Week <- data.frame(days,temp,snowed) # creates data frame for all data

RPI_Weather_Week
head(RPI_Weather_Week) # shows first 6 rows

str(RPI_Weather_Week)

summary(RPI_Weather_Week)

RPI_Weather_Week[1,] # first row all columns
RPI_Weather_Week[,1] # first column all rows
RPI_Weather_Week[,'snowed']
RPI_Weather_Week[,'days']
RPI_Weather_Week[,'temp']
RPI_Weather_Week[1:5,c("days","temp")]
RPI_Weather_Week$temp # chooses temp column
subset(RPI_Weather_Week,subset=snowed==TRUE)

sorted.snowed <- order(RPI_Weather_Week['snowed'])
sorted.snowed
RPI_Weather_Week[sorted.snowed,]

# RPI_Weather_Week[descending_snowed,]
dec.snow <- order(-RPI_Weather_Week$temp)
dec.snow
RPI_Weather_Week[dec.snow,]

# creating empty dataframe
empty.DataFrame <- data.frame()
v1 <- 1:10
v1
letters
v2 <- letters[1:10]
df <- data.frame(col.name.1 = v1, col.name.2 = v2)
df
# importing data and exporting data
write.csv(df,file = 'saved_df1.csv')
df2 <- read.csv('saved_df1.csv')
df2

nrow(df)
ncol(df)
colnames(df)



# Exercise #1
# for 2010 EPI Data

help("read.csv")
EPI_2010data <-read.csv(file.choose(), header = TRUE) # EPI_data = 2010 EPI data
# changes first row to header
names(EPI_2010data) <- as.matrix(EPI_2010data[1,])
EPI_2010data <- EPI_2010data[-1,]
EPI_2010data[] <- lapply(EPI_2010data, function(x) type.convert(as.character(x)))
EPI_2010data

View(EPI_2010data) # opens data up in another tab
attach(EPI_2010data)
fix(EPI_2010data)

# for EPI column
EPI
tf <- is.na(EPI)
E <- EPI[!tf] # removes NA values
E

summary(EPI)
fivenum(EPI,na.rm=TRUE)
stem(EPI)
hist(EPI)
hist(EPI, seq(30.,95,1.0),prob=TRUE)
lines(density(EPI,na.rm=TRUE,bw=1)) # or bw = "SJ"
rug(EPI)
help(stem)

plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(EPI); qqline(EPI)
x <- seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

# for Agriculture
AGRICULTURE
tf <- is.na(AGRICULTURE)
AGR <- AGRICULTURE[!tf] # removes NA values
AGR

summary(AGRICULTURE)
fivenum(AGRICULTURE,na.rm=TRUE)
stem(AGRICULTURE)
hist(AGRICULTURE)
hist(AGRICULTURE, seq(24.,100.,1.0),prob=TRUE)
lines(density(AGRICULTURE,na.rm=TRUE,bw=1)) # or bw = "SJ"
rug(AGRICULTURE)

plot(ecdf(AGRICULTURE), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(AGRICULTURE); qqline(AGRICULTURE)
x <- seq(24,100,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

# for Climate
CLIMATE
tf <- is.na(CLIMATE)
CLI <- CLIMATE[!tf] # removes NA values
CLI

summary(CLIMATE)
fivenum(CLIMATE,na.rm=TRUE)
stem(CLIMATE)
hist(CLIMATE)
hist(CLIMATE, seq(9.,97.,1.0),prob=TRUE)
lines(density(CLIMATE,na.rm=TRUE,bw=1)) # or bw = "SJ"
rug(CLIMATE)

plot(ecdf(CLIMATE), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(CLIMATE); qqline(CLIMATE)
x <- seq(9,97,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

# Boxplot comparisons
boxplot(EPI,AGRICULTURE) 
boxplot(EPI,CLIMATE) 
boxplot(AGRICULTURE,CLIMATE) 


# Exercise #2
EPILand<-EPI[!Landlock]
ELand <- EPILand[!is.na(EPILand)]
hist(ELand)
hist(ELand, seq(30., 95., 1.0), prob=TRUE)

EPINSW<-EPI[!No_surface_water]
ENSW <- EPINSW[!is.na(EPINSW)]
hist(ENSW)
hist(ENSW, seq(30., 95., 1.0), prob=TRUE)

EPIDes<-EPI[!Desert]
EDes <- EPIDes[!is.na(EPIDes)]
hist(EDes)
hist(EDes, seq(30., 95., 1.0), prob=TRUE)

EPIPop<-EPI[!High_Population_Density]
EPop <- EPIPop[!is.na(EPIPop)]
hist(EPop)
hist(EPop, seq(30., 95., 1.0), prob=TRUE)

EPI_South_Asia<-EPI[!GEO_subregion["South_Asia"]]
E_S_Asia <- EPI_South_Asia[!is.na(EPI_South_Asia)]
hist(E_S_Asia)
hist(E_S_Asia, seq(30., 95., 1.0), prob=TRUE)

