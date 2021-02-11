# Attaching EPI Dataset
EPI_2010data <- read.csv("C:/users/caplam/Downloads/EPI2010_data.csv")
attach(EPI_2010data)
names(EPI_2010data)
EPI_2010data

View(EPI_2010data) # opens data up in another tab


# EPI Data Set (& removed NA values)
EPI
tf <- is.na(EPI)
E <- EPI[!tf] # removes NA values
E

# Quantile Quantile Plot
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(EPI); qqline(EPI)
x <- seq(9,97,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

# More on qq plots
plot(ecdf(EPI_2010data$EPI),do.points=FALSE,verticals = TRUE)
plot(ecdf(EPI_2010data$EPI),do.points=TRUE,verticals = TRUE) # points are visible
par(pty="s")
help("qqnorm") # read the RStudio documentation for qqnorm
help("qqplot") # read the RStudio documentation for qqplot
qqnorm(EPI_2010data$EPI)
qqline(EPI_2010data$EPI) # adding the line on the Q-Q plot
x <- seq(30,95,1)
x
x2 <-seq(30,95,2)
x2
x2 <-seq(30,96,2)
x2
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)

# BoxPlot comparison
boxplot(EPI_2010data$EPI,EPI_2010data$DALY)



# Comparison of EPI and ENVHEALTH
# ENVHEALTH Data Set (& removed NA values)
ENVHEALTH
tf <- is.na(ENVHEALTH)
ENV <- ENVHEALTH[!tf] # removes NA values
ENV
summary(ENV)

# Quantile Quantile Plot
plot(ecdf(ENVHEALTH), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(ENVHEALTH); qqline(ENVHEALTH)
x <- seq(9,97,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

# More on qq plots
plot(ecdf(EPI_2010data$ENVHEALTH),do.points=FALSE,verticals = TRUE)
plot(ecdf(EPI_2010data$ENVHEALTH),do.points=TRUE,verticals = TRUE) # points are visible
par(pty="s")
qqnorm(EPI_2010data$ENVHEALTH)
qqline(EPI_2010data$ENVHEALTH) # adding the line on the Q-Q plot
x <- seq(1,96,1)
x
x2 <-seq(1,96,2)
x2
x2 <-seq(1,96,2)
x2
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)

# BoxPlot comparison
boxplot(EPI_2010data$EPI,EPI_2010data$ENVHEALTH)



# Comparison of EPI and ECOSYSTEM
# ECOSYSTEM Data Set (& removed NA values)
ECOSYSTEM
tf <- is.na(ECOSYSTEM)
ECO <- ECOSYSTEM[!tf] # removes NA values
ECO
summary(ECO)

# Quantile Quantile Plot
plot(ecdf(ECOSYSTEM), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(ECOSYSTEM); qqline(ECOSYSTEM)
x <- seq(9,97,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

# More on qq plots
plot(ecdf(EPI_2010data$ECOSYSTEM),do.points=FALSE,verticals = TRUE)
plot(ecdf(EPI_2010data$ECOSYSTEM),do.points=TRUE,verticals = TRUE) # points are visible
par(pty="s")
qqnorm(EPI_2010data$ECOSYSTEM)
qqline(EPI_2010data$ECOSYSTEM) # adding the line on the Q-Q plot
x <- seq(1,96,1)
x
x2 <-seq(1,96,2)
x2
x2 <-seq(1,96,2)
x2
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)

# BoxPlot comparison
boxplot(EPI_2010data$EPI,EPI_2010data$ECOSYSTEM)



# Comparison of EPI and AIR_H
# AIR_H Data Set (& removed NA values)
AIR_H
tf <- is.na(AIR_H)
AIR <- AIR_H[!tf] # removes NA values
AIR
summary(AIR)

# Quantile Quantile Plot
plot(ecdf(AIR_H), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(AIR_H); qqline(AIR_H)
x <- seq(9,97,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

# More on qq plots
plot(ecdf(EPI_2010data$AIR_H),do.points=FALSE,verticals = TRUE)
plot(ecdf(EPI_2010data$AIR_H),do.points=TRUE,verticals = TRUE) # points are visible
par(pty="s")
qqnorm(EPI_2010data$AIR_H)
qqline(EPI_2010data$AIR_H) # adding the line on the Q-Q plot
x <- seq(1,96,1)
x
x2 <-seq(1,96,2)
x2
x2 <-seq(1,96,2)
x2
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)

# BoxPlot comparison
boxplot(EPI_2010data$EPI,EPI_2010data$AIR_H)



# Comparison of EPI and WATER_H
# WATER_H Data Set (& removed NA values)
WATER_H
tf <- is.na(WATER_H)
WATER <- WATER_H[!tf] # removes NA values
WATER
summary(WATER)

# Quantile Quantile Plot
plot(ecdf(WATER_H), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(WATER_H); qqline(WATER_H)
x <- seq(9,97,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

# More on qq plots
plot(ecdf(EPI_2010data$WATER_H),do.points=FALSE,verticals = TRUE)
plot(ecdf(EPI_2010data$WATER_H),do.points=TRUE,verticals = TRUE) # points are visible
par(pty="s")
qqnorm(EPI_2010data$WATER_H)
qqline(EPI_2010data$WATER_H) # adding the line on the Q-Q plot
x <- seq(1,96,1)
x
x2 <-seq(1,96,2)
x2
x2 <-seq(1,96,2)
x2
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)

# BoxPlot comparison
boxplot(EPI_2010data$EPI,EPI_2010data$WATER_H)



# Comparison of EPI and AIR_E
# AIR_E Data Set (& removed NA values)
AIR_E
tf <- is.na(AIR_E)
AIRE <- AIR_E[!tf] # removes NA values
AIRE
summary(AIRE)

# Quantile Quantile Plot
plot(ecdf(AIR_E), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(AIR_E); qqline(AIR_E)
x <- seq(9,97,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

# More on qq plots
plot(ecdf(EPI_2010data$AIR_E),do.points=FALSE,verticals = TRUE)
plot(ecdf(EPI_2010data$AIR_E),do.points=TRUE,verticals = TRUE) # points are visible
par(pty="s")
qqnorm(EPI_2010data$AIR_E)
qqline(EPI_2010data$AIR_E) # adding the line on the Q-Q plot
x <- seq(1,96,1)
x
x2 <-seq(1,96,2)
x2
x2 <-seq(1,96,2)
x2
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)

# BoxPlot comparison
boxplot(EPI_2010data$EPI,EPI_2010data$AIR_E)



# Comparison of EPI and WATER_E
# WATER_E Data Set (& removed NA values)
WATER_E
tf <- is.na(WATER_E)
WATERE <- WATER_E[!tf] # removes NA values
WATERE
summary(WATERE)

# Quantile Quantile Plot
plot(ecdf(WATER_E), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(WATER_E); qqline(WATER_E)
x <- seq(9,97,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

# More on qq plots
plot(ecdf(EPI_2010data$WATER_E),do.points=FALSE,verticals = TRUE)
plot(ecdf(EPI_2010data$WATER_E),do.points=TRUE,verticals = TRUE) # points are visible
par(pty="s")
qqnorm(EPI_2010data$WATER_E)
qqline(EPI_2010data$WATER_E) # adding the line on the Q-Q plot
x <- seq(1,96,1)
x
x2 <-seq(1,96,2)
x2
x2 <-seq(1,96,2)
x2
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)

# BoxPlot comparison
boxplot(EPI_2010data$EPI,EPI_2010data$WATER_E)



# Comparison of EPI and BIODIVERSITY
# BIODIVERSITY Data Set (& removed NA values)
BIODIVERSITY
tf <- is.na(BIODIVERSITY)
BIO <- BIODIVERSITY[!tf] # removes NA values
BIO
summary(BIO)

# Quantile Quantile Plot
plot(ecdf(BIODIVERSITY), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(BIODIVERSITY); qqline(BIODIVERSITY)
x <- seq(9,97,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

# More on qq plots
plot(ecdf(EPI_2010data$BIODIVERSITY),do.points=FALSE,verticals = TRUE)
plot(ecdf(EPI_2010data$BIODIVERSITY),do.points=TRUE,verticals = TRUE) # points are visible
par(pty="s")
qqnorm(EPI_2010data$BIODIVERSITY)
qqline(EPI_2010data$BIODIVERSITY) # adding the line on the Q-Q plot
x <- seq(1,96,1)
x
x2 <-seq(1,96,2)
x2
x2 <-seq(1,96,2)
x2
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)

# BoxPlot comparison
boxplot(EPI_2010data$EPI,EPI_2010data$BIODIVERSITY)


# Extra BoxPlot Comparisons
boxplot(EPI_2010data$AIR_H,EPI_2010data$AIR_E)
boxplot(EPI_2010data$WATER_H,EPI_2010data$WATER_E)



# GG plots
