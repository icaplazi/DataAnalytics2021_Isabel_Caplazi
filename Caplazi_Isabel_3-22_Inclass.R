############# USArrests Data Set #############
data("USArrests") # US arrest data set

View(USArrests) # opens data in another tab
states = row.names(USArrests)
states # reads off state names

names(USArrests) # column names in data set

apply(USArrests, 2, mean) # applies mean function to each column/row of data

apply(USArrests, 2, var) # variances of columns

# scales variables to have a mean of 0
# TRUE = stdev of 1
pr.out = prcomp(USArrests, scale = TRUE)
names(pr.out)

pr.out$center # mean before scale
pr.out$scale # stdev before scale

pr.out$rotation # displays loadings
dim(pr.out$x)

biplot(pr.out, scale = 0) # arrows represent loadings

pr.out$sdev # stdev of principal component
pr.var = pr.out$sdev^2 # principal component variance

pve = pr.var/sum(pr.var)
pve # proportion of variance by each component


############ IRIS DATA SET ############
data("iris") # iris data set
head(iris) # first 6 points

irisdata1 <- iris[,1:4]
irisdata1 # columns 1-4 of iris data

help("princomp")

# cor indicates correlation matrix (no constant variables)
principal_comp <- princomp(irisdata1, cor = TRUE, score = TRUE)
summary(principal_comp) # 4 different features

plot(principal_comp)
plot(principal_comp, type = "l") # line plot
biplot(principal_comp)
