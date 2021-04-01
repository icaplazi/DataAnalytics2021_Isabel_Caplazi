data(swiss)
View(swiss)
require(rpart)
Swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(Swiss_rpart) # try some different plot options
text(Swiss_rpart) # try some different text options

require(party)
treeSwiss<-ctree(Infant.Mortality ~ ., data=swiss)
plot(treeSwiss)

cforest(Infant.Mortality ~ ., data=swiss, controls=cforest_control(mtry=2, mincriterion=0))

treeFert<-ctree(Fertility ~ Agriculture + Education + Catholic, data = swiss)

cforest(Fertility ~ Agriculture + Education + Catholic, data = swiss, controls=cforest_control(mtry=2, mincriterion=0))
# look at help info, vary parameters.

library(tree)
tr <- tree(Infant.Mortality ~ ., data=swiss)
tr
tr$frame
plot(tr)
text(tr)

View(kyphosis)
colnames(kyphosis)
fitK <- ctree(Kyphosis ~ Age + Number + Start, data=kyphosis)
plot(fitK, main="Conditional Inference Tree for Kyphosis")
plot(fitK, main="Conditional Inference Tree for Kyphosis",type="simple")
