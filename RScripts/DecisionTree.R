library(tree)
library(rpart)
library(rpart.plot)
# written by the same authors, provides better control
#Reading data, creating depending variable as qualitative for classification tree, then forming a dataframe
Bike=read.csv("Bikebuyer.csv", header=T)
Buyer=ifelse (Bike$BikeBuyer==0," No"," Yes ")
mydata =data.frame(Bike,Buyer) 
str(mydata) 

#Tree funtion for classification, sicne dependant variable is not a vector, classification tree is formed.
mymodel = Buyer ~ MaritalStatus + YearlyIncome + TotalChildren + ChildrenAtHome + HouseOwnerFlag + NumberCarsOwned + Age
mytree = rpart(mymodel, data=mydata, method="class", parms=list(split="gini"), control=rpart.control(minsplit = 900, minbucket = 300, cp = 0.01))
#split can be gini or information (not entropy)

#visualizing the tree
mytree
#the split criterion,the number of observations in that branch, the deviance, the overall prediction
#for the branch (Yes or No), and the fraction of observations in that branch that take on values of Yes and No

plot(mytree)
text(mytree, cex=0.6,pretty=0)
# (* denotes terminal node)
#not a good plot, so we try rpart.plot

rpart.plot(mytree, cex=0.5)


#Using the tree for prediction

#same seed will ensure that same results for two people
set.seed (2)
length=nrow(mydata)
train.length=round(length*0.70)
train=sample (1: nrow(mydata), train.length)
test=mydata [-train ,]
train_data = mydata[train,]
Buyer.test=Buyer[-train ]
#here mytree1 built using partitioned train data; tested using test data
mytree1 = rpart(mymodel, data=train_data, method="class", parms=list(split="gini"), control=rpart.control(minsplit = 900, minbucket = 300, cp = 0.01))
tree.pred=predict (mytree1,test,type="class")
table(tree.pred,Buyer.test)

