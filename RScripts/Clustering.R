
Domi<-read.csv("Dominick.csv", header=T)
Domistd=scale(Domi, center=TRUE,scale=TRUE)
#z-score normalisation
Domistd

#K initialisd to the highest range
K <- (nrow(Domistd)-1)*sum(apply(Domistd,2,var))
for (i in 2:15) K[i] <- sum(kmeans(Domistd, centers=i)$withinss)
plot(1:15, K, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares") 

cl1 <- kmeans(Domistd, 4)
cl1
