#input data actor.rds
#output data actor_cluster.rds

library(data.table)
library(cluster)
#read in raw data
setwd("/2 ADA/Project/data")
load(file="actor.rds")

actor3<-actor2[,c(1,2,4)]
setnames(actor3,old="rank",new="ranklag1")
actor3$year=actor3$year+1

actor4<-actor2[,c(1,2,4)]
setnames(actor4,old="rank",new="ranklag2")
actor4$year=actor4$year+2

actor5<-merge(actor2,actor3,by=c("actor", "year"),all.x=TRUE)
actor6<-merge(actor5,actor4,by=c("actor", "year"),all.x=TRUE)

actor6[is.na(actor6)] <- 13

actor7<-actor6[,-c(1,2,3,5,6,7)]
actor8<-scale(actor7)

# Determine number of clusters
#wss <- (nrow(actor8)-1)*sum(apply(actor8,2,var))
#for (i in 2:539988) wss[i] <- sum(kmeans(actor8, centers=i)$withinss)
#plot(1:539988, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 

# K-Means Cluster Analysis
fit <- kmeans(actor8, 5) # 5 cluster solution
# get cluster means
aggregate(actor7,by=list(fit$cluster),FUN=mean)
# append cluster assignment
actor9 <- data.frame(actor6, fit$cluster)


#fit1 <- kmeans(actor8, 6)
#aggregate(actor7,by=list(fit1$cluster),FUN=mean)

#fit2 <- kmeans(actor8, 4)
#aggregate(actor7,by=list(fit2$cluster),FUN=mean)


#fit1 <- pam(actor8, 5) # 5 cluster solution
#library(fpc)
#fit <- pamk(actor8, krange=3:6) # 5 cluster solution

#d <- dist(actor8, method = "euclidean") # distance matrix
#fit.h <- hclust(d, method="ward")
#plot(fit) # display dendogram
#groups <- cutree(fit.h, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
#rect.hclust(fit.h, k=5, border="red") 
actor6[is.na(actor6)] <- 13
actor60<-actor6
for(i in 1:89999) {
  if (actor60$ranklag1[i]<-13){actor60$ranklag1[i]=actor60$ranklag2[i]}
}
for(i in 1:89999) {
  if (actor60$ranklag2[i]<-13){actor60$ranklag2[i]=actor60$ranklag1[i]}
}

actor70<-actor60[,-c(1,2,5,6,7)]
actor80<-scale(actor70)

# K-Means Cluster Analysis
fit0 <- kmeans(actor80, 5) # 5 cluster solution
# get cluster means
aggregate(actor70,by=list(fit0$cluster),FUN=mean)
# append cluster assignment
actor90 <- data.frame(actor60, fit0$cluster)

save(actor9, file="actor_cluster.rds")
