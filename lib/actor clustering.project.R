#input data actor.rds
#output data actor_cluster.rds

library(data.table)
library(cluster)
#read in raw data



actor3<-actor2[,c(1,2,4)]
setnames(actor3,old="rank",new="ranklag1")
actor3$year=actor3$year+1

actor4<-actor2[,c(1,2,4)]
setnames(actor4,old="rank",new="ranklag2")
actor4$year=actor4$year+2

actor5<-merge(actor2,actor3,by=c("actor", "year"),all.x=TRUE)
actor6<-merge(actor5,actor4,by=c("actor", "year"),all.x=TRUE)




actor6[is.na(actor6)] <- 13
actor60<-actor6

actor70<-actor60[,-c(1,2,5,6,7)]
actor80<-scale(actor70)

# K-Means Cluster Analysis
fit0 <- kmeans(actor80, 5) # 5 cluster solution
# get cluster means
aggregate(actor70,by=list(fit0$cluster),FUN=mean)

# virtualization
library(fpc)
plotcluster(actor70,fit0$cluster, main = "Cluster plot ")
plotcluster(actor80,fit0$cluster, main = "Cluster plot for the scaled data ")

summary(actor60)
