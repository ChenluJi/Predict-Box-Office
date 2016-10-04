# input data rank1.csv
# output data actor.rds
setwd("/Users/S7/Dropbox/5291 ADA/Project/data")

library(plyr)
library(sqldf)

rank=read.csv("rank1.csv",header=T)
rank1<-rank[,c(1:6)]

rank2<-rank[,c(7:12)]
rank3<-rank[,c(13:18)]
rank4<-rank[,c(19:24)]
rank5<-rank[,c(25:30)]
rank6<-rank[,c(31:36)]
names(rank1)
names(rank2)
names(rank3)
names(rank4)
names(rank5)
names(rank6)
setnames(rank2,old=c("title.year.1", "actor.1" ,     "rank.1"   ,    "high.1"    ,  "med.1"      ,  "low.1"),
         new=c("title.year" ,"actor"   ,   "rank"    ,  "high"   ,    "med"   ,     "low"))
setnames(rank3,old=c("title.year.2" ,"actor.2"   ,   "rank.2"     ,  "high.2"   ,    "med.2"  ,      "low.2"),
         new=c("title.year" ,"actor"   ,   "rank"    ,  "high"   ,    "med"   ,     "low"))
setnames(rank4,old=c( "title.year.3", "actor.3"  ,    "rank.3"  ,    "high.3"    ,   "med.3"  ,      "low.3"),
         new=c("title.year" ,"actor"   ,   "rank"    ,  "high"   ,    "med"   ,     "low"))
setnames(rank5,old=c("title.year.4","actor.4"   ,   "rank.4"   ,    "high.4"    ,   "med.4"    ,    "low.4" ),
         new=c("title.year" ,"actor"   ,   "rank"    ,  "high"   ,    "med"   ,     "low"))
setnames(rank6,old=c("title.year.5" ,"actor.5"   ,  "rank.5"     ,  "high.5"   ,   "med.5"     ,   "low.5"),
         new=c("title.year" ,"actor"   ,   "rank"    ,  "high"   ,    "med"   ,     "low"))

actor<-rbind(rank1,rank2, rank3, rank4, rank5, rank6)
names(actor)

setnames(actor,old=c("title.year"),new=c("year"))
actor1<-sqldf("select actor,year, count(*) as count, sum(rank)/count(*) as rank,sum(high) as high,sum(med) as med,sum(low) as low
               from actor
              group by actor, year")
actor1$high.rate<-actor1$high/actor1$count
actor1$med.rate<-actor1$med/actor1$count
actor1$low.rate<-actor1$low/actor1$count
actor2<-actor1[-c(1:86,27838:27925,52776:52816),]

save(actor2, file="actor.rds")

#load(file="actor.rds")
