data<-read.csv("Data Merged( Kaggle TMDB Studio, no NAs).csv")
library(vcd)
library(grid)
library(plyr)
head(data)

attach(data)
studio
count.dir<-count(director_name)
class(count.dir)
dir.mat<-as.matrix(count.dir)
as.numeric(dir.mat[,2])
freq.dir<-as.numeric(dir.mat[,2])
big5.data<-data[which(freq.dir>5),]

#dir.mat[?,1]可以用来取导演的名字
#which(freq.dir>5)用来衡量哪些index表示导演

data.ts<-data[director_name=="Tom Shadyac",]
data.ts[,c(2,3,29,33,35)]

data.mm<-data[director_name=="Mike Mitchell",]
data.mm[,c(2,3,29,33,35)]
#as I wish
data.djc<-data[director_name=="Adam Shankman",]
data.djc[,c(2,3,29,33,35)]

data.am<-data[director_name=="Adam McKay",]
data.am[,c(2,3,29,33,35)]
#too old
data.ah<-data[director_name=="Alfred Hitchcock",]
data.ah[,c(2,3,29,33,35)]

data.af<-data[director_name=="Andy Fickman",]
data.af[,c(2,3,29,33,35)]

data.gr<-data[director_name=="Guy Ritchie",]
data.gr[,c(2,3,29,33,35)]


data.mb<-data[director_name=="Michael Bay",]
data.mb[,c(2,3,29,33,35)]


data.mm<-data[director_name=="Michael Mann",]
data.mm[,c(2,3,29,33,35)]

#as i wish
data.re<-data[director_name=="Roland Emmerich",]
data.re[,c(2,3,29,33,35)]


######budget
budget.1996<-mean(data[which(Year==1996),]$Budget)
budget.2000<-mean(data[which(Year==2000),]$Budget)
budget.2004<-mean(data[which(Year==2004),]$Budget)
budget.2008<-mean(data[which(Year==2008),]$Budget)
budget.2012<-mean(data[which(Year==2012),]$Budget)
budget.2013<-mean(data[which(Year==2013),]$Budget)
budget.2014<-mean(data[which(Year==2014),]$Budget)
budget.2015<-mean(data[which(Year==2015),]$Budget)
budget.years<-c(budget.1996,budget.2000,budget.2004,budget.2008,budget.2012,budget.2013,budget.2014,budget.2015)
plot(c(1996,2000,2004,2008,2012,2013,2014,2015),budget.years)
#######Revenue
revenue.1996<-mean(data[which(Year==1996),]$Revenue)
revenue.2000<-mean(data[which(Year==2000),]$Revenue)
revenue.2004<-mean(data[which(Year==2004),]$Revenue)
revenue.2008<-mean(data[which(Year==2008),]$Revenue)
revenue.2012<-mean(data[which(Year==2012),]$Revenue)
revenue.2013<-mean(data[which(Year==2013),]$Revenue)
revenue.2014<-mean(data[which(Year==2014),]$Revenue)
revenue.2015<-mean(data[which(Year==2015),]$Revenue)
revenue.years<-c(revenue.1996,revenue.2000,revenue.2004,revenue.2008,revenue.2012,revenue.2013,revenue.2014,revenue.2015)
plot(c(1996,2000,2004,2008,2012,2013,2014,2015),revenue.years)
