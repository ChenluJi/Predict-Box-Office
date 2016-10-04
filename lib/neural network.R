##########neural network for revenue sign##########
library(neuralnet)
sign.neural<-read.csv("~/Documents/ADA/revenue.sign.neural.csv")
sign.neural<-subset(sign.neural,select = -studio)
n<-names(sign.neural)
f<-as.formula(paste("Sign~",paste(n[!n %in% "Sign"],collapse = " + ")))
f
m<-model.matrix(~Sign + genres + num_voted_users + num_user_for_reviews + imdb_score + 
                  aspect_ratio + movie_facebook_likes + Type + Year + Runtime + 
                  Budget + budget.diff + Actor1 + Actor2 + Actor3 + Actor4 + 
                  Actor5 + Actor6,data=sign.neural)
n1<-colnames(m)
f1<-as.formula(paste("Sign~",paste(n1[!n1 %in% "Sign"],collapse = " + ")))
f1
neuralnet.sign<-neuralnet(Sign ~genresAdventure + genresAnimation + genresBiography + 
                            genresComedy + genresCrime + genresDocumentary + genresDrama + 
                            genresFamily + genresFantasy + genresFilmNoir + genresHorror + 
                            genresMusical + genresMystery + genresRomance + genresSciFi + 
                            genresThriller + genresWestern + num_voted_users + num_user_for_reviews + 
                            imdb_score + aspect_ratio + movie_facebook_likes + TypeApproved + 
                            TypeG + TypeGP + TypeM + TypeNC17 + TypeNotRated + TypePassed + 
                            TypePG + TypePG13 + TypeR + TypeUnrated + TypeX + Year + 
                            Runtime + Budget + budget.diff + Actor1 + Actor2 + Actor3 + 
                            Actor4 + Actor5 + Actor6,data=m,hidden = c(5,3),lifesign ="none")
neuralnet.sign$net.result
net.result<-rep(0,length(sign.neural$Sign))
for(i in 1:length(net.result))
{
  if(unlist(neuralnet.sign$net.result)[i]>=0.5) net.result[i]=1
}
mse.nn<-0
for(i in 1:length(net.result))
{
  mse.nn<-mse.nn+(net.result[i]-sign.neural$Sign[i])^2
}
plot(neuralnet.sign)






##########neural network for revenue##########
library(neuralnet)
revenue.neural<-read.csv("~/Documents/ADA/revenue.neural.csv")
revenue.neural<-subset(revenue.neural,select = -c(studio,genres))
n<-names(revenue.neural)
f<-as.formula(paste("Revenue~",paste(n[!n %in% "Revenue"],collapse = " + ")))
f
m<-model.matrix(~Revenue  + num_voted_users + num_user_for_reviews + 
                  imdb_score + aspect_ratio + movie_facebook_likes + Type + 
                  Year + Runtime + Budget + budget.diff + Actor1 + Actor2 + 
                  Actor3 + Actor4 + Actor5 + Actor6,data=revenue.neural)
n1<-colnames(m)
f1<-as.formula(paste("Revenue~",paste(n1[!n1 %in% "Revenue"],collapse = " + ")))
f1
neuralnet.revenue<-neuralnet(Revenue ~ 
                              TypeApproved + 
                               TypeG + TypeGP + TypeM + TypeNC17 + TypeNotRated + TypePassed + 
                               TypePG + TypePG13 + TypeR + TypeUnrated + TypeX + Year + 
                               Runtime + Budget + budget.diff + Actor1 + Actor2 + Actor3 + 
                               Actor4 + Actor5 + Actor6,data=m,hidden = 1,lifesign ="none",stepmax=1e6)
neuralnet.sign$net.result
net.result<-rep(0,length(sign.neural$Sign))
for(i in 1:length(net.result))
{
  if(unlist(neuralnet.sign$net.result)[i]>=0.5) net.result[i]=1
}
mse.nn<-0
for(i in 1:length(net.result))
{
  mse.nn<-mse.nn+(net.result[i]-sign.neural$Sign[i])^2
}
plot(neuralnet.revenue)
