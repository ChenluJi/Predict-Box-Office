library(neuralnet)
sign.neural<-read.csv("/Users/JJason/Documents/Study/summer project/Predict-Box-Office/data/revenue.sign.neural.csv")
sign.neural<-subset(sign.neural,select = -studio)

set.seed(5)
index = sample(c(1:2408),1820)
train = sign.neural[index,]
validate = sign.neural[-index,]

train<-model.matrix(~Sign + genres + num_voted_users + num_user_for_reviews + imdb_score + 
                  aspect_ratio + movie_facebook_likes + Type + Year + Runtime + 
                  Budget + budget.diff + Actor1 + Actor2 + Actor3 + Actor4 + 
                  Actor5 + Actor6,data=train)

validate = model.matrix(~Sign + genres + num_voted_users + num_user_for_reviews + imdb_score + 
                          aspect_ratio + movie_facebook_likes + Type + Year + Runtime + 
                          Budget + budget.diff + Actor1 + Actor2 + Actor3 + Actor4 + 
                          Actor5 + Actor6,data=validate)

#n1<-colnames(m)

#f1<-as.formula(paste("Sign~",paste(n1[!n1 %in% "Sign"],collapse = " + ")))
#f1
neuralnet.sign<-neuralnet(Sign ~genresAdventure + genresAnimation + genresBiography + 
                            genresComedy + genresCrime + genresDocumentary + genresDrama + 
                            genresFamily + genresFantasy + genresFilmNoir + genresHorror + 
                            genresMusical + genresMystery + genresRomance + genresSciFi + 
                            genresThriller + genresWestern + num_voted_users + num_user_for_reviews + 
                            imdb_score + aspect_ratio + movie_facebook_likes + TypeApproved + 
                            TypeG + TypeGP + TypeM + TypeNC17 + TypeNotRated + TypePassed + 
                            TypePG + TypePG13 + TypeR + TypeUnrated + TypeX + Year + 
                            Runtime + Budget + budget.diff + Actor1 + Actor2 + Actor3 + 
                            Actor4 + Actor5 + Actor6,data=train,hidden = c(5,3),lifesign ="none")

result = compute(neuralnet.sign, validate[,c(-1,-2)])$net.result
result[which(result >= 0.5)] = 1
result[which(result < 0.5)] = 0
sum(result == validate[,2])/588

predict = neuralnet.sign$net.result[[1]]
predict[which(predict >= 0.5)] = 1
predict[which(predict < 0.5)] = 0
sum(train[,2] == predict)/1820


revenue.neural<-read.csv("/Users/JJason/Documents/Study/summer project/Predict-Box-Office/data/revenue.neural.csv")
revenue.neural<-subset(revenue.neural,select = -studio)
revenue.neural$Revenue[which(revenue.neural$Revenue - revenue.neural$Budget >= 0)] = 1
revenue.neural$Revenue[which(revenue.neural$Revenue != 1)] = 0

set.seed(1)
index = sample(which(revenue.neural$Revenue == 1),500)
index1 = which(revenue.neural$Revenue == 0)

#index = sample(c(1:2408),1820)
train = revenue.neural[c(index,index1),]
validate = revenue.neural[-c(index,index1),] 





#for(m in 1:nrow(revenue.neural)){
#  if (revenue.neural$Revenue[m] <= 1000000)
#    revenue.neural$Revenue[m] = 0
#  else if (revenue.neural$Revenue[m] > 1000000 & revenue.neural$Revenue[m] <= 10000000)
#    revenue.neural$Revenue[m] = 1
#  else if (revenue.neural$Revenue[m] > 10000000 & revenue.neural$Revenue[m] <= 20000000)
#    revenue.neural$Revenue[m] = 2
#  else if (revenue.neural$Revenue[m] > 20000000 & revenue.neural$Revenue[m] <= 40000000)
#    revenue.neural$Revenue[m] = 3
#  else if (revenue.neural$Revenue[m] > 40000000 & revenue.neural$Revenue[m] <= 65000000)
#    revenue.neural$Revenue[m] = 4
#  else if (revenue.neural$Revenue[m] > 64000000 & revenue.neural$Revenue[m] <= 100000000)
#    revenue.neural$Revenue[m] = 5
#  else if (revenue.neural$Revenue[m] > 100000000 & revenue.neural$Revenue[m] <= 150000000)
#    revenue.neural$Revenue[m] = 6
#  else if (revenue.neural$Revenue[m] > 150000000 & revenue.neural$Revenue[m] <= 200000000)
#    revenue.neural$Revenue[m] = 7
#  else
#    revenue.neural$Revenue[m] = 8
#}



#train$Revenue = (train$Revenue-min(train$Revenue))/(max(train$Revenue)-min(train$Revenue))

train<-model.matrix(~Revenue  + num_voted_users + num_user_for_reviews + imdb_score + 
                      aspect_ratio + movie_facebook_likes + Type + Year + Runtime + 
                      Budget + budget.diff + Actor1 + Actor2 + Actor3 + Actor4 + 
                      Actor5 + Actor6 + genres,data=train)

validate<-model.matrix(~Revenue  + num_voted_users + num_user_for_reviews + imdb_score + 
                         aspect_ratio + movie_facebook_likes + Type + Year + Runtime + 
                         Budget + budget.diff + Actor1 + Actor2 + Actor3 + Actor4 + 
                         Actor5 + Actor6 + genres,data=validate)


neuralnet.revenue<-neuralnet(Revenue ~ 
                               genresAdventure + genresAnimation + genresBiography + 
                               genresComedy + genresCrime + genresDocumentary + genresDrama + 
                               genresFamily + genresFantasy + genresFilmNoir + genresHorror + 
                               genresMusical + genresMystery + genresRomance + genresSciFi + 
                               genresThriller + genresWestern + num_voted_users + num_user_for_reviews + 
                               imdb_score + aspect_ratio + movie_facebook_likes + TypeApproved + 
                               TypeG + TypeGP + TypeM + TypeNC17 + TypeNotRated + TypePassed + 
                               TypePG + TypePG13 + TypeR + TypeUnrated + TypeX + Year + 
                               Runtime + Budget + budget.diff + Actor1 + Actor2 + Actor3 + 
                               Actor4 + Actor5 + Actor6,data=train,hidden = c(5,3),lifesign ="none")
predict = neuralnet.revenue$net.result[[1]]
predict[which(predict >= 0.5)] = 1
predict[which(predict < 0.5)] = 0
sum(train[,2] == predict)/1016

result = compute(neuralnet.revenue, validate[,-c(1,2)])$net.result
result[which(result >= 0.5)] = 1
result[which(result < 0.5)] = 0
sum(result == validate[,2])/1392

plot(neuralnet.revenue)







