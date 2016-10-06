library(neuralnet)
sign.neural<-read.csv("/Users/JJason/Documents/Study/summer project/Predict-Box-Office/data/revenue.sign.neural.csv")
sign.neural<-subset(sign.neural,select = -studio)

set.seed(5)
index = sample(c(1:2408),1820)
train = sign.neural[index,]
validate = sign.neural[-index,]

sign.neural$num_voted_users = (sign.neural$num_voted_users - mean(sign.neural$num_voted_users))/sd(sign.neural$num_voted_users)
sign.neural$num_user_for_reviews = (sign.neural$num_user_for_reviews - mean(sign.neural$num_user_for_reviews))/sd(sign.neural$num_user_for_reviews)
sign.neural$imdb_score = (sign.neural$imdb_score - mean(sign.neural$imdb_score))/sd(sign.neural$imdb_score)
sign.neural$aspect_ratio = (sign.neural$aspect_ratio - mean(sign.neural$aspect_ratio))/sd(sign.neural$aspect_ratio)
sign.neural$movie_facebook_likes = (sign.neural$movie_facebook_likes - mean(sign.neural$movie_facebook_likes))/sd(sign.neural$movie_facebook_likes)
sign.neural$Runtime = (sign.neural$Runtime - mean(sign.neural$Runtime))/sd(sign.neural$Runtime)
sign.neural$Budget = (sign.neural$Budget - mean(sign.neural$Budget))/sd(sign.neural$Budget)
sign.neural$budget.diff = (sign.neural$budget.diff - mean(sign.neural$budget.diff))/sd(sign.neural$budget.diff)
sign.neural$Year = sign.neural$Year - mean(sign.neural$Year)

train<-model.matrix(~Sign + genres + num_voted_users + imdb_score + 
                  aspect_ratio + movie_facebook_likes + Type + Year + Runtime + 
                  Budget + Actor1,data=train)

validate = model.matrix(~Sign + genres + num_voted_users + imdb_score + 
                          aspect_ratio + movie_facebook_likes + Type + Year + Runtime + 
                          Budget + Actor1,data=validate)

#n1<-colnames(m)

#f1<-as.formula(paste("Sign~",paste(n1[!n1 %in% "Sign"],collapse = " + ")))
#f1
neuralnet.sign<-neuralnet(Sign ~genresAdventure + genresAnimation + genresBiography + 
                            genresComedy + genresCrime + genresDocumentary + genresDrama + 
                            genresFamily + genresFantasy + genresFilmNoir + genresHorror + 
                            genresMusical + genresMystery + genresRomance + genresSciFi + 
                            genresThriller + genresWestern + num_voted_users + 
                            imdb_score + aspect_ratio + movie_facebook_likes + TypeApproved + 
                            TypeG + TypeGP + TypeM + TypeNC17 + TypeNotRated + TypePassed + 
                            TypePG + TypePG13 + TypeR + TypeUnrated + TypeX + Year + 
                            Runtime + Budget + Actor1,data=train,hidden = c(5,3),lifesign ="full", threshold = 0.1)


predict = neuralnet.sign$net.result[[1]]
predict[which(predict >= 0.5)] = 1
predict[which(predict < 0.5)] = 0
sum(train[,2] == predict)/1820

result = compute(neuralnet.sign, validate[,c(-1,-2)])$net.result
result[which(result >= 0.5)] = 1
result[which(result < 0.5)] = 0
sum(result == validate[,2])/588

revenue.neural<-read.csv("/Users/JJason/Documents/Study/summer project/Predict-Box-Office/data/revenue.neural.csv")
revenue.neural<-subset(revenue.neural,select = -studio)

revenue.neural$Revenue0 = 0
revenue.neural$Revenue1 = 0
revenue.neural$Revenue2 = 0
revenue.neural$Revenue3 = 0
revenue.neural$Revenue4 = 0
revenue.neural$Revenue5 = 0
revenue.neural$Revenue6 = 0
revenue.neural$Revenue7 = 0
revenue.neural$Revenue8 = 0

for(m in 1:nrow(revenue.neural)){
  if (revenue.neural$Revenue[m] <= 1000000)
    revenue.neural$Revenue0[m] = 1
  else if (revenue.neural$Revenue[m] > 1000000 & revenue.neural$Revenue[m] <= 10000000)
    revenue.neural$Revenue1[m] = 1
  else if (revenue.neural$Revenue[m] > 10000000 & revenue.neural$Revenue[m] <= 20000000)
    revenue.neural$Revenue2[m] = 1
  else if (revenue.neural$Revenue[m] > 20000000 & revenue.neural$Revenue[m] <= 40000000)
    revenue.neural$Revenue3[m] = 1
  else if (revenue.neural$Revenue[m] > 40000000 & revenue.neural$Revenue[m] <= 65000000)
    revenue.neural$Revenue4[m] = 1
  else if (revenue.neural$Revenue[m] > 64000000 & revenue.neural$Revenue[m] <= 100000000)
    revenue.neural$Revenue5[m] = 1
  else if (revenue.neural$Revenue[m] > 100000000 & revenue.neural$Revenue[m] <= 150000000)
    revenue.neural$Revenue6[m] = 1
  else if (revenue.neural$Revenue[m] > 150000000 & revenue.neural$Revenue[m] <= 200000000)
    revenue.neural$Revenue7[m] = 1
  else
    revenue.neural$Revenue8[m] = 1
}

revenue.neural$num_voted_users = (revenue.neural$num_voted_users - mean(revenue.neural$num_voted_users))/sd(revenue.neural$num_voted_users)
revenue.neural$num_user_for_reviews = (revenue.neural$num_user_for_reviews - mean(revenue.neural$num_user_for_reviews))/sd(revenue.neural$num_user_for_reviews)
revenue.neural$imdb_score = (revenue.neural$imdb_score - mean(revenue.neural$imdb_score))/sd(revenue.neural$imdb_score)
revenue.neural$aspect_ratio = (revenue.neural$aspect_ratio - mean(revenue.neural$aspect_ratio))/sd(revenue.neural$aspect_ratio)
revenue.neural$movie_facebook_likes = (revenue.neural$movie_facebook_likes - mean(revenue.neural$movie_facebook_likes))/sd(revenue.neural$movie_facebook_likes)
revenue.neural$Runtime = (revenue.neural$Runtime - mean(revenue.neural$Runtime))/sd(revenue.neural$Runtime)
revenue.neural$Budget = (revenue.neural$Budget - mean(revenue.neural$Budget))/sd(revenue.neural$Budget)
revenue.neural$budget.diff = (revenue.neural$budget.diff - mean(revenue.neural$budget.diff))/sd(revenue.neural$budget.diff)
revenue.neural$Year = revenue.neural$Year - mean(revenue.neural$Year)

#cor = cor(revenue.neural[,c(2,3,4,5,6,9,10,11,13,14,15,16,17,18)],revenue.neural[,c(2,3,4,5,6,9,10,11,13,14,15,16,17,18)])

set.seed(5)


index = sample(c(1:2408),1820)
train = revenue.neural[index,]
validate = revenue.neural[-index,] 

train<-model.matrix(~Revenue0 + Revenue1 + Revenue2 + Revenue3 + Revenue4 + Revenue5 + Revenue6 + Revenue7 + Revenue8  + num_voted_users + imdb_score + 
                      aspect_ratio + movie_facebook_likes + Type + Year + Runtime + 
                      Budget + Actor1 + Actor2 + Actor3 + Actor4 + 
                      Actor5 + Actor6 + genres,data=train)

validate<-model.matrix(~Revenue0 + Revenue1 + Revenue2 + Revenue3 + Revenue4 + Revenue5 + Revenue6 + Revenue7 + Revenue8  + num_voted_users + imdb_score + 
                         aspect_ratio + movie_facebook_likes + Type + Year + Runtime + 
                         Budget + Actor1 + genres,data=validate)


neuralnet.revenue<-neuralnet(Revenue0 + Revenue1 + Revenue2 + Revenue3 + Revenue4 + Revenue5 + Revenue6 + Revenue7 + Revenue8 ~ 
                               num_voted_users + 
                               imdb_score + aspect_ratio + movie_facebook_likes + TypeApproved + 
                               TypeG + TypeGP + TypeM + TypeNC17 + TypeNotRated + TypePassed + 
                               TypePG + TypePG13 + TypeR + TypeUnrated + TypeX + Year + 
                               Runtime + Budget + Actor1 + genresAdventure + genresAnimation + genresBiography + 
                               genresComedy + genresCrime + genresDocumentary + genresDrama + 
                               genresFamily + genresFantasy + genresFilmNoir + genresHorror + 
                               genresMusical + genresMystery + genresRomance + genresSciFi + 
                               genresThriller + genresWestern,data=train,hidden = c(9,9),lifesign ="full",threshold = 0.8)

predict = neuralnet.revenue$net.result[[1]]
aa = apply(predict,1,max)
predict = predict/aa
predict[predict!=1] = 0

error = predict - train[,c(2:10)]
b = apply(abs(error),1,sum)
table(b)/1820

#predict[which(predict >= 0.5)] = 1
#predict[which(predict < 0.5)] = 0
#sum(train[,2] == predict)/1

result = compute(neuralnet.revenue, validate[,-c(1:10)])$net.result
aa = apply(result,1,max)
result = result/aa
result[result!=1] = 0

error = result - validate[,c(2:10)]
b = apply(abs(error),1,sum)
table(b)/588



#dev.new(width=100000, height=10)
#par(pin=c(6,3))
#grid.newpage()
#viewport(x=0.5, y=0.5,w=unit(15, "inches"), h=unit(7, "inches"))
plot(neuralnet.revenue)



#cor = cor(revenue.neural[,c(2,3,4,5,6,9,10,11,13,14,15,16,17,18)],revenue.neural[,c(2,3,4,5,6,9,10,11,13,14,15,16,17,18)])

revenue.neural<-read.csv("/Users/JJason/Documents/Study/summer project/Predict-Box-Office/data/revenue.neural.csv")
revenue.neural<-subset(revenue.neural,select = -studio)
revenue.neural$Revenue[which(revenue.neural$Revenue - revenue.neural$Budget >= 0)] = 1
revenue.neural$Revenue[which(revenue.neural$Revenue != 1)] = 0

revenue.neural$num_voted_users = (revenue.neural$num_voted_users - mean(revenue.neural$num_voted_users))/sd(revenue.neural$num_voted_users)
revenue.neural$num_user_for_reviews = (revenue.neural$num_user_for_reviews - mean(revenue.neural$num_user_for_reviews))/sd(revenue.neural$num_user_for_reviews)
revenue.neural$imdb_score = (revenue.neural$imdb_score - mean(revenue.neural$imdb_score))/sd(revenue.neural$imdb_score)
revenue.neural$aspect_ratio = (revenue.neural$aspect_ratio - mean(revenue.neural$aspect_ratio))/sd(revenue.neural$aspect_ratio)
revenue.neural$movie_facebook_likes = (revenue.neural$movie_facebook_likes - mean(revenue.neural$movie_facebook_likes))/sd(revenue.neural$movie_facebook_likes)
revenue.neural$Runtime = (revenue.neural$Runtime - mean(revenue.neural$Runtime))/sd(revenue.neural$Runtime)
revenue.neural$Budget = (revenue.neural$Budget - mean(revenue.neural$Budget))/sd(revenue.neural$Budget)
revenue.neural$budget.diff = (revenue.neural$budget.diff - mean(revenue.neural$budget.diff))/sd(revenue.neural$budget.diff)
revenue.neural$Year = revenue.neural$Year - mean(revenue.neural$Year)

set.seed(7)


index = sample(c(1:2408),1820)
train = revenue.neural[index,]
validate = revenue.neural[-index,] 

train<-model.matrix(~Revenue + num_voted_users + imdb_score + 
                      aspect_ratio + movie_facebook_likes + Type + Year + Runtime + 
                      Budget + Actor1 + genres,data=train)

validate<-model.matrix(~Revenue + num_voted_users + imdb_score + 
                         aspect_ratio + movie_facebook_likes + Type + Year + Runtime + 
                         Budget + Actor1 + genres,data=validate)


neuralnet.revenue<-neuralnet(Revenue ~ 
                               num_voted_users + 
                               imdb_score + aspect_ratio + movie_facebook_likes + TypeApproved + 
                               TypeG + TypeGP + TypeM + TypeNC17 + TypeNotRated + TypePassed + 
                               TypePG + TypePG13 + TypeR + TypeUnrated + TypeX + Year + 
                               Runtime + Budget + Actor1 + genresAdventure + genresAnimation + genresBiography + 
                               genresComedy + genresCrime + genresDocumentary + genresDrama + 
                               genresFamily + genresFantasy + genresFilmNoir + genresHorror + 
                               genresMusical + genresMystery + genresRomance + genresSciFi + 
                               genresThriller + genresWestern,data=train,hidden = c(9,9),lifesign ="full",threshold = 0.2)

predict = neuralnet.revenue$net.result[[1]]
predict[which(predict >= 0.5)] = 1
predict[which(predict < 0.5)] = 0
sum(train[,2] == predict)/1820

result = compute(neuralnet.revenue, validate[,c(-1,-2)])$net.result
result[which(result >= 0.5)] = 1
result[which(result < 0.5)] = 0
sum(result == validate[,2])/588