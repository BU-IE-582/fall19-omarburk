setwd("Z:/codes/R-Scripts/project")
bets<-fread("bets.csv")
matches<-fread("matches.csv")
stats<-fread("stats.csv")
goals<-fread("goals.csv")
booking<-fread("booking.csv")


matches$epoch<-as_datetime(matches$epoch)
matches_train<-subset(matches,matches$league_id==148&matches$epoch>=as.Date("2018-08-01")&matches$match_status=="Finished")

booking$time<-as.numeric(as.character(booking$time))
early_reds<-booking%>%filter(time<=45&card=="red card")
early_double_yellows=booking%>%filter(time<=45&card=="yellow card")%>%group_by(match_id,home_fault)%>%summarise(yellow_count=n())%>%filter(yellow_count==2&home_fault!="")
early_double_yellows2=booking%>%filter(time<=45&card=="yellow card")%>%group_by(match_id,away_fault)%>%summarise(yellow_count=n())%>%filter(yellow_count==2&away_fault!="")
late_much_goals<-goals%>%filter(grepl("90",goals$time))%>%group_by(match_id)%>%summarise(late_goal_count=n())%>%filter(late_goal_count>1)

matches_to_ecxlude<-c(early_reds$match_id,early_double_yellows$match_id,early_double_yellows2$match_id,late_much_goals$match_id)

matches$epoch<-as_datetime(matches$epoch)
matches_train<-subset(matches,matches$league_id==148&matches$epoch>=as.Date("2018-08-01")&matches$match_status=="Finished")
matches_train<-matches_train%>%filter(!match_id%in%matches_to_ecxlude)


bets_adjusted<-bets%>%group_by(match_id,odd_bookmakers)%>%mutate(latest_bet_time=max(odd_epoch))%>%filter(odd_epoch==latest_bet_time&variable%in%c("odd_1","odd_x","odd_2"))
mathces_train_bets<-merge(matches_train,bets_adjusted,by="match_id",all.x = T)
mathces_train_bets<-mathces_train_bets[!is.na(mathces_train_bets$value),]
mathces_train_bets$result=0
mathces_train_bets[mathces_train_bets$match_hometeam_score>mathces_train_bets$match_awayteam_score,'result']=1
mathces_train_bets[mathces_train_bets$match_hometeam_score<mathces_train_bets$match_awayteam_score,'result']=2
mathces_train_bets<-mathces_train_bets[,-c(2:17)]


library(reshape2)
ttt<-dcast(
  mathces_train_bets,
  match_id + result ~ odd_bookmakers + variable,
  value.var = 'value')




na_list<-sapply(ttt,function(x) sum(is.na(x)))
trusted_bookers<-unlist(na_list[na_list<10])


ttt<-ttt[,(names(trusted_bookers))]  #Train Data
####rps

rps<-function(p1,p0,p2,result){
  
  if(result==0){
    
    rps=(p1-0)^2+ (p0+p1-1)^2
  }
  if(result==1){
    
    rps=(p1-1)^2+ (p0+p1-1)^2
  }  
  
  if(result==2){
    
    rps=(p1-0)^2+ (p0+p1-0)^2
  } 
  return(rps/2)
}


#####Decision Tree#####
library(rpart)
library(caret)
ttt1<-ttt
ttt1$row_ind=1:nrow(ttt1)
train<-sample_n(ttt1,floor(nrow(ttt1)*0.67))
test<-ttt1[!ttt1$row_ind%in%train$row_ind,]
train<-train[,-33]
test<-test[,-33]
tree<-rpart(result ~ ., train[,-1], na.action = na.rpart, method="class",
      model = FALSE, x = FALSE, y = TRUE,control = rpart.control(xval = 10,cp=0.01))
prediction=predict(tree,test[,-1])
test$prob1=prediction[,2]
test$prob0=prediction[,1]
test$prob2=prediction[,3]
rpart.control()
for(j in 1:nrow(test)){
  test[j,"rps"]=rps(test[j,"prob1"],test[j,"prob0"],test[j,"prob2"],test[j,"result"])
}
mean(test$rps) ## results seems okay, now cross validation
min=mean(test$rps)
?rpart.control
test=test[,-c(33:35)]
for(i in 1:50){
  tree<-rpart(result ~ ., train[,-1], na.action = na.rpart, method="class",
              model = FALSE, x = FALSE, y = TRUE,control = rpart.control(xval = 10,cp=i/100))
  prediction=predict(tree,test[,-1])
  prediction<-as.data.frame(matrix(prediction,ncol = 3))
  test$prob1=prediction[,2]
  test$prob0=prediction[,1]
  test$prob2=prediction[,3]
  test$rps=0
for(j in 1:nrow(test)){
  test[j,"rps"]=rps(test[j,"prob1"],test[j,"prob0"],test[j,"prob2"],test[j,"result"])
}
  avg_rps=mean(test$rps)
  
    if(avg_rps<=min){
    min=avg_rps
    cp_best=i/100
  }
    }
tree<-rpart(result ~ ., train[,-1], na.action = na.rpart, method="class",
            model = FALSE, x = FALSE, y = TRUE,control = rpart.control(xval = 10,cp=cp_best))

rpart.plot::rpart.plot(tree)
####gradient boosting
library(gbm)
?gbm
?gbm
gbm_boost=gbm(result ~ . ,data = train[,-1],distribution = "multinomial",n.trees = 100,
                 shrinkage = 0.01, interaction.depth = 4)
prediction_gbm=predict(gbm_boost,test[,-1],n.trees = 100,type = "response")
prediction_gbm<-as.data.frame(matrix(prediction_gbm,ncol = 3))
test$prob1_gbm=prediction_gbm[,2]
test$prob0_gbm=prediction_gbm[,1]
test$prob2_gbm=prediction_gbm[,3]
for(j in 1:nrow(test)){
  test[j,"rps"]=rps(test[j,"prob1_gbm"],test[j,"prob0_gbm"],test[j,"prob2_gbm"],test[j,"result"])
}
min=mean(mean(test$rps))
shrinkage=0.1
depth=1
i=1
j=1
while(i <=10){
  while(j <=10){
    set.seed(sample(1:100,1))
    gbm_fit_temp=gbm(result~.,data=train[,-1],distribution = "multinomial",n.trees = 100,cv.folds = 10,n.minobsinnode = 10,shrinkage =i/10,interaction.depth = j )
    prediction_gbm=predict(gbm_fit_temp,test[,-1],n.trees = 100,type = "response")
    prediction_gbm<-as.data.frame(matrix(prediction_gbm,ncol = 3))
    test$prob1_gbm=prediction_gbm[,2]
    test$prob0_gbm=prediction_gbm[,1]
    test$prob2_gbm=prediction_gbm[,3]
    for(t in 1:nrow(test)){
      test[t,"rps"]=rps(test[t,"prob1_gbm"],test[t,"prob0_gbm"],test[t,"prob2_gbm"],test[t,"result"])
    }
    avg_rps=mean(test$rps)
     if(avg_rps<=min){
      shrinkage=i/10
      depth=j
ntrees=100
min=avg_rps
     }
    j=j+1
  }
  i=i+1
}






pl<-subset(matches,matches$league_id==148&matches$epoch>=as.Date("2019-12-26")&matches$match_status=="Finished")
gbm_fit_last=gbm(result~.,data=train[,-1],distribution = "multinomial",n.trees = 100,cv.folds = 10,n.minobsinnode = 10,shrinkage =shrinkage,interaction.depth = depth )
ttt_p<-subset(ttt,ttt$match_id%in%pl$match_id)
ttt_p_predict=predict.gbm(gbm_fit_last,ttt_p[,-1],type = "response")
ttt_p_predict<-as.data.frame(matrix(ttt_p_predict,ncol = 3))
ttt_p$prob1_gbm=ttt_p_predict[,2]
ttt_p$prob0_gbm=ttt_p_predict[,1]
ttt_p$prob2_gbm=ttt_p_predict[,3]
avg_rps= sum(rps(ttt_p$prob1_gbm,ttt_p$prob0_gbm,ttt_p$prob2_gbm,ttt_p$result))/nrow(ttt_p)

summary(gbm_fit_last)






