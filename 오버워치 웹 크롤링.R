library(tidyverse)
library(rvest)

library(stringr)

## 2019/09/25일에 수집한 데이터
##각 티어별 중간 10페이지(천명)을 추출해 비교



overwatch_doc<-list()
url<-"http://overlog.gg/leaderboards/global/rank/"

overwatch_doc<-url %>%
  read_html()


##function으로 만들기
scraping <- function(page_number){
  url_base<-"http://overlog.gg/leaderboards/kr/rank/"
  url <- paste0(url_base, page_number)
  overwatch_doc<-url %>%
    read_html
  
# ##자료넣기
 #id
   id <- overwatch_doc %>%
   html_nodes("#LeaderBoardsLayoutContent > table > tbody > tr > td.ContentCell.ContentCell-Player > a > b") %>%
   html_text %>%
   str_replace_all('(?<=<b>).+?(?=</b>)',"")
 #level
   level <- overwatch_doc %>%
     html_nodes("#LeaderBoardsLayoutContent > table > tbody > tr > td.ContentCell.ContentCell-Player > a > em") %>%
     html_text() %>%
     str_replace_all('Lv. ',"") %>%
     as.numeric()  
 #score
   score <- overwatch_doc %>%
     html_nodes("#LeaderBoardsLayoutContent > table > tbody > tr >td.ContentCell.ContentCell-SkillRating") %>%
     html_text() %>%
     str_replace_all('\\s+',"") %>%
   as.numeric()
 #killdeath
 killdeath <- overwatch_doc %>%
   html_nodes("#LeaderBoardsLayoutContent > table > tbody > tr > td.ContentCell.ContentCell-KD > b") %>%
   html_text() %>%
   str_replace_all(' : 1',"")  %>%
   as.numeric()
 #winratio
 winratio <- overwatch_doc %>%
   html_nodes("#LeaderBoardsLayoutContent > table > tbody > tr > td.ContentCell.ContentCell-WinRatio > div > div.WinRatio > b") %>%
   html_text() %>%
   as.numeric()
 #playtime
 playtime <- overwatch_doc%>%
   html_nodes("#LeaderBoardsLayoutContent > table > tbody > tr > td.ContentCell.ContentCell-PlayTime") %>%
   html_text() %>%
   str_replace_all(" hours","") %>%
   str_replace_all(" hour", "") %>%
   as.numeric()
 #mostheroes
most1 <- overwatch_doc %>%
  html_nodes("#LeaderBoardsLayoutContent > table > tbody > tr > td.ContentCell.ContentCell-MostHeros > img:nth-child(1)") %>%
  html_attr("alt")
  
most2 <- overwatch_doc %>%
  html_nodes("#LeaderBoardsLayoutContent > table > tbody > tr > td.ContentCell.ContentCell-MostHeros > img:nth-child(2)") %>%
  html_attr("alt") 
most3 <- overwatch_doc %>%
  html_nodes("#LeaderBoardsLayoutContent > table > tbody > tr > td.ContentCell.ContentCell-MostHeros > img:nth-child(3)") %>%
  html_attr("alt") 


list(id,level,score,killdeath,winratio,playtime,most1,most2,most3)

}


ID <- LEVEL <- SCORE <- KILLDEATH <- WINRATIO <- PLAYTIME <- MOST1 <- MOST2 <- MOST3 <-NULL
##1_80은 그랜드마스터 
##81부터 300은 마스터
##301부터 1010은 다이이몬드
##1011부터 2554은 플래티넘
##2555부터 4418은 골드
##4419부터 5515는 실버
##5746부터 5755는 브론 
for (i in 5746:5747){
ID <- unlist(c(ID,scraping(i)[1]))
LEVEL <- unlist(c(LEVEL,scraping(i)[2]))
SCORE <- unlist(c(SCORE, scraping(i)[3]))
KILLDEATH <- unlist(c(KILLDEATH, scraping(i)[4]))
WINRATIO <- unlist(c(WINRATIO, scraping(i)[5]))
PLAYTIME <- unlist(c(PLAYTIME, scraping(i)[6]))
MOST1 <- unlist(c(MOST1,scraping(i)[7]))
MOST2 <- unlist(c(MOST2, scraping(i)[8]))
MOST3 <- unlist(c(MOST3, scraping(i)[9]))
}
#MOST2와 MOST3가 존재하지만 결측치가 많아 제외하기로 한다 
overwatch <- data.frame(ID,LEVEL,SCORE,KILLDEATH,WINRATIO,PLAYTIME,MOST1)
write.csv(overwatch,"overwatch.csv",row.names=F)
##티어 변수를 만들어준다
#티어 나누기다
for (i in 1:nrow(overwatch)){
  if (overwatch$SCORE[i]<=1499) {
    overwatch$tear[i]="bronze"
  } else if (overwatch$SCORE[i]<=1999) {
    overwatch$tear[i]="silver"
  } else if (overwatch$SCORE[i]<=2499){
    overwatch$tear[i]="gold"
  } else if (overwatch$SCORE[i]<=2999){
    overwatch$tear[i]="platinum"
  } else if (overwatch$SCORE[i]<=3499){
    overwatch$tear[i]="diamond"
  } else if (overwatch$SCORE[i]<=3999){
    overwatch$tear[i]="master"
  } else if (overwatch$SCORE[i]<=5000){
    overwatch$tear[i]="grandmaster"
  } else (overwatch$tear[i]="top500")
}
overwatch$tear<-as.factor(overwatch$tear)
overwatch$MOST1<-as.factor(overwatch$MOST1)

heaven <- c("grandmaster","master","diamond","platinum")
overwatch$heaven <- ifelse(overwatch$tear %in% heaven,"yes","no")
overwatch$heaven <- as.factor(overwatch$heaven)
write.csv(overwatch,"overwatch1.csv",row.names=F)
#쓸데이 있을지도 모르니 MOST2와 MOST3 따로 저장 
write.csv(MOST2,"most2.csv",row.names=F)
write.csv(MOST3,"most3.csv",row.names=F)

#setwd("C:\\Users\\jhj73_000\\Desktop\\고대\\데잇걸즈\\개인프로젝트")
#overwatch<-read.csv("overwatch1.csv",header=T)
#EDA
#티어별 평균
#티어 순서로 바꾸기
overwatch$tear<-factor(overwatch$tear, levels=c("grandmaster","master","diamond","platinum","gold","silver","bronze"))
library(ggplot2)
#티어에 따른 레벨 박스플롯 
ggplot(overwatch, aes(x=tear,y=LEVEL, fill=tear)) + 
  geom_boxplot() + scale_fill_brewer(palette="YlOrBr") +
  labs(title="티어에 따른 레벨", x="티어", y="레벨", fill="티어")
#천상계 ox에 따른 레벨 박스플
ggplot(overwatch, aes(x=heaven,y=LEVEL, fill=heaven)) + geom_boxplot() + scale_fill_brewer(palette="YlOrBr")
#티어에 따른 킬뎃 박스플롯
ggplot(overwatch, aes(x=tear,y=KILLDEATH, fill=tear))+
  geom_boxplot() + scale_fill_brewer(palette="YlOrBr") +
  labs(title="티어에 따른 킬뎃", x="티어", y="킬뎃", fill="티어")
#천상계 ox에 따른 킬뎃 박스플롯
ggplot(overwatch, aes(x=heaven,y=KILLDEATH, fill=heaven))+geom_boxplot() + scale_fill_brewer(palette="YlOrBr")
#티어에 따른 승률 박스풀롯
ggplot(overwatch, aes(x=tear, y= WINRATIO, fill=tear))+geom_boxplot() + 
  scale_fill_brewer(palette="YlOrBr") +
  labs(title="티어에 따른 승률", x="티어", y="승률", fill="티어")
#천상계 ox에 승률 박스풀롯
ggplot(overwatch, aes(x=heaven, y= WINRATIO, fill=heaven))+geom_boxplot() + scale_fill_brewer(palette="YlOrBr")

#티어에 따른 게임시간 박스플롯
ggplot(overwatch, aes(x=tear, y=PLAYTIME, fill=tear))+
  geom_boxplot()+ scale_fill_brewer(palette="YlOrBr") +
  labs(title="티어에 따른 게임시간", x="티어", y="게임시간", fill="티어")
#천상계 ox에 게임시 박스풀롯
ggplot(overwatch, aes(x=heaven, y=PLAYTIME, fill=heaven))+
  geom_boxplot()+ scale_fill_brewer(palette="YlOrBr")


#티어에 따른 모스트
ggplot(overwatch, aes(x=MOST1, fill=tear))+geom_bar()+ scale_fill_brewer(palette="YlOrBr")+
  theme(text = element_text(size=20),axis.text.x = element_text(angle=45, hjust=1)) +
  labs(title="영웅별 티어", x="영웅",y="빈도", fill="티어")


##공격 수비 돌격 지원으로 나누기
attack <- c("Genji","Doomfist","Reaper","McCree","Soldier: 76","Sombra","Tracer","Pharah")
defence <- c("Mei","Bastion","Widowmaker","Junkrat","Torbjorn","Hanzo")
charge <- c("Reinhardt","Roadhog","Orisa","Winston","Zarya","D.Va")
support <-c("Lucio","Mercy","Symmetra","Ana","Zenyatta")
overwatch$attack <- ifelse(overwatch$MOST1 %in% attack,1,0)
overwatch$defence <- ifelse(overwatch$MOST1 %in% defence,1,0)
overwatch$charge <- ifelse(overwatch$MOST1 %in% charge,1,0)
overwatch$support <- ifelse(overwatch$MOST1 %in% support,1,0)
overwatch$group <- ifelse(overwatch$MOST1 %in% attack,"attack",ifelse(overwatch$MOST1 %in% defence, "defence", ifelse(overwatch$MOST1 %in% charge, "charge",ifelse(overwatch$MOST1 %in% support,"support",0))))
write.csv(overwatch,"overwatch2.csv",row.names=F)

ggplot(overwatch, aes(x=group, fill=tear))+geom_bar()+ scale_fill_brewer(palette="YlOrBr")
ggplot(overwatch, aes(x=tear, fill=group))+geom_bar()+ scale_fill_brewer(palette="BrBG")+
  labs(title="티어별 영웅 타입", x="티어",y="빈도", fill="영웅 타입")


#train test 나누기 
set.seed(0321)
RandomIndex <- sample(1:nrow(overwatch), size=round(nrow(overwatch)*0.7), replace=F)
TrainOver <- overwatch[RandomIndex,]
TestOver <- overwatch[-RandomIndex,]
overwatch$group <- as.factor(overwatch$group)
#tree package
library(tree)
tree_over<-tree(heaven ~ .-SCORE-tear-heaven-ID, data=TrainOver)
prop.table(table(TrainOver$tear))
tree_over_cv <- cv.tree(tree_over, K=5, FUN=prune.misclass)
plot(tree_over_cv)

plot(tree_over)
text(tree_over, pretty=0)
#별로다

#party package 이용
library(party)
ctree_over<-ctree(heaven ~ LEVEL+KILLDEATH+WINRATIO+PLAYTIME+MOST1+attack+defence+charge+support+group, data=TrainOver)
plot(ctree_over, type="simple")
#훨씬 낫다
summary(ctree_over)

#rpart
library(rpart)
library(rpart.plot)
library(ROCR)
library(e1071)
source("performance_plot_utils.R")
rpart_over <- rpart(heaven~ LEVEL+KILLDEATH+WINRATIO+PLAYTIME+MOST1+attack+defence+charge+support+group, method="class", data=TrainOver,
                   control=rpart.control(minsplit=20, cp=0.01))
plot(rpart_over)
text(rpart_over, pretty=1)

library(caret)
CVtrain <- train(heaven~LEVEL+KILLDEATH+WINRATIO+PLAYTIME+MOST1+attack+defence+charge+support+group, data=TrainOver,
                 method="rpart",
                 trControl=trainControl(method="cv",
                                        number=10))
CVtrain <- train(heaven~LEVEL+KILLDEATH+WINRATIO+PLAYTIME+attack+defence+charge+support+group, data=TrainOver,
                 method="rpart",
                 trControl=trainControl(method="cv",
                                        number=10))
importance <- varImp(CVtrain)
plot(importance)

rpart_fit2 <- rpart(heaven~LEVEL+KILLDEATH+WINRATIO+PLAYTIME+defence+attack,
                    data=TrainOver,
                    control=rpart.control(minsplit=20, cp=0.01))
plot(rpart_fit2)
text(rpart_fit2)

rpart_pred<-predict(rpart_over,TestOver, type="class")
rpart_pred2<-predict(rpart_fit2,TestOver, type="class")
rpart_tab<-table(TestOver$heaven, rpart_pred)
rpart_tab2<-table(TestOver$heaven, rpart_pred2)

#KNN
#normalize 먼저 해야한다
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
train_X <- TrainOver[,names(TrainOver)!="tear" & names(TrainOver)!="ID" & names(TrainOver)!="heaven"&names(TrainOver)!="group"&names(TrainOver)!="MOST1"&names(TrainOver)!="SCORE"]
train_Y <- TrainOver[ , names(TrainOver) =="heaven"]

test_X <- TestOver[,names(TrainOver)!="tear" & names(TrainOver)!="ID" & names(TrainOver)!="heaven"&names(TrainOver)!="SCORE"]
test_Y <- TestOver[,names(TestOver)=="heaven"]

train_X <-apply(train_X,2,normalize)
test_X <- apply(test_X,2,normalize)
prop.table(table(train_Y))
prop.table(table(test_Y))

library(class)
knn_over <- knn(train_X,test_X,train_Y,k=21)
table(knn_over)

library(gmodels)
CrossTable(x=test_Y, y=knn_over, prop.chisq=FALSE, prop.c=FALSE)
set.seed(0529)
CrossIndex <- sample(x=1:5, size=nrow(train_X), replace=TRUE)
Candidate_K <- seq(from=1, to=29, by=2)
ErrorMat <- matrix(nrow=5, ncol=length(Candidate_K))


for(i in 1:5){
  
  CrossTrain_X <- train_X[CrossIndex != i, ]
  CrossTrain_Y <- train_Y[CrossIndex != i]
  
  CrossTest_X <- train_X[CrossIndex == i, ]
  CrossTest_Y <- train_Y[CrossIndex == i]
  
  for(j in 1:length(Candidate_K)){
    
    pred <- knn(train=CrossTrain_X,
                test=CrossTest_X,
                cl=CrossTrain_Y,
                k=Candidate_K[j])
    
    tab <- table(CrossTest_Y, pred)
    
    ErrorMat[i, j] <- sum(tab[1,2] + tab[2,1]) / sum(tab)
    
  }
  
}
ErrorMat
MeanError <- apply(ErrorMat, 2, mean)
names(MeanError) <- Candidate_K
which.min(MeanError) #k=21일때 

knn2_over <- knn(train=train_X,test=test_X,cl=train_Y,k=21)
knn2_tab <- table(test_Y, knn2_over)
knn2_tab

##knn에 가중치 주기
library(kknn)

Train <- data.frame(train_Y, train_X)
Test <- data.frame(test_Y, test_X)
knn_weight <- kknn(train_Y~.,train=Train, test=Test,k=29, distance=2, kernel="triangular")
knn_weight_pred <- fitted(knn_weight)
CrossTable(x=Test$test_Y, y=knn_weight_pred,prop.chisq=FALSE, prop.c=FALSE)
set.seed(0529)
CrossIndex <- sample(x=1:5, size=nrow(Train), replace=TRUE)

Candidate_K <- seq(from=1, to=29, by=2)

ErrorMat <- matrix(nrow=5, ncol=length(Candidate_K))

for(i in 1:5){
  
  CrossTrain <- Train[CrossIndex != i, ]
  CrossTest <- Train[CrossIndex == i, ]
  
  for(j in 1:length(Candidate_K)){
    
    fit <- kknn(train_Y~., 
                train=CrossTrain,
                test=CrossTest,
                k=Candidate_K[j],
                distance=2,
                kernel="triangular")
    
    pred <- fitted(fit)
    
    tab <- table(CrossTest$train_Y, pred)
    
    ErrorMat[i, j] <- sum(tab[1,2] + tab[2,1]) / sum(tab)
    
  }
  
}
MeanError <- apply(ErrorMat, 2, mean)
names(MeanError) <- Candidate_K
which.min(MeanError) #k=29

knn_weight2 <- kknn(train_Y~., 
                  train=Train,
                  test=Test,
                  k=29,
                  distance=2,
                  kernel="triangular")
knn_weight_pred2 <- fitted(knn_weight2)
CrossTable(x=Test$test_Y, y=knn_weight_pred, prop.chisq=FALSE, prop.c=FALSE)
CrossTable(x=Test$test_Y, y=knn_weight_pred2, prop.chisq=FALSE, prop.c=FALSE)
#똑같으니까 그냥 하나만 예측력 평가

names(overwatch)
factors<-c("ID","SCORE", "tear")
trainglm <- TrainOver[,!names(overwatch) %in% factors]
testglm <- TestOver[,!names(overwatch) %in% factors]
#glm
glm_over <- glm(heaven ~.,family=binomial, data=trainglm)
glm_null <- glm(heaven ~ 1,family=binomial,data=trainglm)
glm_both <- step(glm_null,scope=formula(glm_over),direction="both")
glm_for <- step(glm_null, scope=formula(glm_over),direction="forward")
glm_back <- step(glm_over,direction="backward")

glm_both_pred <- predict(glm_both, testglm, type="response")
glm_for_pred <- predict(glm_for, testglm, type="response")
glm_back_pred <- predict(glm_back, testglm, type="response")

glm_both_pred <- ifelse(glm_both_pred <=0.43,"no","yes" )
glm_for_pred <- ifelse(glm_for_pred <=0.43,"no","yes" )
glm_back_pred <- ifelse(glm_back_pred <=0.43,"no","yes" )

glm_both_tab <- table(testglm$heaven,glm_both_pred)
glm_for_tab <- table(testglm$heaven,glm_for_pred)
glm_back_tab <- table(testglm$heaven, glm_back_pred)
#세개다 똑같으니 glm_both_tab하나만 씀


#예측력 평가
#party
party_pred <- predict(ctree_over,TestOver)
party_tab<-table(party_pred,TestOver$heaven)
knn2_tab <- table(knn2_over,test_Y)

party_accuracy <- (party_tab[1,1]+party_tab[2,2])/sum(party_tab) #0.69
party_sen <- party_tab[2,2]/sum(party_tab[,2]) #0.72
party_spe <- party_tab[1,1]/sum(party_tab[,1]) #0.64

knn_accuracy <- (knn2_tab[1,1]+knn2_tab[2,2])/sum(knn2_tab) #0.67
knn_sen <- knn2_tab[2,2]/sum(knn2_tab[,2]) #0.68
knn_spe <- knn2_tab[1,1]/sum(knn2_tab[,1]) #0.65

glm_accuracy <-(glm_both_tab[1,1]+glm_both_tab[2,2])/sum(glm_both_tab) #0.70
glm_sen <- glm_both_tab[2,2]/sum(glm_both_tab[,2]) #0.69
glm_spe <- glm_both_tab[1,1]/sum(glm_both_tab[,1]) #0.69

rpart_accuracy<-(rpart_tab[1,1]+rpart_tab[2,2])/sum(rpart_tab)


#로지스틱 모델이 제일 좋다
summary(glm_both)


#corrplot
#library(corrplot)
#corr <- cor(corrover, use="na.or.complete")
#corrplot(corr, type="upper", order="hclust", tl.col="black", tl.srt=45)

