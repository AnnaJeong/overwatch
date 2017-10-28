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
