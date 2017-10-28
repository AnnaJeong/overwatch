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
