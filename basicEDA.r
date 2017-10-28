#Basic EDA
##1_80은 그랜드마스터 
##81부터 300은 마스터
##301부터 1010은 다이이몬드
##1011부터 2554은 플래티넘
##2555부터 4418은 골드
##4419부터 5515는 실버
##5746부터 5755는 브론즈 
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
