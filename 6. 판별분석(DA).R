#[1] 데이터 전처리와 선택
library(dplyr)

#데이터 로딩
fifa <- read.csv("fifa_eda_stats.csv", header = TRUE)
head(fifa)
str(fifa)
dim(fifa)
summary(fifa)


#결측치 확인
sapply(fifa, function(x) sum(is.na(x)))
fifa <- na.omit(fifa)  #결측치가 포함된 행을 제거


#5000개의 행만 랜덤으로 선택
set.seed(123)
fifa <- fifa[sample(nrow(fifa), 5000), ]



#포지션 라벨 할당
unique(fifa$Position)
fifa$Position[fifa$Position
              %in% c("RF","LW","LF","ST","RS","LS","CF","RAM","LAM","RW")] <- "Forwards"
fifa$Position[fifa$Position
              %in% c("RCM","LCM","CM","CAM","CDM","LDM","RDM","LM","RM")] <- "Midfielders"
fifa$Position[fifa$Position
              %in% c("CB","RCB","LCB","LB","RB","RWB","LWB")] <- "Defenders"
fifa$Position[fifa$Position %in% c("GK")] <- "Goalkeeper"


#판별분석을 위한 변수 선택
fifa <- fifa %>%
  filter(Position %in% c("Forwards", "Defenders"))      #공격수와 수비수만 선택
fifa <- fifa[, c("Name","Position","Finishing","HeadingAccuracy","Dribbling","Crossing","Interceptions",
                   "Marking","StandingTackle","SlidingTackle")]    #공격,수비 관련 변수 선택



#[2] 모공분산 행렬의 동일성 검정

# install.packages("biotools")
library(biotools)

fifa.v = fifa[,3:10]  # 원 데이터에서 변수 추출
fifa.l = fifa$Position    # 원 데이터에서 포지션 라벨 추출


fifa.boxM = biotools::boxM(fifa.v, fifa.l)
fifa.boxM
# 검정 결과 p-value가 매우 작아 등분산 가정이 위배됨.
#즉, 두 클래스의 공분산은 다름->QDA를 수행할 것.



#[3] 판별분석 수행

#이차 판별분석 QDA
library(MASS)
fifa.qda <- MASS::qda(Position ~ Finishing+HeadingAccuracy+
                        Dribbling+Crossing+Interceptions+Marking+
                        StandingTackle+SlidingTackle ,data=fifa)
print(fifa.qda)    #QDA 결과 요약
fifa.qda$scaling   #각 공분산을 scaling




#[4] 라벨 예측
fifa.pred = predict(fifa.qda, fifa)   #확률 계산
print(fifa.pred)



#(1) 정오분류표
fifa.table = table(fifa.l, fifa.pred$class)
print(fifa.table)


#혼동행렬값 입력
fifa.tp <- fifa.table["Forwards", "Forwards"]     
fifa.tn <- fifa.table["Defenders", "Defenders"]   
fifa.fp <- fifa.table["Defenders", "Forwards"]  
fifa.fn <- fifa.table["Forwards", "Defenders"]


#(2) 정확도
fifa.accuracy <- (fifa.tp + fifa.tn) / sum(fifa.table)
print(fifa.accuracy)


#정밀도와 재현율
fifa.precision <- fifa.tp / (fifa.tp + fifa.fp)
fifa.recall <- fifa.tp / (fifa.tp + fifa.fn)


#(3) F1스코어
f1_score <- 2 * (fifa.precision * fifa.recall) / (fifa.precision + fifa.recall)
print(f1_score)


#(4) 시각화
DescTools::Desc(fifa.table, digits=2)
# install.packages("klaR")
library(klaR)
fifa$Position = as.factor(fifa$Position) # 포지션 라벨을 factor 형태로 입력

windows()
klaR::partimat(Position ~ Finishing+HeadingAccuracy+
                 Dribbling+Crossing+Interceptions+Marking+
                 StandingTackle+SlidingTackle, data=fifa, method="qda")
