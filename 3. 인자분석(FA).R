#[1] 데이터 전처리와 기초분석

#데이터 로딩
fifa <- read.csv("fifa_eda_stats.csv", header = TRUE)
head(fifa)
str(fifa)
dim(fifa)
summary(fifa)


#결측치 확인
sapply(fifa, function(x) sum(is.na(x)))
fifa <- na.omit(fifa)  #결측치가 포함된 행을 제거



#의미없는 변수 제거
library(caret)
nearZeroVar(fifa, saveMetrics=TRUE)  #의미없는 변수 없음


#상관관계 파악
fifa_num <- fifa[, sapply(fifa, is.numeric)]  #수치형이 아닌 변수를 제거
fifa_num <- fifa_num[, -((ncol(fifa_num)-4):ncol(fifa_num))]  #GK에 재한 변수 제거
fifa_cor = cor(fifa_num)
print(fifa_cor, digit = 3)


#히트맵
library(corrplot)
corrplot(fifa_cor, method = "color", tl.col = "black",
         tl.srt = 45, addCoef.col = "black", number.cex = 0.7)



#[2] 인자분석 적합성 검정

#KMO 표본적합성 측도
library(psych)
psych::KMO(fifa_num)


#Bartlett 구형성 검정
library(psych)
psych::cortest.bartlett(fifa_cor, n = nrow(fifa_num))



#[3] 인자의 개수

#주성분 분석
fifa_pca <- prcomp(fifa_num, center = T, scale. = T)   #주성분분석
fifa_pca    #주성분분석 결과
summary(fifa_pca)


fifa_pca$sdev^2    #고유값 출력 기준으로 3~4개가 적당해보임
screeplot(fifa_pca, type = 'l') # scree 도표 기준으로 3개


# 인자의 공헌도
library(psych)
fifa_m3 = principal(fifa_num, nfactors = 3, rotate = "varimax")  #인자 3개
fifa_m4 = principal(fifa_num, nfactors = 4, rotate = "varimax")  #인자 4개


print(fifa_m3$loadings, cutoff = 0)  #인자가 3개인 경우
colSums((fifa_m3$loadings)^2) / 5   #각 인자별 공헌도
cumsum(colSums((fifa_m3$loadings)^2) / 5) #인자별 누적공헌도


print(fifa_m4$loadings, cutoff = 0)  #인자가 4개인 경우
colSums((fifa_m4$loadings)^2) / 5   #각 인자별 공헌도
cumsum(colSums((fifa_m4$loadings)^2) / 5) #인자별 누적공헌도


#상관행렬의 고유값 출력
print(fifa_fa$values, digit=4)




#[4] 인자분석

#(1) 주성분분석법
library(psych)
fifa_fa_pca = psych::principal(fifa_num, cor="cor", nfactors=4, rotate="varimax")
print(fifa_fa_pca, sort=TRUE, digit=4)


#인자적재값 출력
print(fifa_fa_pca$loadings, digit=4, cut=0)
print(fifa_fa_pca$loadings, digit=4, cut=0.5)


# 인자점수와 인자적재의 biplot
biplot(x=fifa_fa_pca$scores[sample(10000,50),c(1,2)], y=fifa_fa_pca$loadings[,c(1:3)])
abline(h=0,v=0,lty=2)


#인자구조 다이어그램
library(psych)
psych::fa.diagram(fifa_fa_pca, simple=FALSE, cut=0.7, digit=3)


#(2) 주축인자법
fifa_fa_pa <- psych::fa(fifa_num, cor="cor", nfactors=4, fm="pa", rotate="none")
print(fifa_fa_pa, digits=4) 


#인자적재값 출력
print(fifa_fa_pa$loadings, digit=4, cut=0)
print(fifa_fa_pa$loadings, digit=4, cut=0.5)


#(3) 최대우도법
fifa_fa_ml <- psych::fa(fifa_num,cor="cor", nfactors=4, fm="ml", rotate="none")
print(fifa_fa_ml, digits=4)
