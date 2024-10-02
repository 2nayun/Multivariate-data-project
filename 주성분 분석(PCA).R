#데이터 로딩
ex <-read.csv("exercise.csv",header = T)
summary(ex)



#상관관계 파악
library(psych)
ex1 <- ex[,c(1,3,4,5,6,7,8)] #수치형 데이터만 포함
cor(ex1)   # 상관행렬 출력
corr.test(ex1)   # 상관계수에 대한 p-값 출력
#install.packages("corrplot")
library(corrplot)
corrplot(cor_matrix, method = "color", tl.col = "black",
         tl.srt = 45, addCoef.col = "black", number.cex = 0.7)
#히트맵을 통한 상관관계 파악



#주성분분석
ex2 <- ex[,c(4,5,6,7)]
cov(ex2)   #공분산행렬 출력
ex.prcomp <- prcomp(~ walking+aerobic+weight+both, data=ex2) #주성분분석 
print(ex.prcomp)     #리스트 객체(고유벡터 등) 출력   
ex.prcomp$sdev       #주성분의 표준편차 출력  
ex.prcomp$sdev^2     # 고유값 출력  
summary(ex.prcomp)   # 설명분산 요약      
# 주성분들이 전체 데이터 변동에 대한 설명 비율을 확인할 수 있다.
# 2번째 까지의 주성분이 전체 변동의 약 90%를 설명하므로
#주성분의 개수는 2개로 설정하는 것이 좋다.


#주성분 개수 판정
screeplot(ex.prcomp, type = 'l', npcs = 4)
#스크리 도표를 그렸을때 2인 지점에서 크게 꺾이는 것을 확인할 수 있음.



#주성분점수 출력
ex.score <- cbind(ex,ex.prcomp$x[,1:2])
print(ex.score)



#주성분분석 행렬도(biplot)
ex.prcomp <- prcomp(~ walking+aerobic+weight+both, data=ex2)
biplot(ex.prcomp,xlim=c(-0.5,0.3)) 
abline(h=0,v=0,lty=2) 



#주성분점수 그래프
plot(ex.score[c("PC1","PC2")],xlim=c(-50,40),ylim=c(-20,20))
abline(h=0,v=0)
text(ex.score[c("PC1","PC2")],labels=rownames(ex.score),pos=2)



#성별(범주형 데이터)로 라벨을 붙인 그래프
plot(ex.score[c("PC1","PC2")],xlim=c(-50,40),ylim=c(-20,20))
abline(h=0,v=0)
text(ex.score[c("PC1","PC2")], labels=ex.score$gender,pos=2)



#연령대별 라벨을 붙인 그래프
plot(ex.score[c("PC1","PC2")],xlim=c(-50,40),ylim=c(-20,20))
abline(h=0,v=0)
text(ex.score[c("PC1","PC2")],labels=ex.score$age,pos=2)



# 집단 평균 계산
pcm.age = aggregate(cbind(PC1, PC2) ~ age,    data = ex.score, FUN = mean)
pcm.gender = aggregate(cbind(PC1, PC2) ~ gender, data = ex.score, FUN = mean)



# 개체별 플롯
plot(ex.score[c("PC1","PC2")],xlim=c(-50,40),ylim=c(-20,20))
abline(h=0,v=0)
# 성별 중심점
points(pcm.gender[,2:3],pch=19,cex=1.5,col="red")
text(pcm.gender[,2:3],labels=pcm.gender$gender,pos=4,col="red")



# 개체별 플롯
plot(ex.score[c("PC1","PC2")],xlim=c(-50,40),ylim=c(-20,20))
abline(h=0,v=0)
# 연령대별 중심점
points(pcm.age[,2:3],pch=19,cex=1.5,col="darkgreen")
text(pcm.age[,2:3],labels=pcm.age$age,pos=4,col="darkgreen")

