#[1] 데이터 로딩
fifa <- read.csv("fifa_eda_stats.csv", header = TRUE)
set.seed(12)   #난수생성
fifa <- fifa[sample(nrow(fifa), 20), ]
head(fifa)
str(fifa)
dim(fifa)
summary(fifa)

sapply(fifa, function(x) sum(is.na(x)))  #결측치 확인
fifa <- na.omit(fifa)  #결측치가 포함된 행을 제거



#[2] 변수 그룹 정의
fifa_x <- fifa[,c("Finishing","HeadingAccuracy","Dribbling","Crossing")]  #공격능력 관련
fifa_y <- fifa[,c("Interceptions","Marking","StandingTackle","SlidingTackle")]  #수비능력 관련


#[3] 정준상관분석 수행
# install.packages("CCA")
# library(CCA)
fifa_cc <-CCA::cc(fifa_x, fifa_y)
fifa_cc


#[4] 정준상관계수 출력
CCA::matcor(fifa_x, fifa_y)  #변수 그룹의 상관행렬
fifa_cc$cor 
#첫 정준상관계수가 0.9179로 두 변수그룹이 높은 상관관계를 갖는다.



#[5] 정준계수 출력
fifa_cc$xcoef   # x-집단, 원(raw) 정준계수
fifa_cc$ycoef   # y-집단, 원(raw) 정준계수
fifa_cc$xcoef*sapply(fifa_x,sd)   # x-집단, 표준화된(standardized) 정준계수
fifa_cc$ycoef*sapply(fifa_y,sd)   # y-집단, 표준화된(standardized) 정준계수



#[6] 정준점수 출력 
x_score <- fifa_cc$scores$xscores   # x-정준점수
colnames(x_score) <- paste0("AttackVar", 1:4)
y_score <- fifa_cc$scores$yscores   # y-정준점수
colnames(y_score) <- paste0("DefenseVar", 1:4)
fifa_score <- cbind(x_score,y_score)   # 결합   
rownames(fifa_score) <- fifa$fifa
round(fifa_score,digits=3)



#[7] 정준 점수 플롯
plot(fifa_score[,c(1,4)],pch=1,col="blue", ,xlim=c(-2,2),ylim=c(-2,2))
abline(v=0,h=0,lty=2)
text(fifa_score[,c(1,4)],labels=fifa$Name,pos=4,col="red")
legend('topleft', legend = paste("corr =",format(fifa_cc$cor[1], digits = 3)))

plot(fifa_score[,c(2,5)],pch=1,col="blue", ,xlim=c(-2,2),ylim=c(-2,2))
abline(v=0,h=0,lty=2)
text(fifa_score[,c(2,5)],labels=fifa$Name,pos=4,col="red")
legend('topleft', legend = paste("corr =",format(fifa_cc$cor[2], digits = 3)))



#[8] 정준적재 출력

fifa_cc$xcoef*sapply(fifa_x,sd)   # x-집단, 표준화된(standardized) 정준계수
fifa_cc$ycoef*sapply(fifa_y,sd)   # y-집단, 표준화된(standardized) 정준계수

fifa_cc$scores$corr.X.xscores   # x-정준적재 
fifa_cc$scores$corr.Y.yscores   # y-정준적재 



#[9] 정준적재 플롯

fifa_loading <- rbind(fifa_cc$scores$corr.X.xscores,
                      fifa_cc$scores$corr.Y.yscores)
windows()
plot(fifa_loading[,1:2],pch=1,col="red")
abline(v=0,h=0,lty=2)
text(fifa_loading[,1:2],labels=rownames(fifa_loading),pos=4,col="blue")



#[10] 교차적재
fifa_cc$scores$corr.X.yscores   # x-교차적재 
fifa_cc$scores$corr.Y.xscores   # y-교차적재 


#[10] 정준변량들의 공헌도
#install.packages("yacca")
#library(yacca)
yacca::cca(fifa_x,fifa_y)$xcanvad
yacca::cca(fifa_x,fifa_y)$ycanvad
