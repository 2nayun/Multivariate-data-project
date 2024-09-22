#데이터 로딩
ex <-read.csv("exercise.csv",header = T)

#데이터 기초 탐색
str(ex)
show(ex)
dim(ex) #데이터 사이즈 확인
summary(ex) #데이터의 각 열의 기본 통계량을 확인



#결측치 확인
sapply(ex, function(x) sum(is.na(x))) #모든 변수에 결측값 없음
ex[!complete.cases(ex),] #결측값이 있는 데이터 없음



#플롯
plot(ex)
plot(ex$walking)
plot(ex$aerobic)
plot(ex$weight)
plot(ex$both)
plot(ex$sitting)



#install.packages("caret")
library(caret)
featurePlot(ex[,4:8],ex$age) #연령대별 데이터의 분포 확인



#이상치 탐색(박스플롯)
ex1 <- ex[,c(4:8)] #4~8번째의 열만 포함한 데이터
boxplot(ex1)



#walking 변수의 이상치 파악
Q3 <- quantile(ex1$walking, 0.75)
w_outlier <-Q3 + IQR(ex1$walking)*1.5
which(ex1$walking > w_outlier) #4개의 이상치 존재
ex1$walking[which(ex1$walking > w_outlier)]



#both 변수의 이상치 파악
Q3 <- quantile(ex1$both, 0.75)
e_outlier <-Q3 + IQR(ex1$both)*1.5
which(ex1$both > e_outlier) 
ex1$both[which(ex1$both > e_outlier)]
ex$gender[which(ex1$both > e_outlier)]
ex$age[which(ex1$both > e_outlier)]
#이상치에 대한 age와 gender를 출력한 결과, 모두 20대 남성



#의미없는 변수 제거
library(caret)
nearZeroVar(ex, saveMetrics=TRUE) #0에 가까운 분산을 갖는 변수 없음



#변수 간 상관관계 파악
library(mlbench)
ex2 <- ex[,c(1,3,4,5,6,7,8)] #수치형 데이터만으로 구성된 데이터
findCorrelation(cor(ex2)) #6열(both)이 다른 변수들과 상관관계가 높음
cor_matrix <- cor(ex2)
cor_matrix



#히트맵
#install.packages("corrplot")
library(corrplot)
corrplot(cor_matrix, method = "color", tl.col = "black",
         tl.srt = 45, addCoef.col = "black", number.cex = 0.7)
#weight와 both의 상관관계가 0.9로 가장 높고 aerobic과 both는 0.83으로 두 번째로 높음.
#age는 모든 변수와 음의 상관관계를 가짐.




#상관관계가 높은 변수 시각화
library(ggplot2)

#weight와 both 산점도
ex4 <- data.frame(x = ex$weight, y = ex$both)
ggplot(ex4, aes(x = x, y = y)) + geom_point(size = 3, color = "darkgrey") + 
  labs(title = "weight와 both 산점도", x = "weight", y = "both") + theme_bw()
ex4_1 <- data.frame(x = ex$weight, y = ex$both, gender = factor(ex$gender))  #gender로 컬러구분
ggplot(ex4_1, aes(x = x, y = y, color = gender)) + geom_point(size = 3) + 
  labs(title = "weight와 both 산점도", x = "weight", y = "both") + theme_bw()

#aerobic와 both 산점도
ex5 <- data.frame(x = ex$aerobic, y = ex$both)
ggplot(ex5, aes(x = x, y = y)) + geom_point(size = 3, color = "darkgrey") + 
  labs(title = "aerobic와 both 산점도", x = "aerobic", y = "both") + theme_bw()
ex5_1 <- data.frame(x = ex$aerobic, y = ex$both, gender = factor(ex$gender))  #gender로 컬러구분
ggplot(ex5_1, aes(x = x, y = y, color = gender)) + geom_point(size = 3) + 
  labs(title = "aerobic와 both 산점도", x = "aerobic", y = "both") + theme_bw()




#age와 다른 변수들 간의 상관관계 파악
library(dplyr)
age_group <- ex %>%
  group_by(age) %>%  # age로 그룹화
  summarise(
    walking_mean = mean(walking),  # walking 평균
    aerobic_mean = mean(aerobic),  # aerobic 평균
    weight_mean = mean(weight),    # weight 평균
    both_mean = mean(both))        # both 평균
print(age_group)

ggplot(age_group, aes(x = age)) +
  geom_line(aes(y = walking_mean, color = "Walking"), size = 1) +
  geom_point(aes(y = walking_mean, color = "Walking"), size = 2) +
  geom_line(aes(y = aerobic_mean, color = "Aerobic"), size = 1) +
  geom_point(aes(y = aerobic_mean, color = "Aerobic"), size = 2) +
  geom_line(aes(y = weight_mean, color = "Weight"), size = 1) +
  geom_point(aes(y = weight_mean, color = "Weight"), size = 2) +
  geom_line(aes(y = both_mean, color = "Both"), size = 1) +
  geom_point(aes(y = both_mean, color = "Both"), size = 2) +
  labs(title = "연령대별 그래프", x = "연령대", y = "평균") +
  theme_bw()

