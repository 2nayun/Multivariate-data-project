#[1] 데이터 로딩
library(dplyr)
library(psych)

luad <- read.table("luad.txt", header = TRUE, sep = "\t",
                   na.strings = c('[Not Available]','[Unknown]','[Not Avaliable]',
                                  '[Discrepancy]','[Not Evaluated]','[Not Applicable]'))
str(luad)
dim(luad)



#[2] 데이터 전처리
#결측치 확인
sapply(luad, function(x) sum(is.na(x)))


#na개수가 200개 이상인 변수 삭제
luad0 <-luad[, colSums(is.na(luad)) < 200]
luad0 <- cbind(luad0, luad[, c("death_days_to", "ajcc_tumor_pathologic_pt")])
dim(luad0)
sapply(luad0, function(x) sum(is.na(x)))


#특정 변수의 값이 na인 행 삭제
luad1 <- luad0[!is.na(luad0$race)&!is.na(luad0$ethnicity)&!is.na(luad0$birth_days_to)&
                 !is.na(luad0$history_neoadjuvant_treatment)&!is.na(luad$ajcc_nodes_pathologic_pn)&
                 !is.na(luad0$tumor_status)&!is.na(luad0$ajcc_staging_edition)&
                 !is.na(luad0$tobacco_smoking_history_indicator)&!is.na(luad0$ajcc_metastasis_pathologic_pm)&
                 !is.na(luad0$tobacco_smoking_pack_years_smoked)&!is.na(luad0$age_at_initial_pathologic_diagnosis)&
                 !is.na(luad0$days_to_initial_pathologic_diagnosis)&!is.na(luad0$anatomic_organ_subdivision)&
                 !is.na(luad0$ajcc_pathologic_tumor_stage)|!is.na(luad0$death_days_to),]
sapply(luad1, function(x) sum(is.na(x)))
dim(luad1)


#불필요한 변수 삭제
luad2 <-subset(luad1, select = -c(prospective_collection, retrospective_collection, residual_tumor,
                                  pulmonary_function_test_indicator, tumor_status, ethnicity, last_contact_days_to,
                                  anatomic_organ_subdivision, ajcc_staging_edition ))
sapply(luad2, function(x) sum(is.na(x)))
dim(luad2)


luad2 <- luad2[!is.na(luad2$death_days_to), ]
luad2 <- luad2[!is.na(luad2$race), ]
luad2 <- luad2[!is.na(luad2$birth_days_to), ]
luad2 <- luad2[!is.na(luad2$tobacco_smoking_history_indicator), ]
luad2 <- luad2[!is.na(luad2$tobacco_smoking_pack_years_smoked), ]


dim(luad2)
sapply(luad2, function(x) sum(is.na(x)))
str(luad2)
luad2 <- luad2[-c(1, 2), ]   #데이터 레이블과 코드행 삭제


#[3] 연속형 변수의 처리
luad2$birth_days_to <-as.numeric(luad2$birth_days_to)
luad2$tobacco_smoking_history_indicator <-as.numeric(luad2$tobacco_smoking_history_indicator)
luad2$tobacco_smoking_pack_years_smoked <-as.numeric(luad2$tobacco_smoking_pack_years_smoked)
luad2$age_at_initial_pathologic_diagnosis <-as.numeric(luad2$age_at_initial_pathologic_diagnosis)
luad2$tissue_source_site <-as.numeric(luad2$tissue_source_site)

str(luad2)



#[4] 파생변수 생성
#나이 변수
luad2$birth_days_to <-as.numeric(luad2$bith_days_to)
luad2$birth_days_to <-(abs(luad2$birth_days_to)/360)

str(luad2)



#[5] 병기 변수의 연속형 변환
#ajcc_tumor_pathologic_pt
unique(luad2$ajcc_tumor_pathologic_pt)
luad2 <- luad2 %>%
  mutate(ajcc_tumor_pathologic_pt = case_when(
    ajcc_tumor_pathologic_pt %in% c("T1", "T1a","T1b") ~ 0.5,
    ajcc_tumor_pathologic_pt %in% c("T2", "T2a","T2b") ~ 1.5,
    ajcc_tumor_pathologic_pt == "T3" ~ 3, ajcc_tumor_pathologic_pt == "T4" ~ 4,
    TRUE ~ as.numeric(ajcc_tumor_pathologic_pt)))

#ajcc_nodes_pathologic_pn
luad2 <- luad2 %>%
  mutate(ajcc_nodes_pathologic_pn = case_when(
    ajcc_nodes_pathologic_pn == "N0" ~ 0, ajcc_nodes_pathologic_pn == "N1" ~ 1,
    ajcc_nodes_pathologic_pn == "N2" ~ 2.5, ajcc_nodes_pathologic_pn == "N3" ~ 4,
    TRUE ~ as.numeric(ajcc_nodes_pathologic_pn)))

#ajcc_metastasis_pathologic_pm
luad2 <- luad2 %>%
  mutate(ajcc_metastasis_pathologic_pm = case_when(
    ajcc_metastasis_pathologic_pm %in% c("M1", "M1a","M1b") ~ 1, 
    ajcc_tumor_pathologic_pt == "M0" ~ 0,
    TRUE ~ as.numeric(ajcc_tumor_pathologic_pt)))

str(luad2)
sapply(luad2, function(x) sum(is.na(x)))
luad2 <- luad2[!is.na(luad2$tissue_source_site), ]
luad2 <- luad2[!is.na(luad2$ajcc_nodes_pathologic_pn), ]
sapply(luad2, function(x) sum(is.na(x)))
luad_clean <-luad2



#[6] 수치형 변수만 선택
luad_num <- luad_clean %>% select_if(is.numeric)
dim(luad_num)



#[7] 상관관계 파악
cor_matrix <-cor(luad_num)   # 상관행렬 출력
corr.test(luad_num)   # 상관계수에 대한 p-값 출력
library(corrplot)
plot.new()
dev.off()
corrplot(cor_matrix, method = "color", tl.col = "black",
         tl.srt = 45, addCoef.col = "black", number.cex = 0.7)



#[8] 주성분분석
cov(luad_num)   #공분산행렬 출력
luad_prcomp <- prcomp(~ ., data=luad_num) #주성분분석 
print(luad_prcomp)     #리스트 객체(고유벡터 등) 출력   
luad_prcomp$sdev       #주성분의 표준편차 출력  
luad_prcomp$sdev^2     # 고유값 출력  
summary(luad_prcomp)   # 설명분산 요약      
loadings <- luad_prcomp$rotation
print(loadings)



#[9] 주성분 개수 선택
windows(width = 10, height = 6)
screeplot(luad_prcomp, type = 'l', npcs = 4)



#[10] 주성분 분석 결과 시각화
#주성분분석 행렬도(biplot)
windows(width = 10, height = 6)
biplot(luad_prcomp) 
abline(h=0,v=0,lty=2) 

#주성분점수 출력
luad_score <- cbind(luad_num,luad_prcomp$x[,1:2])
print(luad_score)

#주성분점수 그래프
windows(width = 10, height = 6)
plot(luad_score[c("PC1","PC2")],xlim=c(-50,40),ylim=c(-20,20))
abline(h=0,v=0)
text(luad_score[c("PC1","PC2")],labels=rownames(ex.score),pos=2)

#인종(범주형 데이터)로 라벨을 붙인 그래프
plot(luad_score[c("PC1","PC2")],xlim=c(-50,40),ylim=c(-20,20))
abline(h=0,v=0)
text(luad_score[c("PC1","PC2")], labels=luad_clean$race,pos=2)

#성별(범주형 데이터)로 라벨을 붙인 그래프
plot(luad_score[c("PC1","PC2")],xlim=c(-50,40),ylim=c(-20,20))
abline(h=0,v=0)
text(luad_score[c("PC1","PC2")], labels=luad_clean$gender,pos=2)
