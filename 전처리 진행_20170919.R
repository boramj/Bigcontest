#library load---------------------------------------------------
library(ggplot2)
library(dplyr)
library(reshape2)
library(corrplot)
library(rpart)
library(rpart.plot)
library(randomForest)
#wd,readcsv ---------------------------------------------------------
setwd("C:/Users/hanbum/Desktop/rdata/빅콘") # 한범 wd
setwd('C:/Users/User/Desktop/') # 혁주 wd
data_set <- read.csv('Data_set.csv',header = T, stringsAsFactors = F,
                     na.strings = c('NULL',''))

data_set = data_set[,-1] #이후 데이터 셋은 0번 고객 번호까지 삭제한 데이터셋을 활용함
str(data_set)
colSums(is.na(data_set))


#####연체 & 비 연체 그룹으로 분할#########
data_0 <- data_set[data_set$TARGET==0,]
data_1 <- data_set[data_set$TARGET==1,]

# ① Data Pre-processing---------------------------------------------

# [6] TOT_LNIF_AMT : 단위 수정
TOT_LNIF_AMT <- data_set[,6]*1000
data_set[,6] <- TOT_LNIF_AMT

# [7] TOT_CLIF_AMT : 단위 수정
TOT_CLIF_AMT <- data_set[,7]*1000
data_set[,7] <- TOT_CLIF_AMT

# [8] BNK_LNIF_AMT : 단위 수정 
BNK_LNIF_AMT <- data_set[,8]*1000
data_set[,8] <- BNK_LNIF_AMT

# [9] CPT_LNIF_AMT : 단위 수정
CPT_LNIF_AMT <- data_set[,9]*1000
data_set[,9] <- CPT_LNIF_AMT

# [15] CB_GUIF_AMT : 단위 수정
CB_GUIF_AMT <- data_set[,15]*1000
data_set[,15] <- CB_GUIF_AMT

# [16] OCCP_NAME_G : Missing value 추정

# [17] CUST_JOB_INCM : 단위 수정
CUST_JOB_INCM <- data_set[,17]*10000
data_set[,17] <- CUST_JOB_INCM

# [18] HSHD_INFR_INCM : 단위 수정
HSHD_INFR_INCM <- data_set[,18]*10000
data_set[,18] <- HSHD_INFR_INCM

# [21] LAST_CHID_AGE : Missing value 추정 


# [22] MATE_OCCP_NAME_G : Missing value 추정

# [23] MATE_JOB_INCM : 단위 수정
MATE_JOB_INCM <- data_set[,23]*10000
data_set[,23] <- MATE_JOB_INCM

# [25] MIN_CNTT_DATE : 변수 타입 전처리 
tmp1 <- substr(data_set[,25],1,4)
tmp2 <- substr(data_set[,25],5,6)
tmp3 <- rep(0,length(tmp1))
tmp3[(tmp1 != 0)] = 15
MIN_CNTT_DATE = paste(tmp1,tmp2,tmp3,sep = '-')
MIN_CNTT_DATE[MIN_CNTT_DATE == "0--0"] = 0
MIN_CNTT_DATE <-as.Date(MIN_CNTT_DATE,'%Y-%m-%d',origin = '1970-01-01')
data_set[,25] <- MIN_CNTT_DATE
table(data_set[,25])
table(MIN_CNTT_DATE) # 대출 받지 않은 사람을 '0'으로 봐야 할지 아니면 데이터 처럼 NA값으로 봐야 할지

# 가장최근가입일로 부터 기간값이 필요한 경우
b1 = as.Date('2016-04-15') #가장 최근날짜 시점
difftime_week<-as.numeric(difftime(b1, MIN_CNTT_DATE,units='weeks'))
difftime_week_t0<-as.numeric(difftime(b1, MIN_CNTT_DATE[which(TARGET==0)],units='weeks'))
difftime_week_t1<-as.numeric(difftime(b1, MIN_CNTT_DATE[which(TARGET==1)],units='weeks'))


diff_1 <-  cut(difftime_week, breaks = seq(0,900


# [28] CRLN_OVDU_RATE : 파생변수 (0 값을 갖는 관측치가 매우 높아 0과 1(연체율)의 값을 갖는 변수 생성
CRLN_OVDU_RATE_1 = data_set[,28]
CRLN_OVDU_RATE_1[CRLN_OVDU_RATE_1!= 0] = 1
CRLN_OVDU_RATE_1

# [29] CRLN_30OVDU_RATE : 파생변수 (0 값을 갖는 관측치가 매우 높아 0과 1(연체율)의 값을 갖는 변수 생성
CRLN_30OVDU_RATE_1 = data_set[,29]
CRLN_30OVDU_RATE_1[CRLN_30OVDU_RATE_1!= 0] = 1
AVG_STLN_RATE_1

# [34] LT1Y_SLOD_RATE : 변수 타입 전처리
data_set$LT1Y_PEOD_RATE<-gsub('미만','',data_set$LT1Y_PEOD_RATE)
data_set$LT1Y_PEOD_RATE<-gsub('이상','',data_set$LT1Y_PEOD_RATE)
data_set$LT1Y_PEOD_RATE<-as.numeric(data_set$LT1Y_PEOD_RATE)

# [35] AVG_STLN_RATE : 파생변수 (0 값을 갖는 관측치가 매우 높아 0과 1(연체율)의 값을 갖는 변수 생성
AVG_STLN_RATE_1 = data_set[,35]
AVG_STLN_RATE_1[AVG_STLN_RATE_1!= 0] = 1
AVG_STLN_RATE_1

# [38] LT1Y_SLOD_RATE : 파생변수 (0 값을 갖는 관측치가 매우 높아 0과 1(연체율)의 값을 갖는 변수 생성
LT1Y_SLOD_RATE = data_set[,38]
LT1Y_SLOD_RATE_1[LT1Y_SLOD_RATE_1!= 0] = 1
LT1Y_SLOD_RATE_1

# [52] AGE : 범주화
data_set[data_set[,52] == "*",52] = '0'
data_set[,52] = as.numeric(data_set[,52])
AGE_1 <-  cut(data_set$AGE, breaks = c(0,30,40,65,75),include.lowest = FALSE, right = FALSE,labels = c('*','20-35','36-60','61 이상'))
AGE_1

# [56] TEL_MBSP_GRAD : Missing value

# [57] ARPU : 변수 타입 전처리
data_set[data_set[,57] == -1,57] = 0

# [59] CBPT_MBSP_YN : 변수 타입 전처리
data_set[data_set[,59] == 'Y' ,59] = 1
data_set[data_set[,59] == 'N' ,59] = 0
data_set[,59] <- as.numeric(data_set[,59])


# [61] TEL_CNTT_QTR : 변수 타입 전처리
summary(data_set[,61])
max(data_set[,61])
min(data_set[,61])
year <- substr(as.factor(data_set[,61]),1,4)
month <- as.numeric(substr(as.factor(data_set[,61]),5,5))
month_1 = rep(0,length(month))

for (i in 1:length(month)){
  if(month[i] == 1) {
    month_1[i] = 02
  }
  else if (month[i] == 2) {
    month_1[i] = 05
  }
  else if (month[i] == 3) {
    month_1[i] = 08
  }
  else if (month[i] == 4) {
    month_1[i] = 11
  }
}
TEL_CNTT_QTR <- paste(year,month_1,'15',sep="-")
TEL_CNTT_QTR<- as.numeric(as.Date(TEL_CNTT_QTR, origin="1970-01-01"))
now <- as.numeric(as.Date('2016-8-15', origin="1970-01-01"))
for (i in 1:length(month)) {
  TEL_CNTT_QTR[i] = now - TEL_CNTT_QTR[i]
}
data_set[,61] = TEL_CNTT_QTR

# [66] PAYM_METD Missinf value

# [67] LINE_STUS : 변수 타입 전처리
data_set[data_set[,67] == 'U' ,67] = 1
data_set[data_set[,67] == 'S' ,67] = 0
data_set[,67] <- as.numeric(data_set[,67])
table(data_set[,67])

# ① Data Pre-processing 1. EDA---------------------------------------------



# Data 탐색------------------------------ <<<<<<  중요X  >>>>>>------------------------
####가족관련 변수들
#소득(본인, 배우자, 가구)
spou_income<-CUST_JOB_INCM+MATE_JOB_INCM #배우자 +나의 소득합
cor(spou_income, HSHD_INFR_INCM) #가구추정소득, 부부 소득합 상관관계 (0.576)

#본인+배우자-가구  소득 (3자 X 자녀나이)
third_income = CUST_JOB_INCM + MATE_JOB_INCM - HSHD_INFR_INCM 
third_income<-third_income[which(LAST_CHLD_AGE!='NA')] #last_child변수에서 NA값있는 행 빼버림
new_last_child<- LAST_CHLD_AGE[which(LAST_CHLD_AGE!='NA')] #NA값 제외하고 새로운 변수 생성
length(third_income)==length(new_last_child) #상관계수 구하려는 값들의 길이가 같은지 확인
cor(new_last_child,third_income) 

# 실제 가족원수 & 보험가입원수
cor(ACTL_FMLY_NUM, CUST_FMLY_NUM) #상관관계 = 0.39
prop_fam_insurance<- CUST_FMLY_NUM/ACTL_FMLY_NUM
prop_fam_insurance<-round(prop_fam_insurance, 1)

# 성별에 따른 배우자와의 소득비교
#sex = 남자/ 배우자 소득비교
summary(data_set$MATE_JOB_INCM[which(data_set$SEX==1&data_set$MATE_JOB_INCM!=0&TARGET==0)])
summary(data_set$MATE_JOB_INCM[which(data_set$SEX==1&data_set$MATE_JOB_INCM!=0&TARGET==1)])
#sex = 여자/ 배우자 소득비교
summary(data_set$MATE_JOB_INCM[which(data_set$SEX==2&data_set$MATE_JOB_INCM!=0&TARGET==0)])
summary(data_set$MATE_JOB_INCM[which(data_set$SEX==2&data_set$MATE_JOB_INCM!=0&TARGET==1)])
#sex = 남자/ "배우자가 수익이 없는경우"
length(data_set$MATE_JOB_INCM[which(data_set$SEX==1&data_set$MATE_JOB_INCM!=0)])
length(data_set$MATE_JOB_INCM[which(data_set$SEX==1&data_set$MATE_JOB_INCM==0)])
#sex = 여자/ "배우자가 수익이 없는경우"
length(data_set$MATE_JOB_INCM[which(data_set$SEX==2&data_set$MATE_JOB_INCM==0)])
length(data_set$MATE_JOB_INCM[which(data_set$SEX==2&data_set$MATE_JOB_INCM!=0)])

### 26X27 대출금액X상환금액
#26X27 전체 대출 금액중 상환 금액 비율

repay<- (TOT_REPY_AMT[which(TOT_CRLN_AMT!=0)])/(TOT_CRLN_AMT[which(TOT_CRLN_AMT!=0)]) # 계산을위해 대출금액이 0인거제외
summary(repay)

repay_t0<- (TOT_REPY_AMT[which(TOT_CRLN_AMT!=0&TARGET==0)])/(TOT_CRLN_AMT[which(TOT_CRLN_AMT!=0&TARGET==0)]) #target 0
repay_t1<- (TOT_REPY_AMT[which(TOT_CRLN_AMT!=0&TARGET==1)])/(TOT_CRLN_AMT[which(TOT_CRLN_AMT!=0&TARGET==1)]) #target 1
summary(repay_t0);summary(repay_t1)
                                           
                                           



# AGE_1 탐색적 분석
aggregate(formula = data_set[,1] ~AGE_1,data = data_set,FUN = mean) # 대출 연체 여부

aggregate(formula = data_set[,6] ~AGE_1,data = data_set,FUN = mean) # 총 대출 금액 

aggregate(formula = data_set[,17] ~AGE_1,data = data_set,FUN = mean) # 추정소득

aggregate(formula = data_set[,44] ~AGE_1,data = data_set,FUN = mean) # 기납입 보험료

# 이외의 SCI 도메인과의 관계
for (i in 2:15) {
  a <- aggregate(formula = data_set[,i] ~AGE_1,data = data_set,FUN = mean)
  print(a)
} 





# 멤버쉽 등급 전처리 (미완료)
str(data_set$TEL_MBSP_GRAD)
data_MBSP_NA <- data_set[is.na(data_set$TEL_MBSP_GRAD),] # NA값 분류
data_MBSP <- data_set[!is.na(data_set$TEL_MBSP_GRAD),]

aggregate(formula = MON_TLFE_AMT ~TEL_MBSP_GRAD,data = data_set,FUN = mean) # 평균 비교
aggregate(formula = ARPU ~ TEL_MBSP_GRAD, data = data_set,FUN = mean)


# 각 등급 summary
E <- summary(data_MBSP[data_MBSP$TEL_MBSP_GRAD == 'E',54:65]) # VIP, E
R <- summary(data_MBSP[data_MBSP$TEL_MBSP_GRAD == 'R',54:65]) #GOLD, R
W <- summary(data_MBSP[data_MBSP$TEL_MBSP_GRAD == 'W',54:65]) #SLIVER, W
Q <- summary(data_MBSP[data_MBSP$TEL_MBSP_GRAD == 'Q',54:65]) #일반, Q
par(mfrow = c(1,1))
hist(data_MBSP[data_MBSP$TEL_MBSP_GRAD == 'E',57])
hist(data_MBSP[data_MBSP$TEL_MBSP_GRAD == 'R',57])
hist(data_MBSP[data_MBSP$TEL_MBSP_GRAD == 'W',57])
hist(data_MBSP[data_MBSP$TEL_MBSP_GRAD == 'Q',57])


# 의사결정 나무로 확인
data_MBSP_rpart<- data_MBSP[,c(54,55,56,57,58,60,61,62,63,64,65)]
decision_tree<- rpart(TEL_MBSP_GRAD ~.,
                      data = data_MBSP_rpart,method = "class")
rpart.plot(decision_tree,type=2)
install.packages('randomForest')

randomF<-randomForest(TEL_MBSP_GRAD ~.,
                      data = data_MBSP_rpart,
                      ntree = 500)
