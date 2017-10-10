#library load---------------------------------------------------
library(ggplot2) ;library(dplyr) ;library(reshape2) ;library(corrplot) ;library(tree)
library(rpart) ;library(rpart.plot) ;library(randomForest) ;library(caret) ;
library(DMwR)


gc()
rm(list=ls())
#wd,readcsv ---------------------------------------------------------
setwd("C:\\Users\\hanbum\\Desktop\\Data\\Bigcontest") 
data_set <- read.csv('Data_set.csv',header = T, stringsAsFactors = F,
                     na.strings = c('NULL',''))

data_set = data_set[,-1] #이후 데이터 셋은 0번 고객 번호까지 삭제한 데이터셋을 활용함
str(data_set)

data_set1<-data_set

#####연체 & 비 연체 그룹으로 분할#########
data_0 <- data_set[data_set$TARGET==0,]
data_1 <- data_set[data_set$TARGET==1,]

# ① Data Pre-processing [1.기본]---------------------------------------------

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
data_set[data_set[,25] !=0 ,25] = 1
data_set[data_set[,25] == 0 ,25] = 0
data_set$MIN_CNTT_DATE<-as.factor(data_set$MIN_CNTT_DATE)
table(data_set$MIN_CNTT_DATE)


####BGCON_CLAIM_DATA.TRAIN$RECP_DATE_YEAR <- as.numeric(format(as.Date(as.character(BGCON_CLAIM_DATA.TRAIN$RECP_DATE), format = "%Y%m%d"), "%Y"))
# [28] CRLN_OVDU_RATE : 파생변수 (0 값을 갖는 관측치가 매우 높아 0과 1(연체율)의 값을 갖는 변수 생성
CRLN_OVDU_RATE_1 = data_set[,28]
CRLN_OVDU_RATE_1[CRLN_OVDU_RATE_1!= 0] = 1
CRLN_OVDU_RATE_1

# [29] CRLN_30OVDU_RATE : 파생변수 (0 값을 갖는 관측치가 매우 높아 0과 1(연체율)의 값을 갖는 변수 생성
CRLN_30OVDU_RATE_1 = data_set[,29]
CRLN_30OVDU_RATE_1[CRLN_30OVDU_RATE_1!= 0] = 1

# [34] LT1Y_SLOD_RATE : 변수 타입 전처리
data_set$LT1Y_PEOD_RATE<-gsub('미만','',data_set$LT1Y_PEOD_RATE)
data_set$LT1Y_PEOD_RATE<-gsub('이상','',data_set$LT1Y_PEOD_RATE)
data_set$LT1Y_PEOD_RATE<-as.numeric(data_set$LT1Y_PEOD_RATE)

# [35] AVG_STLN_RATE : 파생변수 (0 값을 갖는 관측치가 매우 높아 0과 1(연체율)의 값을 갖는 변수 생성
AVG_STLN_RATE_1 = data_set[,35]
AVG_STLN_RATE_1[AVG_STLN_RATE_1!= 0] = 1
AVG_STLN_RATE_1

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

# [66] PAYM_METD : Missing value

# [67] LINE_STUS : 변수 타입 전처리
data_set[data_set[,67] == 'U' ,67] = 1
data_set[data_set[,67] == 'S' ,67] = 0
data_set[,67] <- as.numeric(data_set[,67])
table(data_set[,67])

data_set1<-data_set
# ① Data Pre-processing [2.log 변환] ---------------------------------------------
# [6] TOT_LNIF_AMT : log 변환
data_set$TOT_LNIF_AMT <- log(data_set$TOT_LNIF_AMT+1)

# [7] TOT_CLIF_AMT : log 변환
data_set$TOT_CLIF_AMT <- log(data_set$TOT_CLIF_AMT + 1)

# [8] BNK_LNIF_AMT : log 변환
data_set$BNK_LNIF_AMT <- log(data_set$BNK_LNIF_AMT + 1)

# [9] CPT_LNIF_AMT : log 변환
data_set$CPT_LNIF_AMT <- log(data_set$CPT_LNIF_AMT + 1)

# [15] CB_GUIF_AMT : log 변환
data_set$CB_GUIF_AMT <- log(data_set$CB_GUIF_AMT + 1)

# [26] TOT_CRLN_AMT : log 변환
data_set$TOT_CRLN_AMT <- log(data_set$TOT_CRLN_AMT + 1)

# [27] TOT_REPY_AMT : log 변환
data_set$TOT_REPY_AMT <- log(data_set$TOT_REPY_AMT + 1)

# [36] STLN_REMN_AMT
data_set$STLN_REMN_AMT <- log(data_set$STLN_REMN_AMT + 1)

# [37] LT1Y_STLN_AMT : log 변환
data_set$LT1Y_STLN_AMT <- log(data_set$LT1Y_STLN_AMT +1)

# [39] GDINS_MON_PREM
data_set$GDINS_MON_PREM <-log(data_set$GDINS_MON_PREM +1)

# [40] SVINS_MON_PREM : log 변환
data_set$SVINS_MON_PREM <-log(data_set$SVINS_MON_PREM +1)

# [41] FMLY_GDINS_MNPREM : log 변환
data_set$FMLY_GDINS_MNPREM <- log(data_set$FMLY_GDINS_MNPREM +1)

# [42] FMLY_SVINS_MNPREM : log 변환
data_set$FMLY_SVINS_MNPREM <- log(data_set$FMLY_SVINS_MNPREM +1)

# [43] MAX_MON_PREM : log 변환
data_set$MAX_MON_PREM <- log(data_set$MAX_MON_PREM +1)

# [44] TOT_PREM : log 변환
data_set$TOT_PREM <- log(data_set$TOT_PREM +1)

# [45] FMLY_TOT_PREM : log 변환
data_set$FMLY_TOT_PREM <- log(data_set$FMLY_TOT_PREM +1)

#--------------------------------------------------------------------------------------

# 결측치 처리
str(data_set)
data_set$TEL_MBSP_GRAD <- as.factor(data_set$TEL_MBSP_GRAD)
data_set$OCCP_NAME_G  <- as.factor(data_set$OCCP_NAME_G)
data_set$SEX  <- as.factor(data_set$SEX)
data_set$PAYM_METD  <- as.factor(data_set$PAYM_METD)
data_set$MATE_OCCP_NAME_G  <- as.factor(data_set$MATE_OCCP_NAME_G)
data_set$TARGET <- as.factor(data_set$TARGET)


str(data_set)
colSums(is.na(data_set))

##############################
##########모델링##############
##############################

#데이터 나누기(8:2)
set.seed(1)
trainIndex <- createDataPartition(data_set$TARGET, p = .8)

dataTrain <- data_set[trainIndex$Resample1,]
dataTest  <- data_set[-trainIndex$Resample1,]
str(dataTrain)
str(dataTest)



####################################################
#########결측치 포함 변수 없애기#########
####################################################
nodatatrain<-dataTrain[,-c(16,21,22,56,66)]
nodatatest<-dataTest[,-c(16,21,22,56,66)]
str(nodatatrain)


#불균형 맞추기
set.seed(1)
table(nodatatrain$TARGET)
smote_train <- SMOTE(TARGET ~ ., nodatatrain, perc.over=600, perc.under = 100)
table(smote_train$TARGET)


###stepwise + logistic regression
glm.fit <- glm(TARGET ~., family = binomial, data=smote_train)
step.fit <- step(glm.fit, methods ='backward')
logipr <- predict(step.fit, newdata = nodatatest)

result <- confusionMatrix(logipr, dataTest$TARGET)


