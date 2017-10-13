#library load---------------------------------------------------
library(ggplot2) ;library(plyr) ; library(dplyr) ;library(reshape2) ;library(corrplot) ;
library(tree) ; library(rpart) ;library(rpart.plot) ;library(randomForest) ;library(caret) ;
library(DMwR) ; library(xgboost) ; library(pastecs); library(gmodels) ; library(gridExtra);
library(caret) ; library(RANN)

gc()
rm(list=ls())

# #wd,readcsv ---------------------------------------------------------
setwd("C:\\Users\\hanbum\\Desktop\\Data\\Bigcontest")
data_set <- read.csv('Data_set.csv',header = T, stringsAsFactors = F,
                     na.strings = c('NULL','','*')) ## '*' 비식별 값(16번,22번,52번,53번) NA처리###

data_set = data_set[,-1] #이후 데이터 셋은 0번 고객 번호까지 삭제한 데이터셋을 활용
str(data_set)

# ##############################
# #데이터 타입 확인 및 변경#####
# ##############################

# ##chr-->factor##########(16번,22번,52번,53번, 56번, 59번, 66번, 67번)
data_set$OCCP_NAME_G <- as.factor(data_set$OCCP_NAME_G)
data_set$MATE_OCCP_NAME_G <- as.factor(data_set$MATE_OCCP_NAME_G)
data_set$SEX <- as.factor(data_set$SEX)
data_set$CBPT_MBSP_YN <- as.factor(data_set$CBPT_MBSP_YN)
data_set$TEL_MBSP_GRAD <- as.factor(data_set$TEL_MBSP_GRAD)
data_set$PAYM_METD <- as.factor(data_set$PAYM_METD)
data_set$LINE_STUS <- as.factor(data_set$LINE_STUS)

# ##int-->factor############(1번)
data_set$TARGET <- as.factor(data_set$TARGET)


# ##'미만','이상'LT1Y_PEOD_RATE 변수 integer로 처리(34번)
data_set$LT1Y_PEOD_RATE<-gsub('미만','',data_set$LT1Y_PEOD_RATE)
data_set$LT1Y_PEOD_RATE<-gsub('이상','',data_set$LT1Y_PEOD_RATE)
data_set$LT1Y_PEOD_RATE<-as.integer(data_set$LT1Y_PEOD_RATE)
#
# ## Date 타입처리(25번, 61번)
data_set[data_set[,25] !=0 ,25] = 1
data_set[data_set[,25] == 0 ,25] = 0
data_set$MIN_CNTT_DATE<-as.factor(data_set$MIN_CNTT_DATE)

# 61번
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
data_set[,61] = as.integer(TEL_CNTT_QTR)

# [57] ARPU : 변수 타입 전처리(min값이 -1로 되어 있다)
data_set[data_set[,57] == -1,57] = 0

str(data_set)

# ########################
# ######단위수정##########
# ########################
#
# [6] TOT_LNIF_AMT : 단위 수정
data_set$TOT_LNIF_AMT <- data_set[,6]*1000

# [7] TOT_CLIF_AMT : 단위 수정
data_set$TOT_CLIF_AMT <- data_set[,7]*1000

# [8] BNK_LNIF_AMT : 단위 수정
data_set$BNK_LNIF_AMT <- data_set[,8]*1000

# [9] CPT_LNIF_AMT : 단위 수정
data_set$CPT_LNIF_AMT <- data_set[,9]*1000

# [15] CB_GUIF_AMT : 단위 수정
data_set$CB_GUIF_AMT <- data_set[,15]*1000

# [17] CUST_JOB_INCM : 단위 수정
data_set$CUST_JOB_INCM <- data_set[,17]*10000

# [18] HSHD_INFR_INCM : 단위 수정
data_set$HSHD_INFR_INCM <- data_set[,18]*10000

# [23] MATE_JOB_INCM : 단위 수정
data_set$MATE_JOB_INCM <- data_set[,23]*10000

########################
#####log변환############
########################

# [6] TOT_LNIF_AMT : log 변환
data_set$TOT_LNIF_AMT <- log(data_set$TOT_LNIF_AMT+1)
# [7] TOT_CLIF_AMT : log 변환
data_set$TOT_CLIF_AMT <- log(data_set$TOT_CLIF_AMT + 1)
# [8] BNK_LNIF_AMT : log 변환
data_set$BNK_LNIF_AMT <- log(data_set$BNK_LNIF_AMT + 1)
# [9] CPT_LNIF_AMT : log 변환
data_set$CPT_LNIF_AMT <- log(data_set$CPT_LNIF_AMT + 1)
# [12] CPT_LNIF_AMT : scale 변환
data_set$CRDT_CARD_CNT <- scale(data_set$CRDT_CARD_CNT)
# [13] CPT_LNIF_AMT : scale 변환
data_set$CTCD_OCCR_MDIF <- scale(data_set$CTCD_OCCR_MDIF)
# [15] CB_GUIF_AMT : log 변환
data_set$CB_GUIF_AMT <- log(data_set$CB_GUIF_AMT + 1)
# [17] CB_GUIF_AMT : log 변환
data_set$CUST_JOB_INCM <- log(data_set$CUST_JOB_INCM + 1)
# [18] CB_GUIF_AMT : log 변환
data_set$HSHD_INFR_INCM <- log(data_set$HSHD_INFR_INCM + 1)
# [23] CB_GUIF_AMT : log 변환
data_set$MATE_JOB_INCM <- log(data_set$MATE_JOB_INCM + 1)
# [26] TOT_CRLN_AMT : log 변환
data_set$TOT_CRLN_AMT <- log(data_set$TOT_CRLN_AMT + 1)
# [27] TOT_REPY_AMT : log 변환
data_set$TOT_REPY_AMT <- log(data_set$TOT_REPY_AMT + 1)
# [28] TOT_REPY_AMT : log 변환
data_set[,28] <- scale(data_set[,28])
# [29] TOT_REPY_AMT : scale 변환
data_set[,29] <- scale(data_set[,29])
# [30] TOT_REPY_AMT : scale 변환
data_set[,30] <- scale(data_set[,30])
# [33] TOT_REPY_AMT : scale 변환
data_set[,33] <- scale(data_set[,33])
# [34] STLN_REMN_AMT : scale 변환
data_set$LT1Y_PEOD_RATE <- scale(data_set$LT1Y_PEOD_RATE)
# [35] STLN_REMN_AMT : scale 변환
data_set$AVG_STLN_RATE <- scale(data_set$AVG_STLN_RATE)
# [36] STLN_REMN_AMT
data_set$STLN_REMN_AMT <- log(data_set$STLN_REMN_AMT + 1)
# [37] LT1Y_STLN_AMT : log 변환
data_set$LT1Y_STLN_AMT <- log(data_set$LT1Y_STLN_AMT +1)
# [38] LT1Y_STLN_AMT : scale 변환
data_set$LT1Y_SLOD_RATE <- log(data_set$LT1Y_SLOD_RATE +1)
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

data_set[,49] <- log(data_set[,49] +1)
data_set[,54] <- log(data_set[,54] +1)
data_set[,55] <- log(data_set[,55] +1)
data_set[,57] <- log(data_set[,57] +1)
data_set[,58] <- log(data_set[,58] +1)
data_set[,60] <- log(data_set[,60] +1)
data_set[,63] <- log(data_set[,63] +1)
data_set[,65] <- log(data_set[,65] +1)
data_set[,68] <- log(data_set[,68] +1)



# ##############################
# #####결측치 확인 및 입력######
# ##############################

# # ##결측치 확인
anyNA(data_set)
colSums(is.na(data_set)) #16, 21, 22, 52,56, 66번 변수에서 결측치 확인

# Impute missing values with missRanger
data_impu <- missRanger(data_set, pmm.k = 3, num.trees = 100)
colSums(is.na(data_impu))
str(data_impu)

################################
#####여기서 부터 다시 하자 #####
################################
rm(list=ls())
setwd("C:\\Users\\hanbum\\Desktop\\Data\\Bigcontest")
data_set <- read.csv('data_set2.csv',header = T, stringsAsFactors = T,
                     na.strings = c('NULL',''))
data_set<-data_set[,-1]
data_set$TARGET<-as.factor(data_set$TARGET)
data_set$SEX<-as.factor(data_set$SEX)
data_set$MIN_CNTT_DATE<-as.factor(data_set$MIN_CNTT_DATE)
str(data_set)

#####연체 & 비 연체 그룹으로 분할#########
data_0 <- data_set[data_set$TARGET==0,]
data_1 <- data_set[data_set$TARGET==1,]


#######상관계수 보자#############
data_cor<-data_set
data_cor$TARGET<-as.integer(data_cor$TARGET)
str(data_cor)
data_cor<-data_cor[,-c(16,22,25,53,56,59,66,67)]
cor1<-cor(data_cor)


########변수제거###########
data_set_eli<-data_set[,-c(12,17,18,19,20,23,26,27,28,29,30,31,32,35,36,42,49,50,54,57,58,61,16,22,25,56)]
################################
#데이터 나누기(8:2)#############
################################
set.seed(1)
trainIndex <- createDataPartition(data_set_eli$TARGET, p = .8, list=F)
dataTrain <- data_set_eli[trainIndex,]
dataTest  <- data_set_eli[-trainIndex,]
str(dataTrain)

#불균형 맞추기
set.seed(1)
#Target to target variable again
smote_train <- SMOTE(TARGET ~ ., dataTrain, perc.over=100, perc.under=200)
table(smote_train$TARGET)

str(smote_train)

#caret은 target에 0,1이 들어가있으면 돌아가지 않는다. 바꿔주자
smote_train$TARGET<- factor(smote_train$TARGET, levels= c("0", "1"), labels=c("no", "yes"))
dataTest$TARGET<-factor(dataTest$TARGET, levels= c("0", "1"), labels=c("no", "yes"))

# Set up for parallel procerssing
set.seed(1)

# Train xgboost
traincr <- trainControl(method = "repeatedcv",   # 10fold cross validation
                        number = 5,							# do 5 repititions of cv
                        summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                        classProbs=T)
# tunegrid=expand.grid(
#   .alpha=1,
#   .lambda=seq(0, 100, by = 0.1))

xgb.tune <-train(TARGET ~ .,
                 data= smote_train,
                 method="xgbTree",
                 metric="ROC",
                 trControl=traincr)



### xgboostModel Predictions and Performance
# Make predictions using the test data set
xgb.pred <- predict(xgb.tune,dataTest)

#Look at the confusion matrix  
confusionMatrix(xgb.pred,dataTest$TARGET)
confusionMatrix(xgb.pred,dataTest$TARGET)
r=252/(252+614)
f=252/(252+191)

2/{(1/r)+(1/f)}

#########################answer################################################
answersample <- sample(1:nrow(dataTest), 2020)
data_answer<-dataTest[answersample,]


xgb.pred <- predict(xgb.tune,data_answer)

#Look at the confusion matrix  
confusionMatrix(xgb.pred,data_answer$TARGET)

r=63/(63+35)
f=63/(63+124)

2/{(1/r)+(1/f)}



# #####################################
# #Stacking############################
# #####################################
# library(mlbench)
# library(caret)
# library(caretEnsemble)
# 
# inTrain<- trainControl(method="repeatedcv",number = 10,repeats=3,savePredictions = TRUE,
#                        classProbs = TRUE)
# algorithmList<- c('xgbTree','rf','gbm')
# set.seed(1)
# models<- caretList(TARGET~.,data=smote_train,trControl = inTrain,methodList = algorithmList)
# 
# results<- resamples(models)
# summary(results)
# 
# #stack using Random Forest
# stackControl<-trainControl(method="repeatedcv",number=10,repeats=3,savePredictions = TRUE,
#                            classProbs = TRUE)
# set.seed(1)
# stack.rf<- caretStack(models,method="rf",metric="ROC",trControl=stackControl)
# print(stack.rf)
# 
