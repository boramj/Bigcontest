#library load---------------------------------------------------
library(ggplot2) ;library(plyr) ; library(dplyr) ;library(reshape2) ;library(corrplot) ;
library(tree) ; library(rpart) ;library(rpart.plot) ;library(randomForest) ;library(caret) ;
library(DMwR) ; library(xgboost) ; library(pastecs); library(gmodels) ; library(gridExtra);
library(caret) ; library(RANN)

gc()
rm(list=ls())

# #wd,readcsv ---------------------------------------------------------
# setwd("C:\\Users\\hanbum\\Desktop\\Data\\Bigcontest") 
# data_set <- read.csv('Data_set.csv',header = T, stringsAsFactors = F,
#                      na.strings = c('NULL','')) ## '*' 비식별 값(16번,22번,52번,53번) NA처리###
# 
# data_set = data_set[,-1] #이후 데이터 셋은 0번 고객 번호까지 삭제한 데이터셋을 활용
# str(data_set)
# ##############################
# #데이터 타입 확인 및 변경#####
# ##############################
# 
# ##chr-->factor##########(16번,22번,52번,53번, 56번, 59번, 66번, 67번)
# data_set$OCCP_NAME_G <- as.factor(data_set$OCCP_NAME_G)
# data_set$MATE_OCCP_NAME_G <- as.factor(data_set$MATE_OCCP_NAME_G)
# data_set$SEX <- as.factor(data_set$SEX)
# data_set$CBPT_MBSP_YN <- as.factor(data_set$CBPT_MBSP_YN)
# data_set$TEL_MBSP_GRAD <- as.factor(data_set$TEL_MBSP_GRAD)
# data_set$PAYM_METD <- as.factor(data_set$PAYM_METD)
# data_set$LINE_STUS <- as.factor(data_set$LINE_STUS)
# 
# ##int-->factor############(1번)
# data_set$TARGET <- as.factor(data_set$TARGET)
# 
# ##chr-->integer(AGE): 비식별은 0으로 
# data_set[,52] = as.integer(data_set[,52])
# 
# 
# ##'미만','이상'LT1Y_PEOD_RATE 변수 integer로 처리(34번)
# data_set$LT1Y_PEOD_RATE<-gsub('미만','',data_set$LT1Y_PEOD_RATE)
# data_set$LT1Y_PEOD_RATE<-gsub('이상','',data_set$LT1Y_PEOD_RATE)
# data_set$LT1Y_PEOD_RATE<-as.integer(data_set$LT1Y_PEOD_RATE)
# 
# ## Date 타입처리(25번, 61번)
# data_set[data_set[,25] !=0 ,25] = 1
# data_set[data_set[,25] == 0 ,25] = 0
# data_set$MIN_CNTT_DATE<-as.factor(data_set$MIN_CNTT_DATE)
# 
# # 61번
# summary(data_set[,61])
# max(data_set[,61])
# min(data_set[,61])
# year <- substr(as.factor(data_set[,61]),1,4)
# month <- as.numeric(substr(as.factor(data_set[,61]),5,5))
# month_1 = rep(0,length(month))
# 
# for (i in 1:length(month)){
#   if(month[i] == 1) {
#     month_1[i] = 02
#   }
#   else if (month[i] == 2) {
#     month_1[i] = 05
#   }
#   else if (month[i] == 3) {
#     month_1[i] = 08
#   }                             
#   else if (month[i] == 4) {
#     month_1[i] = 11
#   }
# }
# TEL_CNTT_QTR <- paste(year,month_1,'15',sep="-")
# TEL_CNTT_QTR<- as.numeric(as.Date(TEL_CNTT_QTR, origin="1970-01-01"))
# now <- as.numeric(as.Date('2016-8-15', origin="1970-01-01"))
# 
# for (i in 1:length(month)) {
#   TEL_CNTT_QTR[i] = now - TEL_CNTT_QTR[i]
# }
# data_set[,61] = as.integer(TEL_CNTT_QTR)
# 
# # [57] ARPU : 변수 타입 전처리(min값이 -1로 되어 있다)
# data_set[data_set[,57] == -1,57] = 0
# 
# str(data_set)
# ##############################
# #####결측치 확인 및 입력######
# ##############################
# 
# # ##결측치 확인
# anyNA(data_set)
# colSums(is.na(data_set)) #16, 21, 22, 52,56, 66번 변수에서 결측치 확인
# 
# # Impute missing values with missRanger
# data_impu <- missRanger(data_set, pmm.k = 3, num.trees = 100)
# colSums(is.na(data_impu))


# # ① Data Pre-processing [1.기본]---------------------------------------------
# 
# ########################
# ######단위수정##########
# ########################
# 
# # [6] TOT_LNIF_AMT : 단위 수정
# data_impu$TOT_LNIF_AMT <- data_impu[,6]*1000
# 
# # [7] TOT_CLIF_AMT : 단위 수정
# data_impu$TOT_CLIF_AMT <- data_impu[,7]*1000
# 
# # [8] BNK_LNIF_AMT : 단위 수정 
# data_impu$BNK_LNIF_AMT <- data_impu[,8]*1000
# 
# # [9] CPT_LNIF_AMT : 단위 수정
# data_impu$CPT_LNIF_AMT <- data_impu[,9]*1000
# 
# # [15] CB_GUIF_AMT : 단위 수정
# data_impu$CB_GUIF_AMT <- data_impu[,15]*1000
# 
# # [17] CUST_JOB_INCM : 단위 수정
# data_impu$CUST_JOB_INCM <- data_impu[,17]*10000
# 
# # [18] HSHD_INFR_INCM : 단위 수정
# data_impu$HSHD_INFR_INCM <- data_impu[,18]*10000
# 
# # [23] MATE_JOB_INCM : 단위 수정
# data_impu$MATE_JOB_INCM <- data_impu[,23]*10000
# 
# 


#####여기서 부터 다시 하자 
data_set <- read.csv('data_impute.csv',header = T, stringsAsFactors = T,
                     na.strings = c('NULL',''))
data_set<-data_set[,-1]
data_set$SEX<-as.factor(data_set$SEX)
data_set$TARGET<-as.factor(data_set$TARGET)

str(data_set)

#####연체 & 비 연체 그룹으로 분할#########
data_0 <- data_set[data_set$TARGET==0,]
data_1 <- data_set[data_set$TARGET==1,]

#############################
######log변환(보류)##########
#############################

# # [6] TOT_LNIF_AMT : log 변환
# data_set$TOT_LNIF_AMT <- log(data_set$TOT_LNIF_AMT+1)
# 
# # [7] TOT_CLIF_AMT : log 변환
# data_set$TOT_CLIF_AMT <- log(data_set$TOT_CLIF_AMT + 1)
# 
# # [8] BNK_LNIF_AMT : log 변환
# data_set$BNK_LNIF_AMT <- log(data_set$BNK_LNIF_AMT + 1)
# 
# # [9] CPT_LNIF_AMT : log 변환
# data_set$CPT_LNIF_AMT <- log(data_set$CPT_LNIF_AMT + 1)
# 
# # [15] CB_GUIF_AMT : log 변환
# data_set$CB_GUIF_AMT <- log(data_set$CB_GUIF_AMT + 1)
# 
# # [26] TOT_CRLN_AMT : log 변환
# data_set$TOT_CRLN_AMT <- log(data_set$TOT_CRLN_AMT + 1)
# 
# # [27] TOT_REPY_AMT : log 변환
# data_set$TOT_REPY_AMT <- log(data_set$TOT_REPY_AMT + 1)
# 
# # [36] STLN_REMN_AMT
# data_set$STLN_REMN_AMT <- log(data_set$STLN_REMN_AMT + 1)
# 
# # [37] LT1Y_STLN_AMT : log 변환
# data_set$LT1Y_STLN_AMT <- log(data_set$LT1Y_STLN_AMT +1)
# 
# # [39] GDINS_MON_PREM
# data_set$GDINS_MON_PREM <-log(data_set$GDINS_MON_PREM +1)
# 
# # [40] SVINS_MON_PREM : log 변환
# data_set$SVINS_MON_PREM <-log(data_set$SVINS_MON_PREM +1)
# 
# # [41] FMLY_GDINS_MNPREM : log 변환
# data_set$FMLY_GDINS_MNPREM <- log(data_set$FMLY_GDINS_MNPREM +1)
# 
# # [42] FMLY_SVINS_MNPREM : log 변환
# data_set$FMLY_SVINS_MNPREM <- log(data_set$FMLY_SVINS_MNPREM +1)
# 
# # [43] MAX_MON_PREM : log 변환
# data_set$MAX_MON_PREM <- log(data_set$MAX_MON_PREM +1)
# 
# # [44] TOT_PREM : log 변환
# data_set$TOT_PREM <- log(data_set$TOT_PREM +1)
# 
# # [45] FMLY_TOT_PREM : log 변환
# data_set$FMLY_TOT_PREM <- log(data_set$FMLY_TOT_PREM +1)

#################################################
#feature engineering#############################
#################################################

####category reduction(16번,22번,)#############

################################
#데이터 나누기(9:1)#############
################################
set.seed(1)
trainIndex <- createDataPartition(data_set$TARGET, p = .9, list=F)

dataTrain <- data_set[trainIndex,]
dataTest  <- data_set[-trainIndex,]

#불균형 맞추기
set.seed(1)
smote_train <- SMOTE(TARGET ~ ., dataTrain, perc.over=600, perc.under =300)
table(smote_train$TARGET)


######xgboost##############
#caret은 target에 0,1이 들어가있으면 돌아가지 않는다. 바꿔주자
smote_train$TARGET<- factor(smote_train$TARGET, levels= c("0", "1"), labels=c("no", "yes"))
dataTest$TARGET<-factor(dataTest$TARGET, levels= c("0", "1"), labels=c("no", "yes"))

# Set up for parallel procerssing
set.seed(1)

# Train xgboost
traincr <- trainControl(method = "repeatedcv",   # 10fold cross validation
                        number = 5,							# do 5 repititions of cv
                        summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                        classProbs=TRUE,
                        allowParallel = TRUE)


xgb.tune <-train(TARGET~., data= smote_train,
                 method="xgbTree",
                 metric="ROC",
                 trControl=traincr)


xgb.tune$bestTune
plot(xgb.tune)  		# Plot the performance of the training models
res <- xgb.tune$results
res

### xgboostModel Predictions and Performance
# Make predictions using the test data set
xgb.pred <- predict(xgb.tune,nodatatest)

#Look at the confusion matrix  
confusionMatrix(xgb.pred,nodatatest$TARGET)
