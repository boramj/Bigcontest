#library load---------------------------------------------------
library(ggplot2)
library(dplyr)
library(reshape2)

#wd,readcsv ---------------------------------------------------------
getwd()
setwd("C:/Users/hanbum/Desktop/rdata/빅콘")
data_set <- read.csv('Data_set.csv',header = T, stringsAsFactors = F,
                     na.strings = c('NULL',''))
data_set = data_set[,-1] #이후 데이터 셋은 0번 고객 번호까지 삭제한 데이터셋을 활용함

#####연체 & 비 연체 그룹으로 분할#########
data_0 <- data_set[data_set$TARGET==0,]
data_1 <- data_set[data_set$TARGET==1,]

# ① Data Pre-processing---------------------------------------------
### ① Data Pre-processing 1.Value Type------------------------------
str(data_set)

### int변수를 factor로 변경하는 작업 ###
TARGET <- as.factor(data_set[,1])
data_set[,1] <- TARGET

BNK_LNIF_CNT <- as.factor(data_set[,2])
data_set[,2] <- BNK_LNIF_CNT

CPT_LNIF_CNT <- as.factor(data_set[,3])
data_set[,3] <- CPT_LNIF_CNT

SPART_LNIF_CNT <- as.factor(data_set[,4])
data_set[,4] <- SPART_LNIF_CNT

ECT_LNIF_CNT <- as.factor(data_set[,5])
data_set[,5] <- ECT_LNIF_CNT

CRDT_CARD_CNT <- as.factor(data_set[,12])
data_set[,12] <- CRDT_CARD_CNT

CB_GUIF_CNT <- as.factor(data_set[,16])
data_set[,16] <- CB_GUIF_CNT

ACTL_FMLY_NUM <- as.factor(data_set[,19])
data_set[,19] <- ACTL_FMLY_NUM

CUST_FMLY_NUM <- as.factor(data_set[,20])
data_set[,20] <- CUST_FMLY_NUM

MATE_OCCP_NAME_G <- as.factor(data_set[,22])
data_set[,22] <- MATE_OCCP_NAME_G

CRDT_LOAN_CNT <- as.factor(data_set[,24])
data_set[,24] <- CRDT_LOAN_CNT

MIN_CNTT_DATE <- as.factor(data_set[,25])
data_set[,25] <- MIN_CNTT_DATE

STRT_CRDT_GRAD <- as.factor(data_set[,31])
data_set[,31] <- STRT_CRDT_GRAD 

LTST_CRDT_GRAD <- as.factor(data_set[,32])
data_set[,32] <- LTST_CRDT_GRAD 

CNTT_LAMT_CNT <- as.factor(data_set[,46])
data_set[,46] <- CNTT_LAMT_CNT 

LT1Y_CTLT_CNT <- as.factor(data_set[,47])
data_set[,47] <- LT1Y_CTLT_CNT

TLFE_UNPD_CNT <- as.factor(data_set[,64])
data_set[,64] <- TLFE_UNPD_CNT

### ① Data Pre-processing 2.Missing Value----------------------------
colSums(is.na(data_set))  #결측값 확인


### ① Data Pre-processing 3.Data Transforming------------------------------
### SCI 금액 변수 수정 ###
TOT_LNIF_AMT <- data_set[,6]*1000
data_set[,6] <- TOT_LNIF_AMT

TOT_CLIF_AMT <- data_set[,7]*1000
data_set[,7] <- TOT_CLIF_AMT

BNK_LNIF_AMT <- data_set[,8]*1000
data_set[,8] <- BNK_LNIF_AMT

CPT_LNIF_AMT <- data_set[,9]*1000
data_set[,9] <- CPT_LNIF_AMT

CB_GUIF_AMT <- data_set[,15]*1000
data_set[,15] <- CB_GUIF_AMT

HSHD_INFR_INCM <- data_set[,18]*10000
data_set[,18] <- HSHD_INFR_INCM

MATE_JOB_INCM <- data_set[,23]*10000
data_set[,23] <- MATE_JOB_INCM


# ② Values Selection--------------------------------------------------
# ③ Classification Model ---------------------------------------------
# ④ Classification Model ---------------------------------------------
