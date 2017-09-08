#library load---------------------------------------------------
library(ggplot2)
library(dplyr)
library(reshape2)
library(corrplot)

#wd,readcsv ---------------------------------------------------------
setwd("C:/Users/hanbum/Desktop/rdata/빅콘")
data_set <- read.csv('Data_set.csv',header = T, stringsAsFactors = F,
                     na.strings = c('NULL',''))

data_set = data_set[,-1] #이후 데이터 셋은 0번 고객 번호까지 삭제한 데이터셋을 활용함
str(data_set)
colSums(is.na(data_set))


#####연체 & 비 연체 그룹으로 분할#########
data_0 <- data_set[data_set$TARGET==0,]
data_1 <- data_set[data_set$TARGET==1,]

# ① Data Pre-processing---------------------------------------------
# ① Data Pre-processing 1. EDA---------------------------------------------
str(data_set)
names(data_set)

summary(data_set$MOBL_PRIN)
summary(data_0$MOBL_PRIN)
summary(data_1$MOBL_PRIN)

table(data_set$NUM_DAY_SUSP)
table(data_0$NUM_DAY_SUSP)
table(data_1$NUM_DAY_SUSP)

boxplot(data_set$MOBL_PRIN~data_set$TARGET)

barplot(table(data_set$NUM_DAY_SUSP,data_set$TARGET), beside = T)

hist(data_set$MOBL_PRIN)
hist(log10(data_set$AVG_CALL_FREQ))


## rate 비율 맞추기 #########################################
# 28번 변수(신용대출연체율)  CRLN_OVDU_RATE 
# 29번 변수(30일이내신용대출연체율)   CRLN_30OVDU_RATE
# 30번 변수(최근1년신용대출연체율)   LT1Y_CLOD_RATE
# 33번 변수(보험료연체율)    PREM_OVDU_RATE
# 34번 변수(최근1년보험료연체율)   LT1Y_PEOD_RATE
# 35번 변수(평균약대율)    AVG_STLN_RATE
# 38번 변수(최근1년약대연체율)      LT1Y_SLOD_RATE


table(data_set$CRLN_OVDU_RATE)
table(data_set$CRLN_30OVDU_RATE)
table(data_set$LT1Y_CLOD_RATE)
table(data_set$PREM_OVDU_RATE)
table(data_set$LT1Y_PEOD_RATE)
table(data_set$AVG_STLN_RATE)
table(data_set$LT1Y_SLOD_RATE)



data_set$LT1Y_PEOD_RATE<-gsub('미만','',data_set$LT1Y_PEOD_RATE)
data_set$LT1Y_PEOD_RATE<-gsub('이상','',data_set$LT1Y_PEOD_RATE)
data_set$LT1Y_PEOD_RATE<-as.numeric(data_set$LT1Y_PEOD_RATE)
table(data_set$LT1Y_PEOD_RATE)

#######date 변수 맞추기
table(data_set$MIN_CNTT_DATE)
unique(data_set$MIN_CNTT_DATE)

data_set %>%
  select(MIN_CNTT_DATE) %>%
  filter(MIN_CNTT_DATE!=1)

table(data_set$TEL_CNTT_QTR)
unique(data_set$TEL_CNTT_QTR)

str(data_set)
########missing value handling


