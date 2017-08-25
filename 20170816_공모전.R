getwd()
setwd("G:/bigcon")
data_set <- read.csv('Data_set.csv',header = T)
head(data_set)
sum(find(data_set$LINE_STUS)) #missing value
cor.test(TARGET[(1000:100000)],ACTL_FMLY_NUM[(1000:100000)])
length(TARGET[(1000:100000)])
length(ACTL_FMLY_NUM[(1000:100000)])
ACTL_FMLY_NUM
a<- data_set$V36
#logistic
glm.fit<- glm(TARGET~a,data = data_set, family = binomial)
summary(glm.fit)
glm.probs<- predict(glm.fit, type='response')
glm.probs[0:100]
glm.pred= rep('y',100)
glm.pred[glm.probs>0.5] = 'n'
table(glm.pred, TARGET)
summary(data_set$V35)
table(data_set$V35)
hist(data_set$V35)

#outlier
a<- glm()
library('car')
rstudent(a) #
b<- oulierTest(a,n.max = )
b

###### 전체 데이터의 변수의 타입이 int/factor로 구분되어 있지만 일부 수정이 필요########

str(data_set) # 데이터 변수타입 확인
summary(data_set)
### int변수를 factor로 변경하는 작업 ###
TARGET <- as.factor(data_set[,3])
data_set[,2] <- TARGET
TARGET
BNK_LNIF_CNT <- as.factor(data_set[,3])
data_set[,3] <- BNK_LNIF_CNT

CPT_LNIF_CNT <- as.factor(data_set[,4])
data_set[,4] <- CPT_LNIF_CNT

SPART_LNIF_CNT <- as.factor(data_set[,5])
data_set[,5] <- SPART_LNIF_CNT

ECT_LNIF_CNT <- as.factor(data_set[,6])
data_set[,6] <- ECT_LNIF_CNT

CRDT_CARD_CNT <- as.factor(data_set[,13])
data_set[,13] <- CRDT_CARD_CNT
  
CB_GUIF_CNT <- as.factor(data_set[,15])
data_set[,15] <- CB_GUIF_CNT

ACTL_FMLY_NUM <- as.factor(data_set[,20])
data_set[,20] <- ACTL_FMLY_NUM

CUST_FMLY_NUM <- as.factor(data_set[,21])
data_set[,21] <- CUST_FMLY_NUM

CRDT_LOAN_CNT <- as.factor(data_set[,25])
data_set[,25] <- CRDT_LOAN_CNT

MIN_CNTT_DATE <- as.factor(data_set[,26])
data_set[,26] <- MIN_CNTT_DATE

STRT_CRDT_GRAD <- as.factor(data_set[,32])
data_set[,32] <- STRT_CRDT_GRAD 

LTST_CRDT_GRAD <- as.factor(data_set[,33])
data_set[,33] <- LTST_CRDT_GRAD 

CNTT_LAMT_CNT <- as.factor(data_set[,47])
data_set[,47] <- CNTT_LAMT_CNT 

LT1Y_CTLT_CNT <- as.factor(data_set[,48])
data_set[,48] <- LT1Y_CTLT_CNT

TLFE_UNPD_CNT <- as.factor(data_set[,65])
data_set[,65] <- TLFE_UNPD_CNT


str(data_set)  ## 제대로 변경되었는지 확인작업

######################################################################################
summary <- summary(data_set)
write.csv(summary,'data_summary.csv')


###############연체 & 비 연체 그룹으로 분할###########################################
data_0 <- data_set[data_set$TARGET==0,]
data_1 <- data_set[data_set$TARGET==1,]

summary_0 <- summary(data_0)
summary_1 <- summary(data_1)
summary(data_1)

###############변수 별 데이터 분표를 plot으로 비교####################################
par(mfrow = c(2, 1))
plot(data_0[,3])  # 정의서에서의 변수번호와 다름 (정의서에서는 타겟이 1번이지만 R안에서는 3번)
plot(data_1[,3])  # 정의서 번호 + 2 해서 확인


###############평균이 비슷한 변수 t-test##############################################
# 17. CUST_JOB_INCM t-test
mean(data_0[,19])
mean(data_1[,19])
t.test(data_0[,19],data_1[,19])
a <- sample(data_0[,19],200)
b <- sample(data_1[,19],200)

t.test(a,b)  # sample이 맞는건지 확인 필요, 다른 변수에 대해서도 확인 필요

###############################기타 확인 작업을 위한 코딩 ###########################
a <- summary(data_0[,45])
b <- summary(data_1[,45])
mean(summary(data_0$TOT_PREM))
mean(summary(data_1[,44]))
mean(summary(data_set[,45]))
summary(data_set$TOT_PREM)
data_1
write.csv(data_0,'target0.csv')
write.csv(data_1,'target1.csv')
unclass(summary(data_set$TOT_PREM))

#### 수정되는지 확인작업 ####
