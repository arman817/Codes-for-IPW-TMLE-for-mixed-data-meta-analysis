#################################################################################
#Define the variables
#################################################################################

library(readxl)
library(mice)
library(car)
library(caret)
library(xlsx)

TMLE_Stage2_BothCorrect_EMB_Jackknife = c()
TMLE_Stage2_Correct_G_EMB_Jackknife = c()
TMLE_Stage2_Correct_Q_EMB_Jackknife = c()
TMLE_Stage2_BothInorrect_EMB_Jackknife = c()

TMLE_Stage2_BothCorrect_EMB_New_Jackknife = c()
TMLE_Stage2_Correct_G_EMB_New_Jackknife = c()
TMLE_Stage2_Correct_Q_EMB_New_Jackknife = c()
TMLE_Stage2_BothInorrect_EMB_New_Jackknife = c()

TMLE_Estimate_EMB_Transport_Jackknife = c()
TMLE_Estimate_EMB_Jackknife = c()
GLM_Estimate_Incorrect_EMB_Jackknife = c()

TMLE_Stage2_BothCorrect_AMK_Jackknife = c()
TMLE_Stage2_Correct_G_AMK_Jackknife = c()
TMLE_Stage2_Correct_Q_AMK_Jackknife = c()
TMLE_Stage2_BothInorrect_AMK_Jackknife = c()

TMLE_Stage2_BothCorrect_AMK_New_Jackknife = c()
TMLE_Stage2_Correct_G_AMK_New_Jackknife = c()
TMLE_Stage2_Correct_Q_AMK_New_Jackknife = c()
TMLE_Stage2_BothInorrect_AMK_New_Jackknife = c()
TMLE_Estimate_AMK_Transport_Jackknife = c()

TMLE_Estimate_AMK_Jackknife = c()
GLM_Estimate_Incorrect_AMK_Jackknife = c()


TMLE_Stage2_BothCorrect_CAP_Jackknife = c()
TMLE_Stage2_Correct_G_CAP_Jackknife = c()
TMLE_Stage2_Correct_Q_CAP_Jackknife = c()
TMLE_Stage2_BothInorrect_CAP_Jackknife = c()

TMLE_Stage2_BothCorrect_CAP_New_Jackknife = c()
TMLE_Stage2_Correct_G_CAP_New_Jackknife = c()
TMLE_Stage2_Correct_Q_CAP_New_Jackknife = c()
TMLE_Stage2_BothInorrect_CAP_New_Jackknife = c()
TMLE_Estimate_CAP_Transport_Jackknife = c()

TMLE_Estimate_CAP_Jackknife = c()
GLM_Estimate_Incorrect_CAP_Jackknife = c()

TMLE_Stage2_BothCorrect_CIP_Jackknife = c()
TMLE_Stage2_Correct_G_CIP_Jackknife = c()
TMLE_Stage2_Correct_Q_CIP_Jackknife = c()
TMLE_Stage2_BothInorrect_CIP_Jackknife = c()

TMLE_Stage2_BothCorrect_CIP_New_Jackknife = c()
TMLE_Stage2_Correct_G_CIP_New_Jackknife = c()
TMLE_Stage2_Correct_Q_CIP_New_Jackknife = c()
TMLE_Stage2_BothInorrect_CIP_New_Jackknife = c()
TMLE_Estimate_CIP_Transport_Jackknife = c()

TMLE_Estimate_CIP_Jackknife = c()
GLM_Estimate_Incorrect_CIP_Jackknife = c()

TMLE_Stage2_BothCorrect_CS_Jackknife = c()
TMLE_Stage2_Correct_G_CS_Jackknife = c()
TMLE_Stage2_Correct_Q_CS_Jackknife = c()
TMLE_Stage2_BothInorrect_CS_Jackknife = c()

TMLE_Stage2_BothCorrect_CS_New_Jackknife = c()
TMLE_Stage2_Correct_G_CS_New_Jackknife = c()
TMLE_Stage2_Correct_Q_CS_New_Jackknife = c()
TMLE_Stage2_BothInorrect_CS_New_Jackknife = c()
TMLE_Estimate_CS_Transport_Jackknife = c()

TMLE_Estimate_CS_Jackknife = c()
GLM_Estimate_Incorrect_CS_Jackknife = c()

TMLE_Stage2_BothCorrect_ETO_Jackknife = c()
TMLE_Stage2_Correct_G_ETO_Jackknife = c()
TMLE_Stage2_Correct_Q_ETO_Jackknife = c()
TMLE_Stage2_BothInorrect_ETO_Jackknife = c()

TMLE_Stage2_BothCorrect_ETO_New_Jackknife = c()
TMLE_Stage2_Correct_G_ETO_New_Jackknife = c()
TMLE_Stage2_Correct_Q_ETO_New_Jackknife = c()
TMLE_Stage2_BothInorrect_ETO_New_Jackknife = c()
TMLE_Estimate_ETO_Transport_Jackknife = c()

TMLE_Estimate_ETO_Jackknife = c()
GLM_Estimate_Incorrect_ETO_Jackknife = c()


TMLE_Stage2_BothCorrect_OFX_Jackknife = c()
TMLE_Stage2_Correct_G_OFX_Jackknife = c()
TMLE_Stage2_Correct_Q_OFX_Jackknife = c()
TMLE_Stage2_BothInorrect_OFX_Jackknife = c()

TMLE_Stage2_BothCorrect_OFX_New_Jackknife = c()
TMLE_Stage2_Correct_G_OFX_New_Jackknife = c()
TMLE_Stage2_Correct_Q_OFX_New_Jackknife = c()
TMLE_Stage2_BothInorrect_OFX_New_Jackknife = c()
TMLE_Estimate_OFX_Transport_Jackknife = c()

TMLE_Estimate_OFX_Jackknife = c()
GLM_Estimate_Incorrect_OFX_Jackknife = c()

TMLE_Stage2_BothCorrect_PAS_Jackknife = c()
TMLE_Stage2_Correct_G_PAS_Jackknife = c()
TMLE_Stage2_Correct_Q_PAS_Jackknife = c()
TMLE_Stage2_BothInorrect_PAS_Jackknife = c()

TMLE_Stage2_BothCorrect_PAS_New_Jackknife = c()
TMLE_Stage2_Correct_G_PAS_New_Jackknife = c()
TMLE_Stage2_Correct_Q_PAS_New_Jackknife = c()
TMLE_Stage2_BothInorrect_PAS_New_Jackknife = c()
TMLE_Estimate_PAS_Transport_Jackknife = c()

TMLE_Estimate_PAS_Jackknife = c()
GLM_Estimate_Incorrect_PAS_Jackknife = c()


TMLE_Stage2_BothCorrect_PTO_Jackknife = c()
TMLE_Stage2_Correct_G_PTO_Jackknife = c()
TMLE_Stage2_Correct_Q_PTO_Jackknife = c()
TMLE_Stage2_BothInorrect_PTO_Jackknife = c()

TMLE_Stage2_BothCorrect_PTO_New_Jackknife = c()
TMLE_Stage2_Correct_G_PTO_New_Jackknife = c()
TMLE_Stage2_Correct_Q_PTO_New_Jackknife = c()
TMLE_Stage2_BothInorrect_PTO_New_Jackknife = c()
TMLE_Estimate_PTO_Transport_Jackknife = c()

TMLE_Estimate_PTO_Jackknife = c()
GLM_Estimate_Incorrect_PTO_Jackknife = c()


TMLE_Stage2_BothCorrect_RIF_Jackknife = c()
TMLE_Stage2_Correct_G_RIF_Jackknife = c()
TMLE_Stage2_Correct_Q_RIF_Jackknife = c()
TMLE_Stage2_BothInorrect_RIF_Jackknife = c()

TMLE_Stage2_BothCorrect_RIF_New_Jackknife = c()
TMLE_Stage2_Correct_G_RIF_New_Jackknife = c()
TMLE_Stage2_Correct_Q_RIF_New_Jackknife = c()
TMLE_Stage2_BothInorrect_RIF_New_Jackknife = c()
TMLE_Estimate_RIF_Transport_Jackknife = c()

TMLE_Estimate_RIF_Jackknife = c()
GLM_Estimate_Incorrect_RIF_Jackknife = c()

TMLE_Stage2_BothCorrect_SM_Jackknife = c()
TMLE_Stage2_Correct_G_SM_Jackknife = c()
TMLE_Stage2_Correct_Q_SM_Jackknife = c()
TMLE_Stage2_BothInorrect_SM_Jackknife = c()

TMLE_Stage2_BothCorrect_SM_New_Jackknife = c()
TMLE_Stage2_Correct_G_SM_New_Jackknife = c()
TMLE_Stage2_Correct_Q_SM_New_Jackknife = c()
TMLE_Stage2_BothInorrect_SM_New_Jackknife = c()
TMLE_Estimate_SM_Transport_Jackknife = c()

TMLE_Estimate_SM_Jackknife = c()
GLM_Estimate_Incorrect_SM_Jackknife = c()


TMLE_Stage2_BothCorrect_PZA_Jackknife = c()
TMLE_Stage2_Correct_G_PZA_Jackknife = c()
TMLE_Stage2_Correct_Q_PZA_Jackknife = c()
TMLE_Stage2_BothInorrect_PZA_Jackknife = c()

TMLE_Stage2_BothCorrect_PZA_New_Jackknife = c()
TMLE_Stage2_Correct_G_PZA_New_Jackknife = c()
TMLE_Stage2_Correct_Q_PZA_New_Jackknife = c()
TMLE_Stage2_BothInorrect_PZA_New_Jackknife = c()
TMLE_Estimate_PZA_Transport_Jackknife = c()

TMLE_Estimate_PZA_Jackknife = c()
GLM_Estimate_Incorrect_PZA_Jackknife = c()

TMLE_Stage2_BothCorrect_KMAMK_Jackknife = c()
TMLE_Stage2_Correct_G_KMAMK_Jackknife = c()
TMLE_Stage2_Correct_Q_KMAMK_Jackknife = c()
TMLE_Stage2_BothInorrect_KMAMK_Jackknife = c()

TMLE_Stage2_BothCorrect_KMAMK_New_Jackknife = c()
TMLE_Stage2_Correct_G_KMAMK_New_Jackknife = c()
TMLE_Stage2_Correct_Q_KMAMK_New_Jackknife = c()
TMLE_Stage2_BothInorrect_KMAMK_New_Jackknife = c()
TMLE_Estimate_KMAMK_Transport_Jackknife = c()

TMLE_Estimate_KMAMK_Jackknife = c()
GLM_Estimate_Incorrect_KMAMK_Jackknife = c()


TMLE_Stage2_BothCorrect_Highquin_Jackknife = c()
TMLE_Stage2_Correct_G_Highquin_Jackknife = c()
TMLE_Stage2_Correct_Q_Highquin_Jackknife = c()
TMLE_Stage2_BothInorrect_Highquin_Jackknife = c()

TMLE_Stage2_BothCorrect_Highquin_New_Jackknife = c()
TMLE_Stage2_Correct_G_Highquin_New_Jackknife = c()
TMLE_Stage2_Correct_Q_Highquin_New_Jackknife = c()
TMLE_Stage2_BothInorrect_Highquin_New_Jackknife = c()
TMLE_Estimate_Highquin_Transport_Jackknife = c()

TMLE_Estimate_Highquin_Jackknife = c()
GLM_Estimate_Incorrect_Highquin_Jackknife = c()

TMLE_Stage2_BothCorrect_Group5_Jackknife = c()
TMLE_Stage2_Correct_G_Group5_Jackknife = c()
TMLE_Stage2_Correct_Q_Group5_Jackknife = c()
TMLE_Stage2_BothInorrect_Group5_Jackknife = c()

TMLE_Stage2_BothCorrect_Group5_New_Jackknife = c()
TMLE_Stage2_Correct_G_Group5_New_Jackknife = c()
TMLE_Stage2_Correct_Q_Group5_New_Jackknife = c()
TMLE_Stage2_BothInorrect_Group5_New_Jackknife = c()
TMLE_Estimate_Group5_Transport_Jackknife = c()

TMLE_Estimate_Group5_Jackknife = c()
GLM_Estimate_Incorrect_Group5_Jackknife = c()

#################################################################################

#################################################################################
#Define Pi
#################################################################################
setwd("/Users/Arman/Desktop/RA")
read_data_IPD = read_excel("IPD & AD categorization.xlsx",sheet = "IPD Data")
read_data_IPD$`Starting enrollment year` = as.numeric(read_data_IPD$`Starting enrollment year`)
read_data_IPD$`Sex Ratio (Female)` = as.numeric(read_data_IPD$`Sex Ratio (Female)`)
read_data_IPD$`Average or Median age` = as.numeric(read_data_IPD$`Average or Median age`)
read_data_IPD$`Percent of the cohort with a history of previous tuberculosis treatment` = as.numeric(
  read_data_IPD$`Percent of the cohort with a history of previous tuberculosis treatment`
)
read_data_IPD$`Percent of the cohort that was cured` = as.numeric(read_data_IPD$`Percent of the cohort that was cured`)
read_data_IPD$`Percent of the cohort with successful outcome (cure or treatment complete)` = as.numeric(
  read_data_IPD$`Percent of the cohort with successful outcome (cure or treatment complete)`
)
read_data_IPD$`Percent of the cohort that died` = as.numeric(read_data_IPD$`Percent of the cohort that died`)
read_data_IPD$Type = 1

req_data_IPD = read_data_IPD[,c(3,5,6,7,8,10,11,12,13,14,15)]
req_data_IPD = data.frame(req_data_IPD)

read_data_AD = read_excel("IPD & AD categorization.xlsx",sheet = "Aggregate Data")
read_data_AD$`Starting enrollment year` = as.numeric(read_data_AD$`Starting enrollment year`)
read_data_AD$`Sex Ratio (Female)` = as.numeric(read_data_AD$`Sex Ratio (Female)`)
read_data_AD$`Average or Median age` = as.numeric(read_data_AD$`Average or Median age`)
read_data_AD$`Percent of the cohort with a history of previous tuberculosis treatment` = as.numeric(
  read_data_AD$`Percent of the cohort with a history of previous tuberculosis treatment`
)
read_data_AD$`Percent of the cohort that was cured` = as.numeric(read_data_AD$`Percent of the cohort that was cured`)
read_data_AD$`Percent of the cohort with successful outcome (cure or treatment complete)` = as.numeric(
  read_data_AD$`Percent of the cohort with successful outcome (cure or treatment complete)`
)
read_data_AD$`Percent of the cohort that died` = as.numeric(read_data_AD$`Percent of the cohort that died`)
read_data_AD$Type = 0
req_data_AD = read_data_AD[,c(3,5,6,7,8,10,11,12,13,14,15)]
req_data_AD = data.frame(req_data_AD)


req_data = rbind(req_data_IPD,req_data_AD)
set.seed(3462) #different number for each draw
m1 = mice(req_data,m=1,maxit=20,printFlag = F) #can use default method
dat1 = complete(m1)
#randomized_data =  dat1[sample(nrow(dat1)),]
randomized_data = dat1
randomized_data = randomized_data[,-c(6,8)]
randomized_data$Starting.enrollment.year = scale(randomized_data$Starting.enrollment.year, center = TRUE, scale = TRUE)
randomized_data$N..Patients = scale(randomized_data$N..Patients, center = TRUE, scale = TRUE)
randomized_data$Sex.Ratio..Female. = scale(randomized_data$Sex.Ratio..Female., center = TRUE, scale = TRUE)
randomized_data$Average.or.Median.age = scale(randomized_data$Average.or.Median.age, center = TRUE, scale = TRUE)
randomized_data = data.frame(randomized_data)
Pi_Data = randomized_data
For_AD = dat1

#################################################################################


#################################################################################
#Usual Functions
#################################################################################
Func1 <- function(Cov,A,Y){
  data1 <- data.frame(Cov,Y,A)
  data2 <- data1[which(data1$A==1),]
  data3 = subset(data2,select=-c(A))
  model <- glm(Y~.,data=data3,family="binomial")
  prediction <- predict.glm(model,type="response",newdata=data1)
  return(prediction)
}

Func2 <- function(Cov,A){
  data <- data.frame(Cov,A)
  model <- glm(A~.,data=data,family="binomial")
  prediction <- predict.glm(model,type="response",newdata=data)
  return(prediction)
}

Func3 <- function(A,Y){
  data1 <- data.frame(Y,A)
  data2 <- data1[which(data1$A==1),]
  model <- glm(Y~1,data=data2,family="binomial")
  prediction <- predict.glm(model,type="response",newdata=data1)
  return(prediction)
}

Func4 <- function(Y,Q,pi,A){
  pi.tronc<-pi
  # if((1/min(pi.tronc))>200){
  #   q<-quantile(pi,c(0.01,1))
  #   pi.tronc[pi<q[1]]<-q[1]
  #   pi.tronc[pi>q[2]]<-q[2]
  # }
  weight <- 1/pi
  data <- data.frame(weight,Q)
  data1 <- data.frame(weight,Q,Y,A)
  data2 <- data1[which(data1$A == 1),]
  h <- data2$weight
  #Update Step in the regression
  model <- glm(Y ~ -1 + h ,offset = qlogis(Q),data = data2,family = "binomial")
  #Predict Step
  prediction <- plogis(qlogis(Q) + coef(model)["h"]/pi)
  #prediction <- predict.glm(model, type = "response",newdata=data) #Q_star
  return(prediction)
}

Func5 <- function(Cov,A){
  data <- data.frame(Cov,A)
  model <- glm(A~.,data=data,family="binomial")
  prediction <- predict.glm(model,type="response",newdata=data)
  return(prediction)
}

Func6 <- function(Cov,A,A_ind){
  data1 <- data.frame(Cov,A,A_ind)
  data2 <- data1[which(data1$A_ind==1),]
  model <- glm(A~.,data=data2,family="binomial")
  prediction <- predict.glm(model,type="response",newdata=data1)
  return(prediction)
}

Func7 <- function(Cov,A,A_ind){
  data1 <- data.frame(Cov,A,A_ind)
  data2 <- data1[which(data1$A_ind==1),]
  model <- glm(A~1,data=data2,family="binomial")
  prediction <- predict.glm(model,type="response",newdata=data1)
  return(prediction)
}
#################################################################################



#################################################################################
#IPW Functions
#################################################################################
Func1_IPW <- function(Cov,A,Y,Kao){
  data1 <- data.frame(Cov,Y,A,Kao)
  data2 <- data1[which(data1$A==1),]
  data3 <- subset(data2,select=-c(Kao,A))
  model <- glm(Y~., data = data3, weights = data2$Kao,family="binomial")
  prediction <- predict.glm(model,type="response",newdata=data1)
  return(prediction)
}


Func3_IPW <- function(A,Y){
  data1 <- data.frame(Y,A)
  data2 <- data1[which(data1$A==1),]
  model <- glm(Y~1,data=data2,family="binomial")
  prediction <- predict.glm(model,type="response",newdata=data1)
  return(prediction)
}

Func4_IPW <- function(Y,Q,pi,A,Kao){
  pi.tronc<-pi
  # if((1/min(pi.tronc))>200){
  #   q<-quantile(pi,c(0.01,1))
  #   pi.tronc[pi<q[1]]<-q[1]
  #   pi.tronc[pi>q[2]]<-q[2]
  # }
  weight <- 1/pi
  data <- data.frame(weight,Q,Kao)
  data1 <- data.frame(weight,Q,Y,A,Kao)
  data2 <- data1[which(data1$A == 1),]
  h <- data2$weight
  #Update Step in the regression
  model <- glm(Y ~ -1 + h ,offset = qlogis(Q),data = data2, weights = data2$Kao, family = "binomial")
  #Predict Step
  prediction <- plogis(qlogis(Q) + coef(model)["h"]/pi)
  #prediction <- predict.glm(model, type = "response",newdata=data) #Q_star
  return(prediction)
}

Func5_IPW <- function(Cov,A,Kao){
  data1 <- data.frame(Cov,A)
  model <- bayesglm(A~.,data=data1,weights = Kao,family="binomial")
  prediction <- predict.glm(model,type="response",newdata=data1)
  return(prediction)
}


Func6_IPW <- function(Cov,A,A_ind,Kao){
  data1 <- data.frame(Cov,A,A_ind,Kao)
  data2 <- data1[which(data1$A_ind==1),]
  data3 <- subset(data2,select=-c(Kao,A_ind))
  model <- glm(A~.,data=data3,weights = data2$Kao,family="binomial")
  prediction <- predict.glm(model,type="response",newdata=data1)
  return(prediction)
}

Func7_IPW <- function(Cov,A,A_ind){
  data1 <- data.frame(Cov,A,A_ind)
  data2 <- data1[which(data1$A_ind==1),]
  model <- glm(A~1,data=data2,family="binomial")
  prediction <- predict.glm(model,type="response",newdata=data1)
  return(prediction)
}

#################################################################################


#################################################################################
#Read Data
#################################################################################



library(xlsx)
setwd("/Users/arman/Desktop/RA/Data")
data_read = read.xlsx(file = "Data_Analysis_Pediatric_Study.xlsx",sheetName = "Sheet1",header=TRUE)
data_read$Year = as.numeric(scale(data_read$Year, center = TRUE, scale = TRUE))
data_read$Age = as.numeric(scale(data_read$Age, center = TRUE, scale = TRUE))
#################################################################################


for(kl in 1:64){
  randomized_data = Pi_Data[-kl,]
  Rem_AD = For_AD[-kl,]
  Sum_AD_Data_Subset = Rem_AD[which(Rem_AD$Type == 0),]
  Sum_AD = sum(Sum_AD_Data_Subset$N..Patients)
  model = glm(Type~.,family = "binomial",data = randomized_data)
  prediction = predict.glm(model,newdata = randomized_data,type = "response")
  Pi = prediction
  if(kl>30){
    data = data_read
  } else{
    data = data_read[which(data_read$Study_ID != kl),]
    for(gh in 1:length(data[,1])){
      if(data$Study_ID[gh] > kl)
        data$Study_ID[gh] = data$Study_ID[gh]-1
    }
  }
  rownames(randomized_data) = NULL
  rownames(data) = NULL
  
  
  #################################################################################
  #Add additional information to your data
  #################################################################################
  Total_Study = 64
  N_Study = length(unique(data$Study_ID))
  Sample_size = as.numeric(table(data$Study_ID))
  N = sum(Sample_size)
  Full_AD_Data = c()
  for(i in 1:N_Study){
    IPD_Data = data[which(data$Study_ID==i),]
    Full_AD_Data = rbind(Full_AD_Data,colMeans(IPD_Data))
  }
  Full_AD_Data = data.frame(Full_AD_Data)
  colnames(Full_AD_Data) <- colnames(data)
  Pi_n = Pi[1:N_Study]
  Full_AD_Data <- data.frame(Sample_size,Full_AD_Data,Pi_n)
  Regression_Weights = c()
  Size <- Full_AD_Data$Sample_size
  for(i in 1:length(Full_AD_Data[,1])){
    Regression_Weights <- append(Regression_Weights,rep(Pi_n[i],Size[i]))
  }
  Final_IPD = data.frame(data,Regression_Weights)
  colnames(Final_IPD)[46] = "Study_Indicator"
  #################################################################################
  
  
  
  
  
  
  #################################################################################
  #For Estimating Simple TMLE for EMB_tx
  #################################################################################
  Interested_IPD = Final_IPD[which(Final_IPD$D_EMB_tx==1),]
  Covariates = Interested_IPD[,c(2:14,16:29)]
  Medication_Interest = Interested_IPD$EMB_tx2
  Outcome = Interested_IPD$Y
  GLM_Estimate = Func1(Covariates,Medication_Interest,Outcome)
  G = Func2(Covariates,Medication_Interest)
  TMLE_Estimate_EMB = Func4(Outcome,GLM_Estimate,G,Medication_Interest)
  GLM_Estimate_Incorrect_EMB = Func3(Medication_Interest,Outcome)
  TMLE_Estimate_Incorrect = Func4(Outcome,GLM_Estimate_Incorrect_EMB,G,Medication_Interest)
  
  #################################################################################
  #For Estimating Transportability TMLE for EMB_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:14,16:29)]
  Medication_Interest = Interested_IPD$EMB_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_EMB_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1(Covariates,Medication_Interest,Outcome)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_EMB_tx
  Size <- Interested_AD$Sample_size
  #P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  TMLE_Estimate_Transport_EMB = Func4(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest)
  #################################################################################
  
  
  #################################################################################
  #For Estimating Two Stage TMLE for EMB_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:14,16:29)]
  Medication_Interest = Interested_IPD$EMB_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_EMB_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1_IPW(Covariates,Medication_Interest,Outcome,Ind_Interest)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_EMB_tx
  Size <- Interested_AD$Sample_size
  #P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6_IPW(Covariates,Medication_Interest,Indicator_1,Ind_Interest)
  G2_Incorrect <- Func7(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  G_Incorrect <- G1*G2_Incorrect
  
  TMLE_Stage1_Estimate_CorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandCorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_CorrectQandIncorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Incorrect,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Incorrect,Medication_Interest,Ind_Interest)
  
  Size_TMLE <- data.frame(cbind(TMLE_Stage1_Estimate_CorrectQandG,TMLE_Stage1_Estimate_IncorrectQandCorrectG,
                                TMLE_Stage1_Estimate_CorrectQandIncorrectG,TMLE_Stage1_Estimate_IncorrectQandG,
                                Stage1_GLM_Estimate,Stage1_GLM_Estimate_Incorrect,Interested_IPD$Study_ID))
  colnames(Size_TMLE) <- c("TMLE_Est_BothCorrect","TMLE_Est_CorrectG","TMLE_Est_CorrectQ","TMLE_Est_BothIncorrectQ",
                           "GLM_Est","GLM_Est_Incorrect","Study_ID")
  TMLE_Both_Correct <- NA
  TMLE_Correct_G <- NA
  TMLE_Correct_Q <- NA
  TMLE_Both_Incorrect <- NA
  Avg_GLM <- NA
  Avg_GLM_Incorrect <- NA
  for(i in 1:length(Interested_AD[,1])){
    New_Data <- Size_TMLE[which(Size_TMLE$Study_ID == Interested_AD$Study_ID[i]),]
    TMLE_Both_Correct[i] <- mean(New_Data$TMLE_Est_BothCorrect)
    TMLE_Correct_G[i] <- mean(New_Data$TMLE_Est_CorrectG)
    TMLE_Correct_Q[i] <- mean(New_Data$TMLE_Est_CorrectQ)
    TMLE_Both_Incorrect[i] <- mean(New_Data$TMLE_Est_BothIncorrectQ)
    Avg_GLM[i] <- mean(New_Data$GLM_Est)
    Avg_GLM_Incorrect[i] <- mean(New_Data$GLM_Est_Incorrect)
  }
  
  
  
  
  
  TMLE_Stage2_BothCorrect_EMB <- (TMLE_Both_Correct*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_G_EMB <- (TMLE_Correct_G*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_Q_EMB <- (TMLE_Correct_Q*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_BothInorrect_EMB <- (TMLE_Both_Incorrect*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  
  TMLE_Stage2_BothCorrect_EMB_New <- (TMLE_Both_Correct/Pi_n)/Total_Study
  TMLE_Stage2_Correct_G_EMB_New <- (TMLE_Correct_G/Pi_n)/Total_Study
  TMLE_Stage2_Correct_Q_EMB_New <- (TMLE_Correct_Q/Pi_n)/Total_Study
  TMLE_Stage2_BothInorrect_EMB_New <- (TMLE_Both_Incorrect/Pi_n)/Total_Study
  #################################################################################
  
  
  
  
  
  
  #################################################################################
  #For Estimating Simple TMLE for AMK_tx
  #################################################################################
  Interested_IPD = Final_IPD[which(Final_IPD$D_AMK_tx==1),]
  Covariates = Interested_IPD[,c(2:15,17:29)]
  Medication_Interest = Interested_IPD$AMK_tx2
  Outcome = Interested_IPD$Y
  GLM_Estimate = Func1(Covariates,Medication_Interest,Outcome)
  G = Func2(Covariates[,-25],Medication_Interest)
  TMLE_Estimate_AMK = Func4(Outcome,GLM_Estimate,G,Medication_Interest)
  GLM_Estimate_Incorrect_AMK = Func3(Medication_Interest,Outcome)
  TMLE_Estimate_Incorrect = Func4(Outcome,GLM_Estimate_Incorrect_AMK,G,Medication_Interest)
  
  #################################################################################
  #For Estimating Transportability TMLE for AMK_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:15,17:29)]
  Medication_Interest = Interested_IPD$AMK_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_AMK_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1(Covariates,Medication_Interest,Outcome)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_AMK_tx
  Size <- Interested_AD$Sample_size
  P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  #P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6(Covariates[,-25],Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  TMLE_Estimate_Transport_AMK = Func4(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest)
  #################################################################################
  
  #################################################################################
  #For Estimating Two Stage TMLE for AMK_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:15,17:29)]
  Medication_Interest = Interested_IPD$AMK_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_AMK_tx
  Stage1_GLM_Estimate <- Func1_IPW(Covariates,Medication_Interest,Outcome,Ind_Interest)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_AMK_tx
  Size <- Interested_AD$Sample_size
  P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  #P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6_IPW(Covariates[,-25],Medication_Interest,Indicator_1,Ind_Interest)
  G2_Incorrect <- Func7(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  G_Incorrect <- G1*G2_Incorrect
  
  TMLE_Stage1_Estimate_CorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandCorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_CorrectQandIncorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Incorrect,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Incorrect,Medication_Interest,Ind_Interest)
  
  Size_TMLE <- data.frame(cbind(TMLE_Stage1_Estimate_CorrectQandG,TMLE_Stage1_Estimate_IncorrectQandCorrectG,
                                TMLE_Stage1_Estimate_CorrectQandIncorrectG,TMLE_Stage1_Estimate_IncorrectQandG,
                                Stage1_GLM_Estimate,Stage1_GLM_Estimate_Incorrect,Interested_IPD$Study_ID))
  colnames(Size_TMLE) <- c("TMLE_Est_BothCorrect","TMLE_Est_CorrectG","TMLE_Est_CorrectQ","TMLE_Est_BothIncorrectQ",
                           "GLM_Est","GLM_Est_Incorrect","Study_ID")
  TMLE_Both_Correct <- NA
  TMLE_Correct_G <- NA
  TMLE_Correct_Q <- NA
  TMLE_Both_Incorrect <- NA
  Avg_GLM <- NA
  Avg_GLM_Incorrect <- NA
  for(i in 1:length(Interested_AD[,1])){
    New_Data <- Size_TMLE[which(Size_TMLE$Study_ID == Interested_AD$Study_ID[i]),]
    TMLE_Both_Correct[i] <- mean(New_Data$TMLE_Est_BothCorrect)
    TMLE_Correct_G[i] <- mean(New_Data$TMLE_Est_CorrectG)
    TMLE_Correct_Q[i] <- mean(New_Data$TMLE_Est_CorrectQ)
    TMLE_Both_Incorrect[i] <- mean(New_Data$TMLE_Est_BothIncorrectQ)
    Avg_GLM[i] <- mean(New_Data$GLM_Est)
    Avg_GLM_Incorrect[i] <- mean(New_Data$GLM_Est_Incorrect)
  }
  
  
  
  
  TMLE_Stage2_BothCorrect_AMK <- (TMLE_Both_Correct*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_G_AMK <- (TMLE_Correct_G*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_Q_AMK <- (TMLE_Correct_Q*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_BothInorrect_AMK <- (TMLE_Both_Incorrect*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  
  TMLE_Stage2_BothCorrect_AMK_New <- (TMLE_Both_Correct/Pi_n)/Total_Study
  TMLE_Stage2_Correct_G_AMK_New <- (TMLE_Correct_G/Pi_n)/Total_Study
  TMLE_Stage2_Correct_Q_AMK_New <- (TMLE_Correct_Q/Pi_n)/Total_Study
  TMLE_Stage2_BothInorrect_AMK_New <- (TMLE_Both_Incorrect/Pi_n)/Total_Study
  
  
  
  
  
  
  #################################################################################
  #For Estimating Simple TMLE for CAP_tx
  #################################################################################
  Interested_IPD = Final_IPD[which(Final_IPD$D_CAP_tx==1),]
  Covariates = Interested_IPD[,c(2:16,18:29)]
  Medication_Interest = Interested_IPD$CAP_tx2
  Outcome = Interested_IPD$Y
  GLM_Estimate = Func1(Covariates,Medication_Interest,Outcome)
  G = Func2(Covariates,Medication_Interest)
  TMLE_Estimate_CAP = Func4(Outcome,GLM_Estimate,G,Medication_Interest)
  GLM_Estimate_Incorrect_CAP = Func3(Medication_Interest,Outcome)
  TMLE_Estimate_Incorrect = Func4(Outcome,GLM_Estimate_Incorrect_CAP,G,Medication_Interest)
  #################################################################################
  #For Estimating Transportability TMLE for CAP_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:16,18:29)]
  Medication_Interest = Interested_IPD$CAP_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_CAP_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1(Covariates,Medication_Interest,Outcome)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_CAP_tx
  Size <- Interested_AD$Sample_size
  P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  #P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  TMLE_Estimate_Transport_CAP = Func4(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest)
  #################################################################################
  
  #################################################################################
  #For Estimating Two Stage TMLE for CAP_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:16,18:29)]
  Medication_Interest = Interested_IPD$CAP_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_CAP_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1_IPW(Covariates,Medication_Interest,Outcome,Ind_Interest)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_CAP_tx
  Size <- Interested_AD$Sample_size
  P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  #P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6_IPW(Covariates,Medication_Interest,Indicator_1,Ind_Interest)
  G2_Incorrect <- Func7(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  G_Incorrect <- G1*G2_Incorrect
  
  TMLE_Stage1_Estimate_CorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandCorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_CorrectQandIncorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Incorrect,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Incorrect,Medication_Interest,Ind_Interest)
  
  Size_TMLE <- data.frame(cbind(TMLE_Stage1_Estimate_CorrectQandG,TMLE_Stage1_Estimate_IncorrectQandCorrectG,
                                TMLE_Stage1_Estimate_CorrectQandIncorrectG,TMLE_Stage1_Estimate_IncorrectQandG,
                                Stage1_GLM_Estimate,Stage1_GLM_Estimate_Incorrect,Interested_IPD$Study_ID))
  colnames(Size_TMLE) <- c("TMLE_Est_BothCorrect","TMLE_Est_CorrectG","TMLE_Est_CorrectQ","TMLE_Est_BothIncorrectQ",
                           "GLM_Est","GLM_Est_Incorrect","Study_ID")
  TMLE_Both_Correct <- NA
  TMLE_Correct_G <- NA
  TMLE_Correct_Q <- NA
  TMLE_Both_Incorrect <- NA
  Avg_GLM <- NA
  Avg_GLM_Incorrect <- NA
  for(i in 1:length(Interested_AD[,1])){
    New_Data <- Size_TMLE[which(Size_TMLE$Study_ID == Interested_AD$Study_ID[i]),]
    TMLE_Both_Correct[i] <- mean(New_Data$TMLE_Est_BothCorrect)
    TMLE_Correct_G[i] <- mean(New_Data$TMLE_Est_CorrectG)
    TMLE_Correct_Q[i] <- mean(New_Data$TMLE_Est_CorrectQ)
    TMLE_Both_Incorrect[i] <- mean(New_Data$TMLE_Est_BothIncorrectQ)
    Avg_GLM[i] <- mean(New_Data$GLM_Est)
    Avg_GLM_Incorrect[i] <- mean(New_Data$GLM_Est_Incorrect)
  }
  
  
  
  TMLE_Stage2_BothCorrect_CAP <- (TMLE_Both_Correct*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_G_CAP <- (TMLE_Correct_G*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_Q_CAP <- (TMLE_Correct_Q*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_BothInorrect_CAP <- (TMLE_Both_Incorrect*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  
  TMLE_Stage2_BothCorrect_CAP_New <- (TMLE_Both_Correct/Pi_n)/Total_Study
  TMLE_Stage2_Correct_G_CAP_New <- (TMLE_Correct_G/Pi_n)/Total_Study
  TMLE_Stage2_Correct_Q_CAP_New <- (TMLE_Correct_Q/Pi_n)/Total_Study
  TMLE_Stage2_BothInorrect_CAP_New <- (TMLE_Both_Incorrect/Pi_n)/Total_Study
  
  
  
  
  
  #################################################################################
  #For Estimating Simple TMLE for CIP_tx
  #################################################################################
  Interested_IPD = Final_IPD[which(Final_IPD$D_CIP_tx==1),]
  Covariates = Interested_IPD[,c(2:17,19:29)]
  Medication_Interest = Interested_IPD$CIP_tx2
  Outcome = Interested_IPD$Y
  GLM_Estimate = Func1(Covariates,Medication_Interest,Outcome)
  G = Func2(Covariates,Medication_Interest)
  TMLE_Estimate_CIP = Func4(Outcome,GLM_Estimate,G,Medication_Interest)
  GLM_Estimate_Incorrect_CIP = Func3(Medication_Interest,Outcome)
  TMLE_Estimate_Incorrect = Func4(Outcome,GLM_Estimate_Incorrect_CIP,G,Medication_Interest)
  #################################################################################
  #For Estimating Transportability TMLE for CIP_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:17,19:29)]
  Medication_Interest = Interested_IPD$CIP_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_CIP_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1(Covariates,Medication_Interest,Outcome)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_CIP_tx
  Size <- Interested_AD$Sample_size
  P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  #P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  TMLE_Estimate_Transport_CIP = Func4(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest)
  #################################################################################
  
  #################################################################################
  #For Estimating Two Stage TMLE for CIP_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:17,19:29)]
  Medication_Interest = Interested_IPD$CIP_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_CIP_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1_IPW(Covariates,Medication_Interest,Outcome,Ind_Interest)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_CIP_tx
  Size <- Interested_AD$Sample_size
  P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  #P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6_IPW(Covariates,Medication_Interest,Indicator_1,Ind_Interest)
  G2_Incorrect <- Func7(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  G_Incorrect <- G1*G2_Incorrect
  
  TMLE_Stage1_Estimate_CorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandCorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_CorrectQandIncorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Incorrect,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Incorrect,Medication_Interest,Ind_Interest)
  
  Size_TMLE <- data.frame(cbind(TMLE_Stage1_Estimate_CorrectQandG,TMLE_Stage1_Estimate_IncorrectQandCorrectG,
                                TMLE_Stage1_Estimate_CorrectQandIncorrectG,TMLE_Stage1_Estimate_IncorrectQandG,
                                Stage1_GLM_Estimate,Stage1_GLM_Estimate_Incorrect,Interested_IPD$Study_ID))
  colnames(Size_TMLE) <- c("TMLE_Est_BothCorrect","TMLE_Est_CorrectG","TMLE_Est_CorrectQ","TMLE_Est_BothIncorrectQ",
                           "GLM_Est","GLM_Est_Incorrect","Study_ID")
  TMLE_Both_Correct <- NA
  TMLE_Correct_G <- NA
  TMLE_Correct_Q <- NA
  TMLE_Both_Incorrect <- NA
  Avg_GLM <- NA
  Avg_GLM_Incorrect <- NA
  for(i in 1:length(Interested_AD[,1])){
    New_Data <- Size_TMLE[which(Size_TMLE$Study_ID == Interested_AD$Study_ID[i]),]
    TMLE_Both_Correct[i] <- mean(New_Data$TMLE_Est_BothCorrect)
    TMLE_Correct_G[i] <- mean(New_Data$TMLE_Est_CorrectG)
    TMLE_Correct_Q[i] <- mean(New_Data$TMLE_Est_CorrectQ)
    TMLE_Both_Incorrect[i] <- mean(New_Data$TMLE_Est_BothIncorrectQ)
    Avg_GLM[i] <- mean(New_Data$GLM_Est)
    Avg_GLM_Incorrect[i] <- mean(New_Data$GLM_Est_Incorrect)
  }
  
  
  
  TMLE_Stage2_BothCorrect_CIP <- (TMLE_Both_Correct*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_G_CIP <- (TMLE_Correct_G*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_Q_CIP <- (TMLE_Correct_Q*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_BothInorrect_CIP <- (TMLE_Both_Incorrect*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  
  TMLE_Stage2_BothCorrect_CIP_New <- (TMLE_Both_Correct/Pi_n)/Total_Study
  TMLE_Stage2_Correct_G_CIP_New <- (TMLE_Correct_G/Pi_n)/Total_Study
  TMLE_Stage2_Correct_Q_CIP_New <- (TMLE_Correct_Q/Pi_n)/Total_Study
  TMLE_Stage2_BothInorrect_CIP_New <- (TMLE_Both_Incorrect/Pi_n)/Total_Study
  
  
  
  
  
  
  
  #################################################################################
  #For Estimating Simple TMLE for CS_tx
  #################################################################################
  Interested_IPD = Final_IPD[which(Final_IPD$D_CS_tx==1),]
  Covariates = Interested_IPD[,c(2:18,20:29)]
  Medication_Interest = Interested_IPD$CS_tx2
  Outcome = Interested_IPD$Y
  GLM_Estimate = Func1(Covariates,Medication_Interest,Outcome)
  G = Func2(Covariates,Medication_Interest)
  TMLE_Estimate_CS = Func4(Outcome,GLM_Estimate,G,Medication_Interest)
  GLM_Estimate_Incorrect_CS = Func3(Medication_Interest,Outcome)
  TMLE_Estimate_Incorrect = Func4(Outcome,GLM_Estimate_Incorrect_CS,G,Medication_Interest)
  #################################################################################
  #For Estimating Transportability TMLE for CS_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:18,20:29)]
  Medication_Interest = Interested_IPD$CS_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_CS_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1(Covariates,Medication_Interest,Outcome)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_CS_tx
  Size <- Interested_AD$Sample_size
  P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  #P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  TMLE_Estimate_Transport_CS = Func4(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest)
  #################################################################################
  
  #################################################################################
  #For Estimating Two Stage TMLE for CS_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:18,20:29)]
  Medication_Interest = Interested_IPD$CS_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_CS_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1_IPW(Covariates,Medication_Interest,Outcome,Ind_Interest)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_CS_tx
  Size <- Interested_AD$Sample_size
  P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  #P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6_IPW(Covariates,Medication_Interest,Indicator_1,Ind_Interest)
  G2_Incorrect <- Func7(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  G_Incorrect <- G1*G2_Incorrect
  
  TMLE_Stage1_Estimate_CorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandCorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_CorrectQandIncorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Incorrect,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Incorrect,Medication_Interest,Ind_Interest)
  
  Size_TMLE <- data.frame(cbind(TMLE_Stage1_Estimate_CorrectQandG,TMLE_Stage1_Estimate_IncorrectQandCorrectG,
                                TMLE_Stage1_Estimate_CorrectQandIncorrectG,TMLE_Stage1_Estimate_IncorrectQandG,
                                Stage1_GLM_Estimate,Stage1_GLM_Estimate_Incorrect,Interested_IPD$Study_ID))
  colnames(Size_TMLE) <- c("TMLE_Est_BothCorrect","TMLE_Est_CorrectG","TMLE_Est_CorrectQ","TMLE_Est_BothIncorrectQ",
                           "GLM_Est","GLM_Est_Incorrect","Study_ID")
  TMLE_Both_Correct <- NA
  TMLE_Correct_G <- NA
  TMLE_Correct_Q <- NA
  TMLE_Both_Incorrect <- NA
  Avg_GLM <- NA
  Avg_GLM_Incorrect <- NA
  for(i in 1:length(Interested_AD[,1])){
    New_Data <- Size_TMLE[which(Size_TMLE$Study_ID == Interested_AD$Study_ID[i]),]
    TMLE_Both_Correct[i] <- mean(New_Data$TMLE_Est_BothCorrect)
    TMLE_Correct_G[i] <- mean(New_Data$TMLE_Est_CorrectG)
    TMLE_Correct_Q[i] <- mean(New_Data$TMLE_Est_CorrectQ)
    TMLE_Both_Incorrect[i] <- mean(New_Data$TMLE_Est_BothIncorrectQ)
    Avg_GLM[i] <- mean(New_Data$GLM_Est)
    Avg_GLM_Incorrect[i] <- mean(New_Data$GLM_Est_Incorrect)
  }
  
  
  
  TMLE_Stage2_BothCorrect_CS <- (TMLE_Both_Correct*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_G_CS <- (TMLE_Correct_G*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_Q_CS <- (TMLE_Correct_Q*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_BothInorrect_CS <- (TMLE_Both_Incorrect*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  
  TMLE_Stage2_BothCorrect_CS_New <- (TMLE_Both_Correct/Pi_n)/Total_Study
  TMLE_Stage2_Correct_G_CS_New <- (TMLE_Correct_G/Pi_n)/Total_Study
  TMLE_Stage2_Correct_Q_CS_New <- (TMLE_Correct_Q/Pi_n)/Total_Study
  TMLE_Stage2_BothInorrect_CS_New <- (TMLE_Both_Incorrect/Pi_n)/Total_Study
  
  
  
  #################################################################################
  #For Estimating Simple TMLE for ETO_tx
  #################################################################################
  Interested_IPD = Final_IPD[which(Final_IPD$D_ETO_tx==1),]
  Covariates = Interested_IPD[,c(2:19,21:29)]
  Medication_Interest = Interested_IPD$ETO_tx2
  Outcome = Interested_IPD$Y
  GLM_Estimate = Func1(Covariates,Medication_Interest,Outcome)
  G = Func2(Covariates,Medication_Interest)
  TMLE_Estimate_ETO = Func4(Outcome,GLM_Estimate,G,Medication_Interest)
  GLM_Estimate_Incorrect_ETO = Func3(Medication_Interest,Outcome)
  TMLE_Estimate_Incorrect = Func4(Outcome,GLM_Estimate_Incorrect_ETO,G,Medication_Interest)
  #################################################################################
  #For Estimating Transportability TMLE for ETO_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:19,21:29)]
  Medication_Interest = Interested_IPD$ETO_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_ETO_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1(Covariates,Medication_Interest,Outcome)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_ETO_tx
  Size <- Interested_AD$Sample_size
  P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  #P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  TMLE_Estimate_Transport_ETO = Func4(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest)
  #################################################################################
  
  #################################################################################
  #For Estimating Two Stage TMLE for ETO_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:19,21:29)]
  Medication_Interest = Interested_IPD$ETO_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_ETO_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1_IPW(Covariates,Medication_Interest,Outcome,Ind_Interest)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_ETO_tx
  Size <- Interested_AD$Sample_size
  P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  #P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6_IPW(Covariates,Medication_Interest,Indicator_1,Ind_Interest)
  G2_Incorrect <- Func7(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  G_Incorrect <- G1*G2_Incorrect
  
  TMLE_Stage1_Estimate_CorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandCorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_CorrectQandIncorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Incorrect,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Incorrect,Medication_Interest,Ind_Interest)
  
  Size_TMLE <- data.frame(cbind(TMLE_Stage1_Estimate_CorrectQandG,TMLE_Stage1_Estimate_IncorrectQandCorrectG,
                                TMLE_Stage1_Estimate_CorrectQandIncorrectG,TMLE_Stage1_Estimate_IncorrectQandG,
                                Stage1_GLM_Estimate,Stage1_GLM_Estimate_Incorrect,Interested_IPD$Study_ID))
  colnames(Size_TMLE) <- c("TMLE_Est_BothCorrect","TMLE_Est_CorrectG","TMLE_Est_CorrectQ","TMLE_Est_BothIncorrectQ",
                           "GLM_Est","GLM_Est_Incorrect","Study_ID")
  TMLE_Both_Correct <- NA
  TMLE_Correct_G <- NA
  TMLE_Correct_Q <- NA
  TMLE_Both_Incorrect <- NA
  Avg_GLM <- NA
  Avg_GLM_Incorrect <- NA
  for(i in 1:length(Interested_AD[,1])){
    New_Data <- Size_TMLE[which(Size_TMLE$Study_ID == Interested_AD$Study_ID[i]),]
    TMLE_Both_Correct[i] <- mean(New_Data$TMLE_Est_BothCorrect)
    TMLE_Correct_G[i] <- mean(New_Data$TMLE_Est_CorrectG)
    TMLE_Correct_Q[i] <- mean(New_Data$TMLE_Est_CorrectQ)
    TMLE_Both_Incorrect[i] <- mean(New_Data$TMLE_Est_BothIncorrectQ)
    Avg_GLM[i] <- mean(New_Data$GLM_Est)
    Avg_GLM_Incorrect[i] <- mean(New_Data$GLM_Est_Incorrect)
  }
  
  
  
  TMLE_Stage2_BothCorrect_ETO <- (TMLE_Both_Correct*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_G_ETO <- (TMLE_Correct_G*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_Q_ETO <- (TMLE_Correct_Q*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_BothInorrect_ETO <- (TMLE_Both_Incorrect*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  
  TMLE_Stage2_BothCorrect_ETO_New <- (TMLE_Both_Correct/Pi_n)/Total_Study
  TMLE_Stage2_Correct_G_ETO_New <- (TMLE_Correct_G/Pi_n)/Total_Study
  TMLE_Stage2_Correct_Q_ETO_New <- (TMLE_Correct_Q/Pi_n)/Total_Study
  TMLE_Stage2_BothInorrect_ETO_New <- (TMLE_Both_Incorrect/Pi_n)/Total_Study
  
  
  
  
  
  
  
  #################################################################################
  #For Estimating Simple TMLE for OFX_tx
  #################################################################################
  Interested_IPD = Final_IPD[which(Final_IPD$D_OFX_tx==1),]
  Covariates = Interested_IPD[,c(2:20,22:29)]
  Medication_Interest = Interested_IPD$OFX_tx2
  Outcome = Interested_IPD$Y
  GLM_Estimate = Func1(Covariates,Medication_Interest,Outcome)
  G = Func2(Covariates,Medication_Interest)
  TMLE_Estimate_OFX = Func4(Outcome,GLM_Estimate,G,Medication_Interest)
  GLM_Estimate_Incorrect_OFX = Func3(Medication_Interest,Outcome)
  TMLE_Estimate_Incorrect = Func4(Outcome,GLM_Estimate_Incorrect_OFX,G,Medication_Interest)
  #################################################################################
  #For Estimating Transportability TMLE for OFX_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:20,22:29)]
  Medication_Interest = Interested_IPD$OFX_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_OFX_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1(Covariates,Medication_Interest,Outcome)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_OFX_tx
  Size <- Interested_AD$Sample_size
  P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  #P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  TMLE_Estimate_Transport_OFX = Func4(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest)
  #################################################################################
  
  #################################################################################
  #For Estimating Two Stage TMLE for OFX_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:20,22:29)]
  Medication_Interest = Interested_IPD$OFX_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_OFX_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1_IPW(Covariates,Medication_Interest,Outcome,Ind_Interest)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_OFX_tx
  Size <- Interested_AD$Sample_size
  P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  #P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6_IPW(Covariates,Medication_Interest,Indicator_1,Ind_Interest)
  G2_Incorrect <- Func7(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  G_Incorrect <- G1*G2_Incorrect
  
  TMLE_Stage1_Estimate_CorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandCorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_CorrectQandIncorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Incorrect,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Incorrect,Medication_Interest,Ind_Interest)
  
  Size_TMLE <- data.frame(cbind(TMLE_Stage1_Estimate_CorrectQandG,TMLE_Stage1_Estimate_IncorrectQandCorrectG,
                                TMLE_Stage1_Estimate_CorrectQandIncorrectG,TMLE_Stage1_Estimate_IncorrectQandG,
                                Stage1_GLM_Estimate,Stage1_GLM_Estimate_Incorrect,Interested_IPD$Study_ID))
  colnames(Size_TMLE) <- c("TMLE_Est_BothCorrect","TMLE_Est_CorrectG","TMLE_Est_CorrectQ","TMLE_Est_BothIncorrectQ",
                           "GLM_Est","GLM_Est_Incorrect","Study_ID")
  TMLE_Both_Correct <- NA
  TMLE_Correct_G <- NA
  TMLE_Correct_Q <- NA
  TMLE_Both_Incorrect <- NA
  Avg_GLM <- NA
  Avg_GLM_Incorrect <- NA
  for(i in 1:length(Interested_AD[,1])){
    New_Data <- Size_TMLE[which(Size_TMLE$Study_ID == Interested_AD$Study_ID[i]),]
    TMLE_Both_Correct[i] <- mean(New_Data$TMLE_Est_BothCorrect)
    TMLE_Correct_G[i] <- mean(New_Data$TMLE_Est_CorrectG)
    TMLE_Correct_Q[i] <- mean(New_Data$TMLE_Est_CorrectQ)
    TMLE_Both_Incorrect[i] <- mean(New_Data$TMLE_Est_BothIncorrectQ)
    Avg_GLM[i] <- mean(New_Data$GLM_Est)
    Avg_GLM_Incorrect[i] <- mean(New_Data$GLM_Est_Incorrect)
  }
  
  
  
  TMLE_Stage2_BothCorrect_OFX <- (TMLE_Both_Correct*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_G_OFX <- (TMLE_Correct_G*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_Q_OFX <- (TMLE_Correct_Q*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_BothInorrect_OFX <- (TMLE_Both_Incorrect*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  
  TMLE_Stage2_BothCorrect_OFX_New <- (TMLE_Both_Correct/Pi_n)/Total_Study
  TMLE_Stage2_Correct_G_OFX_New <- (TMLE_Correct_G/Pi_n)/Total_Study
  TMLE_Stage2_Correct_Q_OFX_New <- (TMLE_Correct_Q/Pi_n)/Total_Study
  TMLE_Stage2_BothInorrect_OFX_New <- (TMLE_Both_Incorrect/Pi_n)/Total_Study
  
  
  
  
  
  
  
  #################################################################################
  #For Estimating Simple TMLE for PAS_tx
  #################################################################################
  Interested_IPD = Final_IPD[which(Final_IPD$D_PAS_tx==1),]
  Covariates = Interested_IPD[,c(2:21,23:29)]
  Medication_Interest = Interested_IPD$PAS_tx2
  Outcome = Interested_IPD$Y
  GLM_Estimate = Func1(Covariates,Medication_Interest,Outcome)
  G = Func2(Covariates,Medication_Interest)
  TMLE_Estimate_PAS = Func4(Outcome,GLM_Estimate,G,Medication_Interest)
  GLM_Estimate_Incorrect_PAS = Func3(Medication_Interest,Outcome)
  TMLE_Estimate_Incorrect = Func4(Outcome,GLM_Estimate_Incorrect_PAS,G,Medication_Interest)
  #################################################################################
  #For Estimating Transportability TMLE for PAS_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:21,23:29)]
  Medication_Interest = Interested_IPD$PAS_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_PAS_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1(Covariates,Medication_Interest,Outcome)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_PAS_tx
  Size <- Interested_AD$Sample_size
  #P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  TMLE_Estimate_Transport_PAS = Func4(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest)
  #################################################################################
  
  #################################################################################
  #For Estimating Two Stage TMLE for PAS_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:21,23:29)]
  Medication_Interest = Interested_IPD$PAS_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_PAS_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1_IPW(Covariates,Medication_Interest,Outcome,Ind_Interest)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_PAS_tx
  Size <- Interested_AD$Sample_size
  #P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6_IPW(Covariates,Medication_Interest,Indicator_1,Ind_Interest)
  G2_Incorrect <- Func7(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  G_Incorrect <- G1*G2_Incorrect
  
  TMLE_Stage1_Estimate_CorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandCorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_CorrectQandIncorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Incorrect,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Incorrect,Medication_Interest,Ind_Interest)
  
  Size_TMLE <- data.frame(cbind(TMLE_Stage1_Estimate_CorrectQandG,TMLE_Stage1_Estimate_IncorrectQandCorrectG,
                                TMLE_Stage1_Estimate_CorrectQandIncorrectG,TMLE_Stage1_Estimate_IncorrectQandG,
                                Stage1_GLM_Estimate,Stage1_GLM_Estimate_Incorrect,Interested_IPD$Study_ID))
  colnames(Size_TMLE) <- c("TMLE_Est_BothCorrect","TMLE_Est_CorrectG","TMLE_Est_CorrectQ","TMLE_Est_BothIncorrectQ",
                           "GLM_Est","GLM_Est_Incorrect","Study_ID")
  TMLE_Both_Correct <- NA
  TMLE_Correct_G <- NA
  TMLE_Correct_Q <- NA
  TMLE_Both_Incorrect <- NA
  Avg_GLM <- NA
  Avg_GLM_Incorrect <- NA
  for(i in 1:length(Interested_AD[,1])){
    New_Data <- Size_TMLE[which(Size_TMLE$Study_ID == Interested_AD$Study_ID[i]),]
    TMLE_Both_Correct[i] <- mean(New_Data$TMLE_Est_BothCorrect)
    TMLE_Correct_G[i] <- mean(New_Data$TMLE_Est_CorrectG)
    TMLE_Correct_Q[i] <- mean(New_Data$TMLE_Est_CorrectQ)
    TMLE_Both_Incorrect[i] <- mean(New_Data$TMLE_Est_BothIncorrectQ)
    Avg_GLM[i] <- mean(New_Data$GLM_Est)
    Avg_GLM_Incorrect[i] <- mean(New_Data$GLM_Est_Incorrect)
  }
  
  
  
  TMLE_Stage2_BothCorrect_PAS <- (TMLE_Both_Correct*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_G_PAS <- (TMLE_Correct_G*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_Q_PAS <- (TMLE_Correct_Q*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_BothInorrect_PAS <- (TMLE_Both_Incorrect*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  
  TMLE_Stage2_BothCorrect_PAS_New <- (TMLE_Both_Correct/Pi_n)/Total_Study
  TMLE_Stage2_Correct_G_PAS_New <- (TMLE_Correct_G/Pi_n)/Total_Study
  TMLE_Stage2_Correct_Q_PAS_New <- (TMLE_Correct_Q/Pi_n)/Total_Study
  TMLE_Stage2_BothInorrect_PAS_New <- (TMLE_Both_Incorrect/Pi_n)/Total_Study
  
  
  
  
  
  
  
  #################################################################################
  #For Estimating Simple TMLE for PTO_tx
  #################################################################################
  Interested_IPD = Final_IPD[which(Final_IPD$D_PTO_tx==1),]
  Covariates = Interested_IPD[,c(2:22,24:29)]
  Medication_Interest = Interested_IPD$PTO_tx2
  Outcome = Interested_IPD$Y
  GLM_Estimate = Func1(Covariates,Medication_Interest,Outcome)
  G = Func2(Covariates,Medication_Interest)
  TMLE_Estimate_PTO = Func4(Outcome,GLM_Estimate,G,Medication_Interest)
  GLM_Estimate_Incorrect_PTO = Func3(Medication_Interest,Outcome)
  TMLE_Estimate_Incorrect = Func4(Outcome,GLM_Estimate_Incorrect_PTO,G,Medication_Interest)
  #################################################################################
  #For Estimating Transportability TMLE for PTO_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:22,24:29)]
  Medication_Interest = Interested_IPD$PTO_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_PTO_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1(Covariates,Medication_Interest,Outcome)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_PTO_tx
  Size <- Interested_AD$Sample_size
  P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  #P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  TMLE_Estimate_Transport_PTO = Func4(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest)
  #################################################################################
  
  #################################################################################
  #For Estimating Two Stage TMLE for PTO_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:22,24:29)]
  Medication_Interest = Interested_IPD$PTO_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_PTO_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1_IPW(Covariates,Medication_Interest,Outcome,Ind_Interest)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_PTO_tx
  Size <- Interested_AD$Sample_size
  P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  #P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6_IPW(Covariates,Medication_Interest,Indicator_1,Ind_Interest)
  G2_Incorrect <- Func7(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  G_Incorrect <- G1*G2_Incorrect
  
  TMLE_Stage1_Estimate_CorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandCorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_CorrectQandIncorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Incorrect,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Incorrect,Medication_Interest,Ind_Interest)
  
  Size_TMLE <- data.frame(cbind(TMLE_Stage1_Estimate_CorrectQandG,TMLE_Stage1_Estimate_IncorrectQandCorrectG,
                                TMLE_Stage1_Estimate_CorrectQandIncorrectG,TMLE_Stage1_Estimate_IncorrectQandG,
                                Stage1_GLM_Estimate,Stage1_GLM_Estimate_Incorrect,Interested_IPD$Study_ID))
  colnames(Size_TMLE) <- c("TMLE_Est_BothCorrect","TMLE_Est_CorrectG","TMLE_Est_CorrectQ","TMLE_Est_BothIncorrectQ",
                           "GLM_Est","GLM_Est_Incorrect","Study_ID")
  TMLE_Both_Correct <- NA
  TMLE_Correct_G <- NA
  TMLE_Correct_Q <- NA
  TMLE_Both_Incorrect <- NA
  Avg_GLM <- NA
  Avg_GLM_Incorrect <- NA
  for(i in 1:length(Interested_AD[,1])){
    New_Data <- Size_TMLE[which(Size_TMLE$Study_ID == Interested_AD$Study_ID[i]),]
    TMLE_Both_Correct[i] <- mean(New_Data$TMLE_Est_BothCorrect)
    TMLE_Correct_G[i] <- mean(New_Data$TMLE_Est_CorrectG)
    TMLE_Correct_Q[i] <- mean(New_Data$TMLE_Est_CorrectQ)
    TMLE_Both_Incorrect[i] <- mean(New_Data$TMLE_Est_BothIncorrectQ)
    Avg_GLM[i] <- mean(New_Data$GLM_Est)
    Avg_GLM_Incorrect[i] <- mean(New_Data$GLM_Est_Incorrect)
  }
  
  
  
  TMLE_Stage2_BothCorrect_PTO <- (TMLE_Both_Correct*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_G_PTO <- (TMLE_Correct_G*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_Q_PTO <- (TMLE_Correct_Q*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_BothInorrect_PTO <- (TMLE_Both_Incorrect*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  
  TMLE_Stage2_BothCorrect_PTO_New <- (TMLE_Both_Correct/Pi_n)/Total_Study
  TMLE_Stage2_Correct_G_PTO_New <- (TMLE_Correct_G/Pi_n)/Total_Study
  TMLE_Stage2_Correct_Q_PTO_New <- (TMLE_Correct_Q/Pi_n)/Total_Study
  TMLE_Stage2_BothInorrect_PTO_New <- (TMLE_Both_Incorrect/Pi_n)/Total_Study
  
  
  
  
  
  
  #################################################################################
  #For Estimating Simple TMLE for RIF_tx
  #################################################################################
  Interested_IPD = Final_IPD[which(Final_IPD$D_RIF_tx==1),]
  Covariates = Interested_IPD[,c(2:23,25:29)]
  Medication_Interest = Interested_IPD$RIF_tx2
  Outcome = Interested_IPD$Y
  GLM_Estimate = Func1(Covariates,Medication_Interest,Outcome)
  G = Func2(Covariates,Medication_Interest)
  TMLE_Estimate_RIF = Func4(Outcome,GLM_Estimate,G,Medication_Interest)
  GLM_Estimate_Incorrect_RIF = Func3(Medication_Interest,Outcome)
  TMLE_Estimate_Incorrect = Func4(Outcome,GLM_Estimate_Incorrect_RIF,G,Medication_Interest)
  #################################################################################
  #For Estimating Transportability TMLE for RIF_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:23,25:29)]
  Medication_Interest = Interested_IPD$RIF_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_RIF_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1(Covariates,Medication_Interest,Outcome)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_RIF_tx
  Size <- Interested_AD$Sample_size
  P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  #P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  TMLE_Estimate_Transport_RIF = Func4(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest)
  #################################################################################
  
  #################################################################################
  #For Estimating Two Stage TMLE for RIF_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:23,25:29)]
  Medication_Interest = Interested_IPD$RIF_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_RIF_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1_IPW(Covariates,Medication_Interest,Outcome,Ind_Interest)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_RIF_tx
  Size <- Interested_AD$Sample_size
  P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  #P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6_IPW(Covariates,Medication_Interest,Indicator_1,Ind_Interest)
  G2_Incorrect <- Func7(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  G_Incorrect <- G1*G2_Incorrect
  
  TMLE_Stage1_Estimate_CorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandCorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_CorrectQandIncorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Incorrect,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Incorrect,Medication_Interest,Ind_Interest)
  
  Size_TMLE <- data.frame(cbind(TMLE_Stage1_Estimate_CorrectQandG,TMLE_Stage1_Estimate_IncorrectQandCorrectG,
                                TMLE_Stage1_Estimate_CorrectQandIncorrectG,TMLE_Stage1_Estimate_IncorrectQandG,
                                Stage1_GLM_Estimate,Stage1_GLM_Estimate_Incorrect,Interested_IPD$Study_ID))
  colnames(Size_TMLE) <- c("TMLE_Est_BothCorrect","TMLE_Est_CorrectG","TMLE_Est_CorrectQ","TMLE_Est_BothIncorrectQ",
                           "GLM_Est","GLM_Est_Incorrect","Study_ID")
  TMLE_Both_Correct <- NA
  TMLE_Correct_G <- NA
  TMLE_Correct_Q <- NA
  TMLE_Both_Incorrect <- NA
  Avg_GLM <- NA
  Avg_GLM_Incorrect <- NA
  for(i in 1:length(Interested_AD[,1])){
    New_Data <- Size_TMLE[which(Size_TMLE$Study_ID == Interested_AD$Study_ID[i]),]
    TMLE_Both_Correct[i] <- mean(New_Data$TMLE_Est_BothCorrect)
    TMLE_Correct_G[i] <- mean(New_Data$TMLE_Est_CorrectG)
    TMLE_Correct_Q[i] <- mean(New_Data$TMLE_Est_CorrectQ)
    TMLE_Both_Incorrect[i] <- mean(New_Data$TMLE_Est_BothIncorrectQ)
    Avg_GLM[i] <- mean(New_Data$GLM_Est)
    Avg_GLM_Incorrect[i] <- mean(New_Data$GLM_Est_Incorrect)
  }
  
  
  
  TMLE_Stage2_BothCorrect_RIF <- (TMLE_Both_Correct*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_G_RIF <- (TMLE_Correct_G*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_Q_RIF <- (TMLE_Correct_Q*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_BothInorrect_RIF <- (TMLE_Both_Incorrect*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  
  TMLE_Stage2_BothCorrect_RIF_New <- (TMLE_Both_Correct/Pi_n)/Total_Study
  TMLE_Stage2_Correct_G_RIF_New <- (TMLE_Correct_G/Pi_n)/Total_Study
  TMLE_Stage2_Correct_Q_RIF_New <- (TMLE_Correct_Q/Pi_n)/Total_Study
  TMLE_Stage2_BothInorrect_RIF_New <- (TMLE_Both_Incorrect/Pi_n)/Total_Study
  
  
  
  
  
  
  #################################################################################
  #For Estimating Simple TMLE for SM_tx
  #################################################################################
  Interested_IPD = Final_IPD[which(Final_IPD$D_SM_tx==1),]
  Covariates = Interested_IPD[,c(2:24,26:29)]
  Medication_Interest = Interested_IPD$SM_tx2
  Outcome = Interested_IPD$Y
  GLM_Estimate = Func1(Covariates,Medication_Interest,Outcome)
  G = Func2(Covariates,Medication_Interest)
  TMLE_Estimate_SM = Func4(Outcome,GLM_Estimate,G,Medication_Interest)
  GLM_Estimate_Incorrect_SM = Func3(Medication_Interest,Outcome)
  TMLE_Estimate_Incorrect = Func4(Outcome,GLM_Estimate_Incorrect_SM,G,Medication_Interest)
  #################################################################################
  #For Estimating Transportability TMLE for SM_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:24,26:29)]
  Medication_Interest = Interested_IPD$SM_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_SM_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1(Covariates,Medication_Interest,Outcome)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_SM_tx
  Size <- Interested_AD$Sample_size
  P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  #P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  TMLE_Estimate_Transport_SM = Func4(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest)
  #################################################################################
  
  #################################################################################
  #For Estimating Two Stage TMLE for SM_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:24,26:29)]
  Medication_Interest = Interested_IPD$SM_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_SM_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1_IPW(Covariates,Medication_Interest,Outcome,Ind_Interest)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_SM_tx
  Size <- Interested_AD$Sample_size
  P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  #P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6_IPW(Covariates,Medication_Interest,Indicator_1,Ind_Interest)
  G2_Incorrect <- Func7(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  G_Incorrect <- G1*G2_Incorrect
  
  TMLE_Stage1_Estimate_CorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandCorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_CorrectQandIncorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Incorrect,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Incorrect,Medication_Interest,Ind_Interest)
  
  Size_TMLE <- data.frame(cbind(TMLE_Stage1_Estimate_CorrectQandG,TMLE_Stage1_Estimate_IncorrectQandCorrectG,
                                TMLE_Stage1_Estimate_CorrectQandIncorrectG,TMLE_Stage1_Estimate_IncorrectQandG,
                                Stage1_GLM_Estimate,Stage1_GLM_Estimate_Incorrect,Interested_IPD$Study_ID))
  colnames(Size_TMLE) <- c("TMLE_Est_BothCorrect","TMLE_Est_CorrectG","TMLE_Est_CorrectQ","TMLE_Est_BothIncorrectQ",
                           "GLM_Est","GLM_Est_Incorrect","Study_ID")
  TMLE_Both_Correct <- NA
  TMLE_Correct_G <- NA
  TMLE_Correct_Q <- NA
  TMLE_Both_Incorrect <- NA
  Avg_GLM <- NA
  Avg_GLM_Incorrect <- NA
  for(i in 1:length(Interested_AD[,1])){
    New_Data <- Size_TMLE[which(Size_TMLE$Study_ID == Interested_AD$Study_ID[i]),]
    TMLE_Both_Correct[i] <- mean(New_Data$TMLE_Est_BothCorrect)
    TMLE_Correct_G[i] <- mean(New_Data$TMLE_Est_CorrectG)
    TMLE_Correct_Q[i] <- mean(New_Data$TMLE_Est_CorrectQ)
    TMLE_Both_Incorrect[i] <- mean(New_Data$TMLE_Est_BothIncorrectQ)
    Avg_GLM[i] <- mean(New_Data$GLM_Est)
    Avg_GLM_Incorrect[i] <- mean(New_Data$GLM_Est_Incorrect)
  }
  
  
  
  TMLE_Stage2_BothCorrect_SM <- (TMLE_Both_Correct*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_G_SM <- (TMLE_Correct_G*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_Q_SM <- (TMLE_Correct_Q*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_BothInorrect_SM <- (TMLE_Both_Incorrect*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  
  TMLE_Stage2_BothCorrect_SM_New <- (TMLE_Both_Correct/Pi_n)/Total_Study
  TMLE_Stage2_Correct_G_SM_New <- (TMLE_Correct_G/Pi_n)/Total_Study
  TMLE_Stage2_Correct_Q_SM_New <- (TMLE_Correct_Q/Pi_n)/Total_Study
  TMLE_Stage2_BothInorrect_SM_New <- (TMLE_Both_Incorrect/Pi_n)/Total_Study
  
  
  
  
  
  
  #################################################################################
  #For Estimating Simple TMLE for PZA_tx
  #################################################################################
  Interested_IPD = Final_IPD[which(Final_IPD$D_PZA_tx==1),]
  Covariates = Interested_IPD[,c(2:25,27:29)]
  Medication_Interest = Interested_IPD$PZA_tx2
  Outcome = Interested_IPD$Y
  GLM_Estimate = Func1(Covariates,Medication_Interest,Outcome)
  G = Func2(Covariates,Medication_Interest)
  TMLE_Estimate_PZA = Func4(Outcome,GLM_Estimate,G,Medication_Interest)
  GLM_Estimate_Incorrect_PZA = Func3(Medication_Interest,Outcome)
  TMLE_Estimate_Incorrect = Func4(Outcome,GLM_Estimate_Incorrect_PZA,G,Medication_Interest)
  #################################################################################
  #For Estimating Transportability TMLE for PZA_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:25,27:29)]
  Medication_Interest = Interested_IPD$PZA_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_PZA_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1(Covariates,Medication_Interest,Outcome)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_PZA_tx
  Size <- Interested_AD$Sample_size
  #P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  TMLE_Estimate_Transport_PZA = Func4(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest)
  #################################################################################
  
  #################################################################################
  #For Estimating Two Stage TMLE for PZA_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:25,27:29)]
  Medication_Interest = Interested_IPD$PZA_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_PZA_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1_IPW(Covariates,Medication_Interest,Outcome,Ind_Interest)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_PZA_tx
  Size <- Interested_AD$Sample_size
  #P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6_IPW(Covariates,Medication_Interest,Indicator_1,Ind_Interest)
  G2_Incorrect <- Func7(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  G_Incorrect <- G1*G2_Incorrect
  
  TMLE_Stage1_Estimate_CorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandCorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_CorrectQandIncorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Incorrect,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Incorrect,Medication_Interest,Ind_Interest)
  
  Size_TMLE <- data.frame(cbind(TMLE_Stage1_Estimate_CorrectQandG,TMLE_Stage1_Estimate_IncorrectQandCorrectG,
                                TMLE_Stage1_Estimate_CorrectQandIncorrectG,TMLE_Stage1_Estimate_IncorrectQandG,
                                Stage1_GLM_Estimate,Stage1_GLM_Estimate_Incorrect,Interested_IPD$Study_ID))
  colnames(Size_TMLE) <- c("TMLE_Est_BothCorrect","TMLE_Est_CorrectG","TMLE_Est_CorrectQ","TMLE_Est_BothIncorrectQ",
                           "GLM_Est","GLM_Est_Incorrect","Study_ID")
  TMLE_Both_Correct <- NA
  TMLE_Correct_G <- NA
  TMLE_Correct_Q <- NA
  TMLE_Both_Incorrect <- NA
  Avg_GLM <- NA
  Avg_GLM_Incorrect <- NA
  for(i in 1:length(Interested_AD[,1])){
    New_Data <- Size_TMLE[which(Size_TMLE$Study_ID == Interested_AD$Study_ID[i]),]
    TMLE_Both_Correct[i] <- mean(New_Data$TMLE_Est_BothCorrect)
    TMLE_Correct_G[i] <- mean(New_Data$TMLE_Est_CorrectG)
    TMLE_Correct_Q[i] <- mean(New_Data$TMLE_Est_CorrectQ)
    TMLE_Both_Incorrect[i] <- mean(New_Data$TMLE_Est_BothIncorrectQ)
    Avg_GLM[i] <- mean(New_Data$GLM_Est)
    Avg_GLM_Incorrect[i] <- mean(New_Data$GLM_Est_Incorrect)
  }
  
  
  
  TMLE_Stage2_BothCorrect_PZA <- (TMLE_Both_Correct*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_G_PZA <- (TMLE_Correct_G*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_Q_PZA <- (TMLE_Correct_Q*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_BothInorrect_PZA <- (TMLE_Both_Incorrect*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  
  TMLE_Stage2_BothCorrect_PZA_New <- (TMLE_Both_Correct/Pi_n)/Total_Study
  TMLE_Stage2_Correct_G_PZA_New <- (TMLE_Correct_G/Pi_n)/Total_Study
  TMLE_Stage2_Correct_Q_PZA_New <- (TMLE_Correct_Q/Pi_n)/Total_Study
  TMLE_Stage2_BothInorrect_PZA_New <- (TMLE_Both_Incorrect/Pi_n)/Total_Study
  
  
  
  
  
  
  #################################################################################
  #For Estimating Simple TMLE for KMAMK_tx
  #################################################################################
  Interested_IPD = Final_IPD[which(Final_IPD$D_KMAMK_tx==1),]
  Covariates = Interested_IPD[,c(2:26,28:29)]
  Medication_Interest = Interested_IPD$KMAMK_tx2
  Outcome = Interested_IPD$Y
  GLM_Estimate = Func1(Covariates,Medication_Interest,Outcome)
  G = Func2(Covariates,Medication_Interest)
  TMLE_Estimate_KMAMK = Func4(Outcome,GLM_Estimate,G,Medication_Interest)
  GLM_Estimate_Incorrect_KMAMK = Func3(Medication_Interest,Outcome)
  TMLE_Estimate_Incorrect = Func4(Outcome,GLM_Estimate_Incorrect_KMAMK,G,Medication_Interest)
  #################################################################################
  #For Estimating Transportability TMLE for KMAMK_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:26,28:29)]
  Medication_Interest = Interested_IPD$KMAMK_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_KMAMK_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1(Covariates,Medication_Interest,Outcome)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_KMAMK_tx
  Size <- Interested_AD$Sample_size
  P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  #P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  TMLE_Estimate_Transport_KMAMK = Func4(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest)
  #################################################################################
  
  #################################################################################
  #For Estimating Two Stage TMLE for KMAMK_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:26,28:29)]
  Medication_Interest = Interested_IPD$KMAMK_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_KMAMK_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1_IPW(Covariates,Medication_Interest,Outcome,Ind_Interest)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_KMAMK_tx
  Size <- Interested_AD$Sample_size
  P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  #P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6_IPW(Covariates,Medication_Interest,Indicator_1,Ind_Interest)
  G2_Incorrect <- Func7(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  G_Incorrect <- G1*G2_Incorrect
  
  TMLE_Stage1_Estimate_CorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandCorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_CorrectQandIncorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Incorrect,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Incorrect,Medication_Interest,Ind_Interest)
  
  Size_TMLE <- data.frame(cbind(TMLE_Stage1_Estimate_CorrectQandG,TMLE_Stage1_Estimate_IncorrectQandCorrectG,
                                TMLE_Stage1_Estimate_CorrectQandIncorrectG,TMLE_Stage1_Estimate_IncorrectQandG,
                                Stage1_GLM_Estimate,Stage1_GLM_Estimate_Incorrect,Interested_IPD$Study_ID))
  colnames(Size_TMLE) <- c("TMLE_Est_BothCorrect","TMLE_Est_CorrectG","TMLE_Est_CorrectQ","TMLE_Est_BothIncorrectQ",
                           "GLM_Est","GLM_Est_Incorrect","Study_ID")
  TMLE_Both_Correct <- NA
  TMLE_Correct_G <- NA
  TMLE_Correct_Q <- NA
  TMLE_Both_Incorrect <- NA
  Avg_GLM <- NA
  Avg_GLM_Incorrect <- NA
  for(i in 1:length(Interested_AD[,1])){
    New_Data <- Size_TMLE[which(Size_TMLE$Study_ID == Interested_AD$Study_ID[i]),]
    TMLE_Both_Correct[i] <- mean(New_Data$TMLE_Est_BothCorrect)
    TMLE_Correct_G[i] <- mean(New_Data$TMLE_Est_CorrectG)
    TMLE_Correct_Q[i] <- mean(New_Data$TMLE_Est_CorrectQ)
    TMLE_Both_Incorrect[i] <- mean(New_Data$TMLE_Est_BothIncorrectQ)
    Avg_GLM[i] <- mean(New_Data$GLM_Est)
    Avg_GLM_Incorrect[i] <- mean(New_Data$GLM_Est_Incorrect)
  }
  
  
  
  TMLE_Stage2_BothCorrect_KMAMK <- (TMLE_Both_Correct*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_G_KMAMK <- (TMLE_Correct_G*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_Q_KMAMK <- (TMLE_Correct_Q*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_BothInorrect_KMAMK <- (TMLE_Both_Incorrect*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  
  TMLE_Stage2_BothCorrect_KMAMK_New <- (TMLE_Both_Correct/Pi_n)/Total_Study
  TMLE_Stage2_Correct_G_KMAMK_New <- (TMLE_Correct_G/Pi_n)/Total_Study
  TMLE_Stage2_Correct_Q_KMAMK_New <- (TMLE_Correct_Q/Pi_n)/Total_Study
  TMLE_Stage2_BothInorrect_KMAMK_New <- (TMLE_Both_Incorrect/Pi_n)/Total_Study
  
  
  
  
  
  
  #################################################################################
  #For Estimating Simple TMLE for Highquin_tx
  #################################################################################
  Interested_IPD = Final_IPD[which(Final_IPD$D_Highquin_tx==1),]
  Covariates = Interested_IPD[,c(2:27,29)]
  Medication_Interest = Interested_IPD$Highquin_tx2
  Outcome = Interested_IPD$Y
  GLM_Estimate = Func1(Covariates,Medication_Interest,Outcome)
  G = Func2(Covariates,Medication_Interest)
  TMLE_Estimate_Highquin = Func4(Outcome,GLM_Estimate,G,Medication_Interest)
  GLM_Estimate_Incorrect_Highquin = Func3(Medication_Interest,Outcome)
  TMLE_Estimate_Incorrect = Func4(Outcome,GLM_Estimate_Incorrect_Highquin,G,Medication_Interest)
  #################################################################################
  #For Estimating Transportability TMLE for Highquin_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:27,29)]
  Medication_Interest = Interested_IPD$Highquin_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_Highquin_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1(Covariates,Medication_Interest,Outcome)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_Highquin_tx
  Size <- Interested_AD$Sample_size
  P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  #P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  TMLE_Estimate_Transport_Highquin = Func4(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest)
  #################################################################################
  
  #################################################################################
  #For Estimating Two Stage TMLE for Highquin_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:27,29)]
  Medication_Interest = Interested_IPD$Highquin_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_Highquin_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1_IPW(Covariates,Medication_Interest,Outcome,Ind_Interest)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_Highquin_tx
  Size <- Interested_AD$Sample_size
  P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  #P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6_IPW(Covariates,Medication_Interest,Indicator_1,Ind_Interest)
  G2_Incorrect <- Func7(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  G_Incorrect <- G1*G2_Incorrect
  
  TMLE_Stage1_Estimate_CorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandCorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_CorrectQandIncorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Incorrect,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Incorrect,Medication_Interest,Ind_Interest)
  
  Size_TMLE <- data.frame(cbind(TMLE_Stage1_Estimate_CorrectQandG,TMLE_Stage1_Estimate_IncorrectQandCorrectG,
                                TMLE_Stage1_Estimate_CorrectQandIncorrectG,TMLE_Stage1_Estimate_IncorrectQandG,
                                Stage1_GLM_Estimate,Stage1_GLM_Estimate_Incorrect,Interested_IPD$Study_ID))
  colnames(Size_TMLE) <- c("TMLE_Est_BothCorrect","TMLE_Est_CorrectG","TMLE_Est_CorrectQ","TMLE_Est_BothIncorrectQ",
                           "GLM_Est","GLM_Est_Incorrect","Study_ID")
  TMLE_Both_Correct <- NA
  TMLE_Correct_G <- NA
  TMLE_Correct_Q <- NA
  TMLE_Both_Incorrect <- NA
  Avg_GLM <- NA
  Avg_GLM_Incorrect <- NA
  for(i in 1:length(Interested_AD[,1])){
    New_Data <- Size_TMLE[which(Size_TMLE$Study_ID == Interested_AD$Study_ID[i]),]
    TMLE_Both_Correct[i] <- mean(New_Data$TMLE_Est_BothCorrect)
    TMLE_Correct_G[i] <- mean(New_Data$TMLE_Est_CorrectG)
    TMLE_Correct_Q[i] <- mean(New_Data$TMLE_Est_CorrectQ)
    TMLE_Both_Incorrect[i] <- mean(New_Data$TMLE_Est_BothIncorrectQ)
    Avg_GLM[i] <- mean(New_Data$GLM_Est)
    Avg_GLM_Incorrect[i] <- mean(New_Data$GLM_Est_Incorrect)
  }
  
  
  
  TMLE_Stage2_BothCorrect_Highquin <- (TMLE_Both_Correct*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_G_Highquin <- (TMLE_Correct_G*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_Q_Highquin <- (TMLE_Correct_Q*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_BothInorrect_Highquin <- (TMLE_Both_Incorrect*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  
  TMLE_Stage2_BothCorrect_Highquin_New <- (TMLE_Both_Correct/Pi_n)/Total_Study
  TMLE_Stage2_Correct_G_Highquin_New <- (TMLE_Correct_G/Pi_n)/Total_Study
  TMLE_Stage2_Correct_Q_Highquin_New <- (TMLE_Correct_Q/Pi_n)/Total_Study
  TMLE_Stage2_BothInorrect_Highquin_New <- (TMLE_Both_Incorrect/Pi_n)/Total_Study
  
  
  
  
  
  
  #################################################################################
  #For Estimating Simple TMLE for Group5_tx
  #################################################################################
  Interested_IPD = Final_IPD[which(Final_IPD$D_Group5_tx==1),]
  Covariates = Interested_IPD[,c(2:28)]
  Medication_Interest = Interested_IPD$Group5_tx2
  Outcome = Interested_IPD$Y
  GLM_Estimate = Func1(Covariates,Medication_Interest,Outcome)
  G = Func2(Covariates,Medication_Interest)
  TMLE_Estimate_Group5 = Func4(Outcome,GLM_Estimate,G,Medication_Interest)
  GLM_Estimate_Incorrect_Group5 = Func3(Medication_Interest,Outcome)
  TMLE_Estimate_Incorrect = Func4(Outcome,GLM_Estimate_Incorrect_Group5,G,Medication_Interest)
  #################################################################################
  #For Estimating Transportability TMLE for Group5_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:28)]
  Medication_Interest = Interested_IPD$Group5_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_Group5_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1(Covariates,Medication_Interest,Outcome)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_Group5_tx
  Size <- Interested_AD$Sample_size
  P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  #P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  TMLE_Estimate_Transport_Group5 = Func4(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest)
  #################################################################################
  
  #################################################################################
  #For Estimating Two Stage TMLE for Group5_tx
  #################################################################################
  Interested_IPD = Final_IPD
  Covariates = Interested_IPD[,c(2:28)]
  Medication_Interest = Interested_IPD$Group5_tx2
  Outcome = Interested_IPD$Y
  Indicator_1 = Interested_IPD$D_Group5_tx
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Stage1_GLM_Estimate <- Func1_IPW(Covariates,Medication_Interest,Outcome,Ind_Interest)
  Stage1_GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD_Data
  Avg_Covariates <- data.frame(Interested_AD$Year,Interested_AD$Age,Interested_AD$HIV_final_pos,
                               Interested_AD$Past_TB_final_yes,Interested_AD$Country_H,Interested_AD$Country_LM)
  #Need to rescale the age and the Year
  Avg_Indicator <- Interested_AD$D_Group5_tx
  Size <- Interested_AD$Sample_size
  P1 <- Func5(Avg_Covariates,Avg_Indicator)
  #Check the probability ratio
  #P1 = rep(sum(Avg_Indicator)/length(Avg_Indicator),length(Avg_Indicator))
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- Func6_IPW(Covariates,Medication_Interest,Indicator_1,Ind_Interest)
  G2_Incorrect <- Func7(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  G_Incorrect <- G1*G2_Incorrect
  
  TMLE_Stage1_Estimate_CorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandCorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_CorrectQandIncorrectG <- Func4_IPW(Outcome,Stage1_GLM_Estimate,G_Incorrect,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandG <- Func4_IPW(Outcome,Stage1_GLM_Estimate_Incorrect,G_Incorrect,Medication_Interest,Ind_Interest)
  
  Size_TMLE <- data.frame(cbind(TMLE_Stage1_Estimate_CorrectQandG,TMLE_Stage1_Estimate_IncorrectQandCorrectG,
                                TMLE_Stage1_Estimate_CorrectQandIncorrectG,TMLE_Stage1_Estimate_IncorrectQandG,
                                Stage1_GLM_Estimate,Stage1_GLM_Estimate_Incorrect,Interested_IPD$Study_ID))
  colnames(Size_TMLE) <- c("TMLE_Est_BothCorrect","TMLE_Est_CorrectG","TMLE_Est_CorrectQ","TMLE_Est_BothIncorrectQ",
                           "GLM_Est","GLM_Est_Incorrect","Study_ID")
  TMLE_Both_Correct <- NA
  TMLE_Correct_G <- NA
  TMLE_Correct_Q <- NA
  TMLE_Both_Incorrect <- NA
  Avg_GLM <- NA
  Avg_GLM_Incorrect <- NA
  for(i in 1:length(Interested_AD[,1])){
    New_Data <- Size_TMLE[which(Size_TMLE$Study_ID == Interested_AD$Study_ID[i]),]
    TMLE_Both_Correct[i] <- mean(New_Data$TMLE_Est_BothCorrect)
    TMLE_Correct_G[i] <- mean(New_Data$TMLE_Est_CorrectG)
    TMLE_Correct_Q[i] <- mean(New_Data$TMLE_Est_CorrectQ)
    TMLE_Both_Incorrect[i] <- mean(New_Data$TMLE_Est_BothIncorrectQ)
    Avg_GLM[i] <- mean(New_Data$GLM_Est)
    Avg_GLM_Incorrect[i] <- mean(New_Data$GLM_Est_Incorrect)
  }
  
  
  
  TMLE_Stage2_BothCorrect_Group5 <- (TMLE_Both_Correct*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_G_Group5 <- (TMLE_Correct_G*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_Correct_Q_Group5 <- (TMLE_Correct_Q*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  TMLE_Stage2_BothInorrect_Group5 <- (TMLE_Both_Incorrect*Size/Pi_n)/(nrow(Final_IPD)+Sum_AD)
  
  TMLE_Stage2_BothCorrect_Group5_New <- (TMLE_Both_Correct/Pi_n)/Total_Study
  TMLE_Stage2_Correct_G_Group5_New <- (TMLE_Correct_G/Pi_n)/Total_Study
  TMLE_Stage2_Correct_Q_Group5_New <- (TMLE_Correct_Q/Pi_n)/Total_Study
  TMLE_Stage2_BothInorrect_Group5_New <- (TMLE_Both_Incorrect/Pi_n)/Total_Study
  
  
  
  #################################################################################
  #Assign Values
  #################################################################################
  
  TMLE_Stage2_BothCorrect_EMB_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_EMB)
  TMLE_Stage2_Correct_G_EMB_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_EMB)
  TMLE_Stage2_Correct_Q_EMB_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_EMB)
  TMLE_Stage2_BothInorrect_EMB_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_EMB)
  
  TMLE_Stage2_BothCorrect_EMB_New_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_EMB_New)
  TMLE_Stage2_Correct_G_EMB_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_EMB_New)
  TMLE_Stage2_Correct_Q_EMB_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_EMB_New)
  TMLE_Stage2_BothInorrect_EMB_New_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_EMB_New)
  
  TMLE_Estimate_EMB_Transport_Jackknife[kl] = mean(TMLE_Estimate_Transport_EMB)
  
  TMLE_Estimate_EMB_Jackknife[kl] = mean(TMLE_Estimate_EMB)
  GLM_Estimate_Incorrect_EMB_Jackknife[kl] = mean(GLM_Estimate_Incorrect_EMB)
  
  
  
  
  TMLE_Stage2_BothCorrect_AMK_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_AMK)
  TMLE_Stage2_Correct_G_AMK_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_AMK)
  TMLE_Stage2_Correct_Q_AMK_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_AMK)
  TMLE_Stage2_BothInorrect_AMK_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_AMK)
  
  TMLE_Stage2_BothCorrect_AMK_New_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_AMK_New)
  TMLE_Stage2_Correct_G_AMK_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_AMK_New)
  TMLE_Stage2_Correct_Q_AMK_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_AMK_New)
  TMLE_Stage2_BothInorrect_AMK_New_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_AMK_New)
  TMLE_Estimate_AMK_Transport_Jackknife[kl] = mean(TMLE_Estimate_Transport_AMK)
  
  TMLE_Estimate_AMK_Jackknife[kl] = mean(TMLE_Estimate_AMK)
  GLM_Estimate_Incorrect_AMK_Jackknife[kl] = mean(GLM_Estimate_Incorrect_AMK)
  
  
  
  TMLE_Stage2_BothCorrect_CAP_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_CAP)
  TMLE_Stage2_Correct_G_CAP_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_CAP)
  TMLE_Stage2_Correct_Q_CAP_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_CAP)
  TMLE_Stage2_BothInorrect_CAP_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_CAP)
  
  TMLE_Stage2_BothCorrect_CAP_New_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_CAP_New)
  TMLE_Stage2_Correct_G_CAP_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_CAP_New)
  TMLE_Stage2_Correct_Q_CAP_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_CAP_New)
  TMLE_Stage2_BothInorrect_CAP_New_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_CAP_New)
  TMLE_Estimate_CAP_Transport_Jackknife[kl] = mean(TMLE_Estimate_Transport_CAP)
  
  TMLE_Estimate_CAP_Jackknife[kl] = mean(TMLE_Estimate_CAP)
  GLM_Estimate_Incorrect_CAP_Jackknife[kl] = mean(GLM_Estimate_Incorrect_CAP)
  
  
  
  TMLE_Stage2_BothCorrect_CIP_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_CIP)
  TMLE_Stage2_Correct_G_CIP_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_CIP)
  TMLE_Stage2_Correct_Q_CIP_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_CIP)
  TMLE_Stage2_BothInorrect_CIP_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_CIP)
  
  TMLE_Stage2_BothCorrect_CIP_New_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_CIP_New)
  TMLE_Stage2_Correct_G_CIP_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_CIP_New)
  TMLE_Stage2_Correct_Q_CIP_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_CIP_New)
  TMLE_Stage2_BothInorrect_CIP_New_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_CIP_New)
  TMLE_Estimate_CIP_Transport_Jackknife[kl] = mean(TMLE_Estimate_Transport_CIP)
  
  TMLE_Estimate_CIP_Jackknife[kl] = mean(TMLE_Estimate_CIP)
  GLM_Estimate_Incorrect_CIP_Jackknife[kl] = mean(GLM_Estimate_Incorrect_CIP)
  
  
  
  TMLE_Stage2_BothCorrect_CS_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_CS)
  TMLE_Stage2_Correct_G_CS_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_CS)
  TMLE_Stage2_Correct_Q_CS_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_CS)
  TMLE_Stage2_BothInorrect_CS_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_CS)
  
  TMLE_Stage2_BothCorrect_CS_New_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_CS_New)
  TMLE_Stage2_Correct_G_CS_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_CS_New)
  TMLE_Stage2_Correct_Q_CS_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_CS_New)
  TMLE_Stage2_BothInorrect_CS_New_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_CS_New)
  TMLE_Estimate_CS_Transport_Jackknife[kl] = mean(TMLE_Estimate_Transport_CS)
  
  TMLE_Estimate_CS_Jackknife[kl] = mean(TMLE_Estimate_CS)
  GLM_Estimate_Incorrect_CS_Jackknife[kl] = mean(GLM_Estimate_Incorrect_CS)
  
  
  
  TMLE_Stage2_BothCorrect_ETO_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_ETO)
  TMLE_Stage2_Correct_G_ETO_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_ETO)
  TMLE_Stage2_Correct_Q_ETO_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_ETO)
  TMLE_Stage2_BothInorrect_ETO_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_ETO)
  
  TMLE_Stage2_BothCorrect_ETO_New_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_ETO_New)
  TMLE_Stage2_Correct_G_ETO_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_ETO_New)
  TMLE_Stage2_Correct_Q_ETO_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_ETO_New)
  TMLE_Stage2_BothInorrect_ETO_New_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_ETO_New)
  TMLE_Estimate_ETO_Transport_Jackknife[kl] = mean(TMLE_Estimate_Transport_ETO)
  
  TMLE_Estimate_ETO_Jackknife[kl] = mean(TMLE_Estimate_ETO)
  GLM_Estimate_Incorrect_ETO_Jackknife[kl] = mean(GLM_Estimate_Incorrect_ETO)
  
  
  
  TMLE_Stage2_BothCorrect_OFX_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_OFX)
  TMLE_Stage2_Correct_G_OFX_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_OFX)
  TMLE_Stage2_Correct_Q_OFX_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_OFX)
  TMLE_Stage2_BothInorrect_OFX_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_OFX)
  
  TMLE_Stage2_BothCorrect_OFX_New_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_OFX_New)
  TMLE_Stage2_Correct_G_OFX_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_OFX_New)
  TMLE_Stage2_Correct_Q_OFX_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_OFX_New)
  TMLE_Stage2_BothInorrect_OFX_New_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_OFX_New)
  TMLE_Estimate_OFX_Transport_Jackknife[kl] = mean(TMLE_Estimate_Transport_OFX)
  
  TMLE_Estimate_OFX_Jackknife[kl] = mean(TMLE_Estimate_OFX)
  GLM_Estimate_Incorrect_OFX_Jackknife[kl] = mean(GLM_Estimate_Incorrect_OFX)
  
  
  
  TMLE_Stage2_BothCorrect_PAS_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_PAS)
  TMLE_Stage2_Correct_G_PAS_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_PAS)
  TMLE_Stage2_Correct_Q_PAS_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_PAS)
  TMLE_Stage2_BothInorrect_PAS_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_PAS)
  
  TMLE_Stage2_BothCorrect_PAS_New_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_PAS_New)
  TMLE_Stage2_Correct_G_PAS_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_PAS_New)
  TMLE_Stage2_Correct_Q_PAS_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_PAS_New)
  TMLE_Stage2_BothInorrect_PAS_New_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_PAS_New)
  TMLE_Estimate_PAS_Transport_Jackknife[kl] = mean(TMLE_Estimate_Transport_PAS)
  
  TMLE_Estimate_PAS_Jackknife[kl] = mean(TMLE_Estimate_PAS)
  GLM_Estimate_Incorrect_PAS_Jackknife[kl] = mean(GLM_Estimate_Incorrect_PAS)
  
  
  
  TMLE_Stage2_BothCorrect_PTO_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_PTO)
  TMLE_Stage2_Correct_G_PTO_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_PTO)
  TMLE_Stage2_Correct_Q_PTO_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_PTO)
  TMLE_Stage2_BothInorrect_PTO_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_PTO)
  
  TMLE_Stage2_BothCorrect_PTO_New_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_PTO_New)
  TMLE_Stage2_Correct_G_PTO_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_PTO_New)
  TMLE_Stage2_Correct_Q_PTO_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_PTO_New)
  TMLE_Stage2_BothInorrect_PTO_New_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_PTO_New)
  TMLE_Estimate_PTO_Transport_Jackknife[kl] = mean(TMLE_Estimate_Transport_PTO)
  
  TMLE_Estimate_PTO_Jackknife[kl] = mean(TMLE_Estimate_PTO)
  GLM_Estimate_Incorrect_PTO_Jackknife[kl] = mean(GLM_Estimate_Incorrect_PTO)
  
  
  
  TMLE_Stage2_BothCorrect_RIF_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_RIF)
  TMLE_Stage2_Correct_G_RIF_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_RIF)
  TMLE_Stage2_Correct_Q_RIF_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_RIF)
  TMLE_Stage2_BothInorrect_RIF_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_RIF)
  
  TMLE_Stage2_BothCorrect_RIF_New_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_RIF_New)
  TMLE_Stage2_Correct_G_RIF_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_RIF_New)
  TMLE_Stage2_Correct_Q_RIF_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_RIF_New)
  TMLE_Stage2_BothInorrect_RIF_New_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_RIF_New)
  TMLE_Estimate_RIF_Transport_Jackknife[kl] = mean(TMLE_Estimate_Transport_RIF)
  
  TMLE_Estimate_RIF_Jackknife[kl] = mean(TMLE_Estimate_RIF)
  GLM_Estimate_Incorrect_RIF_Jackknife[kl] = mean(GLM_Estimate_Incorrect_RIF)
  
  
  
  TMLE_Stage2_BothCorrect_SM_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_SM)
  TMLE_Stage2_Correct_G_SM_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_SM)
  TMLE_Stage2_Correct_Q_SM_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_SM)
  TMLE_Stage2_BothInorrect_SM_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_SM)
  
  TMLE_Stage2_BothCorrect_SM_New_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_SM_New)
  TMLE_Stage2_Correct_G_SM_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_SM_New)
  TMLE_Stage2_Correct_Q_SM_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_SM_New)
  TMLE_Stage2_BothInorrect_SM_New_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_SM_New)
  TMLE_Estimate_SM_Transport_Jackknife[kl] = mean(TMLE_Estimate_Transport_SM)
  
  TMLE_Estimate_SM_Jackknife[kl] = mean(TMLE_Estimate_SM)
  GLM_Estimate_Incorrect_SM_Jackknife[kl] = mean(GLM_Estimate_Incorrect_SM)
  
  
  
  TMLE_Stage2_BothCorrect_PZA_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_PZA)
  TMLE_Stage2_Correct_G_PZA_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_PZA)
  TMLE_Stage2_Correct_Q_PZA_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_PZA)
  TMLE_Stage2_BothInorrect_PZA_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_PZA)
  
  TMLE_Stage2_BothCorrect_PZA_New_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_PZA_New)
  TMLE_Stage2_Correct_G_PZA_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_PZA_New)
  TMLE_Stage2_Correct_Q_PZA_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_PZA_New)
  TMLE_Stage2_BothInorrect_PZA_New_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_PZA_New)
  TMLE_Estimate_PZA_Transport_Jackknife[kl] = mean(TMLE_Estimate_Transport_PZA)
  
  TMLE_Estimate_PZA_Jackknife[kl] = mean(TMLE_Estimate_PZA)
  GLM_Estimate_Incorrect_PZA_Jackknife[kl] = mean(GLM_Estimate_Incorrect_PZA)
  
  
  
  
  
  TMLE_Stage2_BothCorrect_KMAMK_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_KMAMK)
  TMLE_Stage2_Correct_G_KMAMK_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_KMAMK)
  TMLE_Stage2_Correct_Q_KMAMK_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_KMAMK)
  TMLE_Stage2_BothInorrect_KMAMK_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_KMAMK)
  
  TMLE_Stage2_BothCorrect_KMAMK_New_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_KMAMK_New)
  TMLE_Stage2_Correct_G_KMAMK_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_KMAMK_New)
  TMLE_Stage2_Correct_Q_KMAMK_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_KMAMK_New)
  TMLE_Stage2_BothInorrect_KMAMK_New_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_KMAMK_New)
  TMLE_Estimate_KMAMK_Transport_Jackknife[kl] = mean(TMLE_Estimate_Transport_KMAMK)
  
  TMLE_Estimate_KMAMK_Jackknife[kl] = mean(TMLE_Estimate_KMAMK)
  GLM_Estimate_Incorrect_KMAMK_Jackknife[kl] = mean(GLM_Estimate_Incorrect_KMAMK)
  
  
  
  TMLE_Stage2_BothCorrect_Highquin_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_Highquin)
  TMLE_Stage2_Correct_G_Highquin_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_Highquin)
  TMLE_Stage2_Correct_Q_Highquin_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_Highquin)
  TMLE_Stage2_BothInorrect_Highquin_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_Highquin)
  
  TMLE_Stage2_BothCorrect_Highquin_New_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_Highquin_New)
  TMLE_Stage2_Correct_G_Highquin_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_Highquin_New)
  TMLE_Stage2_Correct_Q_Highquin_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_Highquin_New)
  TMLE_Stage2_BothInorrect_Highquin_New_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_Highquin_New)
  TMLE_Estimate_Highquin_Transport_Jackknife[kl] = mean(TMLE_Estimate_Transport_Highquin)
  
  TMLE_Estimate_Highquin_Jackknife[kl] = mean(TMLE_Estimate_Highquin)
  GLM_Estimate_Incorrect_Highquin_Jackknife[kl] = mean(GLM_Estimate_Incorrect_Highquin)
  
  
  
  TMLE_Stage2_BothCorrect_Group5_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_Group5)
  TMLE_Stage2_Correct_G_Group5_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_Group5)
  TMLE_Stage2_Correct_Q_Group5_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_Group5)
  TMLE_Stage2_BothInorrect_Group5_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_Group5)
  
  TMLE_Stage2_BothCorrect_Group5_New_Jackknife[kl] = sum(TMLE_Stage2_BothCorrect_Group5_New)
  TMLE_Stage2_Correct_G_Group5_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_G_Group5_New)
  TMLE_Stage2_Correct_Q_Group5_New_Jackknife[kl] = sum(TMLE_Stage2_Correct_Q_Group5_New)
  TMLE_Stage2_BothInorrect_Group5_New_Jackknife[kl] = sum(TMLE_Stage2_BothInorrect_Group5_New)
  TMLE_Estimate_Group5_Transport_Jackknife[kl] = mean(TMLE_Estimate_Transport_Group5)
  
  TMLE_Estimate_Group5_Jackknife[kl] = mean(TMLE_Estimate_Group5)
  GLM_Estimate_Incorrect_Group5_Jackknife[kl] = mean(GLM_Estimate_Incorrect_Group5)
  
  #################################################################################
  
  print(kl)
}

