#R scripts for breast cancer data
#These scripts illustrate how experiments in Nukui & Onogi (2023) were conducted.


#Read data######################################################################
#Files were downloaded from 
##https://github.com/cclab-brca/neoadjuvant-therapy-response-predictor

#Read RNA count data
Rawcount <- as.matrix(read.csv("transneo-diagnosis-RNAseq-rawcounts.tsv",
                               header = TRUE, row.names = 1, sep ="\t"))
Rawcount<-t(Rawcount)
dim(Rawcount)
#162 57905
any(is.na(Rawcount))
FALSE

#Read meta data
TableS1 <- read.csv("SupplementaryTable1.csv", header = TRUE)
unique(TableS1$RCB.category)
#"RCB-I"   "pCR"     "RCB-II"  "RCB-III" NA 
sum(is.na(TableS1$RCB.category))
7

#Read DNA-related data
MS <- as.matrix(read.csv("transneo-diagnosis-mutational-signatures.tsv",
                         header = TRUE, row.names = 1, sep ="\t"))
HRD <- as.matrix(read.csv("transneo-diagnosis-HRD.tsv",
                          header = TRUE, row.names = 1, sep ="\t"))
PP <- as.matrix(read.csv("transneo-diagnosis-ASCAT-purity.tsv",
                         header = TRUE, row.names = 1, sep ="\t"))
any(is.na(MS))
FALSE
any(is.na(HRD))
FALSE
any(is.na(PP))
FALSE

#Read digital pathology-related data
DP <- as.matrix(read.csv("transneo-diagnosis-DigPathology.tsv",
                         header = TRUE, row.names = 1, sep ="\t"))
any(is.na(DP))
FALSE

#=>DNA and digital pathology-related data were not used for comparison
#because of their low contribution to the response


#Create data####################################################################
#Extract ID common for all data
ID <- TableS1$Donor.ID[!is.na(TableS1$RCB.category) & 
                         rowSums(is.na(TableS1[,c(2:11,13:21)])) == 0]
ID <- ID[is.element(ID, rownames(Rawcount))]
ID <- ID[is.element(ID, rownames(MS))]
ID <- ID[is.element(ID, rownames(HRD))]
ID <- ID[is.element(ID, rownames(PP))]
ID <- ID[is.element(ID, rownames(DP))]
length(ID)
149

#Objective variable
Y <- TableS1$RCB.category[is.element(TableS1$Donor.ID, ID)]
length(Y)
149
sum(is.na(Y))
0

#Prepare RNA count data
X.RNA <- Rawcount[ID, ]
dim(X.RNA)
#149 57905

##Select transcripts
Wilcox.p <- numeric(ncol(X.RNA))
Y.twogrp <- Y
Y.twogrp[Y.twogrp != "pCR"] <- "RCB"
table(Y.twogrp)
for(i in 1:ncol(X.RNA)){
  Wilcox.p[i] <- wilcox.test(X.RNA[Y.twogrp == "pCR", i],
                             X.RNA[Y.twogrp == "RCB", i])$p.value
}
sum(is.na(Wilcox.p))
8669

##Use top 2000 transcripts
X.RNA.select <- X.RNA[, rank(Wilcox.p) < 2001]
###confirm
Wilcox.p <- numeric(ncol(X.RNA.select))
for(i in 1:ncol(X.RNA.select)){
  Wilcox.p[i] <- wilcox.test(X.RNA.select[Y.twogrp == "pCR", i],
                             X.RNA.select[Y.twogrp == "RCB", i])$p.value
}
sum(is.na(Wilcox.p))
0
range(Wilcox.p)
#2.536743e-08 2.699095e-03

#Prepare other data
X.DNA <- cbind(MS[ID,], HRD[ID, "HRD.sum", drop=FALSE], PP[ID,])
X.DP <- as.matrix(DP[ID, ])
X.CL <- TableS1[is.element(TableS1$Donor.ID, ID), c(2:11,13:21)]

#Output X and Y
write.csv(data.frame(Y = Y, row.names=ID),
          "Sammut2022_Category.csv")
write.csv(X.RNA.select,"Sammut2022_RNA.csv")
write.csv(X.DNA,"Sammut2022_DNA.csv")
write.csv(X.DP,"Sammut2022_DP.csv")
write.csv(X.CL,"Sammut2022_CL.csv")


#Read files#####################################################################
X.RNA <- as.matrix(read.csv("Sammut2022_RNA.csv", header = TRUE, row.names = 1))
X.DNA <- as.matrix(read.csv("Sammut2022_DNA.csv", header = TRUE, row.names = 1))
X.DP <- as.matrix(read.csv("Sammut2022_DP.csv", header = TRUE, row.names = 1))
X.CL <- read.csv("Sammut2022_CL.csv", header = TRUE, row.names = 1)
Y <- unlist(read.csv("Sammut2022_Category.csv", header = TRUE, row.names = 1))


#CV preparation#################################################################
#Randomly split data to five folds
N <- length(Y)
M <- 5#Number of folds
v <- rep(1:M, each = 30)
Split <- sample(v, N, replace = FALSE)

#Output
write.csv(data.frame(Split = Split, Y, row.names=rownames(X.RNA)), "Sammut2022_Y.csv")


#Prediction using RNA count data################################################
library(stacking)
library(caret)
library(epiR)#Package for calculating the kappa coefficient

#Base and meta learners
Method <- list(ranger = data.frame(mtry = c(10, 100, 500),
                                   splitrule = c("extratrees", NA, NA),
                                   min.node.size = c(1, 5, 10)),
               xgbTree = data.frame(colsample_bytree = c(0.6, 0.8),
                                    subsample = c(0.5, 1),
                                    nrounds = c(50, 150),
                                    max_depth = c(6, NA),
                                    eta = c(0.3, NA),
                                    gamma = c(0, NA),
                                    min_child_weight = c(1, NA)),
               gbm = data.frame(interaction.depth = c(1, 3, 5),
                                n.trees = c(50, 100, 150),
                                shrinkage = c(0.1, NA, NA),
                                n.minobsinnode = c(10, NA, NA)),
               svmPoly = data.frame(C = c(0.25, 0.5, 1),
                                    scale = c(0.001, 0.01, 0.1),
                                    degree = c(1, NA, NA)),
               glmnet = data.frame(alpha = c(1, 0.8, 0.6, 0.4, 0.2, 0),
                                   lambda = rep(NA, 6)),
               pls = data.frame(ncomp = seq(2, 70, 10))
)

Metamethod <- "ranger"
Method.compared <- c("glmnet", "ranger", "gbm", "xgbTree", "svmPoly", "pls")

Prediction.RNA <- numeric(N)

for(fold in 1:M){
  
  #Training base and meta models
  Y.train <- factor(Y[Split != fold])
  X.train <- X.RNA[Split != fold, ]
  
  Basemodel <- train_basemodel(X.train, Y.train, 5, Method, 16)
  Metamodel <- train_metamodel(Basemodel, NULL, Metamethod)
  
  #Predict
  X.test <- X.RNA[Split == fold, ]
  stacking_train_result <- list(base = Basemodel, meta = Metamodel)
  temp <- stacking_predict(X.test, stacking_train_result)
  Prediction.RNA[Split == fold] <- temp
  rm(stacking_train_result)
}

v <- table(True = Y, Pred = Prediction.RNA)
epi.kappa(v, alternative = "greater")


#Methods compared
Prediction.RNA.single <- matrix(NA, nrow = N, ncol = length(Method.compared))
colnames(Prediction.RNA.single) <- Method.compared

for(m in Method.compared){
  for(fold in 1:M){
    
    Y.train <- Y[Split != fold]
    X.train <- X.RNA[Split != fold, ]
    result <- train(X.train, Y.train, method = m)
    
    #Predict
    X.test <- X.RNA[Split == fold, ]
    v <- as.character(predict(result, X.test))
    Prediction.RNA.single[Split == fold, m] <- v
  }
}

for(m in Method.compared){
  v <- table(True = Y, Pred = Prediction.RNA.single[,m])
  cat(m, as.numeric(epi.kappa(v, alternative = "greater")$kappa),"\n")
}


#Prediction using CL############################################################
X.CL$T.stage <- factor(X.CL$T.stage)
X.CL$LN.status.at.diagnosis <- factor(X.CL$LN.status.at.diagnosis)
X.CL$Histology <- factor(X.CL$Histology)
X.CL$ER.status <- factor(X.CL$ER.status)
X.CL$HER2.status <- factor(X.CL$HER2.status)
X.CL$NAT.regimen <- factor(X.CL$NAT.regimen)
X.CL$Surgery.type <- factor(X.CL$Surgery.type)
X.CL$LVI <- factor(X.CL$LVI)
X.CL <- model.matrix(~., data = X.CL)

#Base and meta learners
Method <- list(ranger = data.frame(mtry = c(5, 20, 44),
                                   splitrule = c("extratrees", NA, NA),
                                   min.node.size = c(1, 5, 10)),
               xgbTree = data.frame(colsample_bytree = c(0.6, 0.8),
                                    subsample = c(0.5, 1),
                                    nrounds = c(50, 150),
                                    max_depth = c(6, NA),
                                    eta = c(0.3, NA),
                                    gamma = c(0, NA),
                                    min_child_weight = c(1, NA)),
               gbm = data.frame(interaction.depth = c(1, 3, 5),
                                n.trees = c(50, 100, 150),
                                shrinkage = c(0.1, NA, NA),
                                n.minobsinnode = c(10, NA, NA)),
               svmPoly = data.frame(C = c(0.25, 0.5, 1),
                                    scale = c(0.001, 0.01, 0.1),
                                    degree = c(1, NA, NA)),
               glmnet = data.frame(alpha = c(1, 0.8, 0.6, 0.4, 0.2, 0),
                                   lambda = rep(NA, 6)),
               pls = data.frame(ncomp = seq(2, 15, 2))
)

Metamethod <- "ranger"
Method.compared <- c("glmnet", "ranger", "gbm", "xgbTree", "svmPoly", "pls")

Prediction.CL <- numeric(N)

for(fold in 1:M){
  
  #Training base and meta models
  Y.train <- factor(Y[Split != fold])
  X.train <- X.CL[Split != fold, ]
  
  Basemodel <- train_basemodel(X.train, Y.train, 5, Method, 16)
  Metamodel <- train_metamodel(Basemodel, NULL, Metamethod)
  
  #Predict
  X.test <- X.CL[Split == fold, ]
  stacking_train_result <- list(base = Basemodel, meta = Metamodel)
  temp <- stacking_predict(X.test, stacking_train_result)
  Prediction.CL[Split == fold] <- temp
  rm(stacking_train_result)
}

v <- table(True = Y, Pred = Prediction.CL)
epi.kappa(v, alternative = "greater")


#Methods compared
Prediction.CL.single <- matrix(NA, nrow = N, ncol = length(Method.compared))
colnames(Prediction.CL.single) <- Method.compared

for(m in Method.compared){
  for(fold in 1:M){
    
    Y.train <- Y[Split != fold]
    X.train <- X.CL[Split != fold, ]
    
    result <- train(X.train, Y.train, method = m)
    
    #Predict
    X.test <- X.CL[Split == fold, ]
    v <- as.character(predict(result, X.test))
    Prediction.CL.single[Split == fold, m] <- v
  }
}

for(m in Method.compared[-6]){
  v <- table(True = Y, Pred = Prediction.CL.single[, m])
  cat(m,as.numeric(epi.kappa(v, alternative = "greater")$kappa),"\n")
}
#The sixth method (pls) failed because RCB-I was missed in the predicted categories
#Calculate the kappa coefficient by adding the category as the last element
m <- Method.compared[6]
v <- table(True = c(Y,"RCB-I"), Pred = c(Prediction.CL.single[, m], "RCB-I"))
v[2, 2] <- 0
as.numeric(epi.kappa(v, alternative = "greater")$kappa)


#Prediction using RNA + CL data#################################################
X.CL <- read.csv("Sammut2022_CL.csv", header = TRUE, row.names = 1)
X.CL$T.stage <- factor(X.CL$T.stage)
X.CL$LN.status.at.diagnosis <- factor(X.CL$LN.status.at.diagnosis)
X.CL$Histology <- factor(X.CL$Histology)
X.CL$ER.status <- factor(X.CL$ER.status)
X.CL$HER2.status <- factor(X.CL$HER2.status)
X.CL$NAT.regimen <- factor(X.CL$NAT.regimen)
X.CL$Surgery.type <- factor(X.CL$Surgery.type)
X.CL$LVI <- factor(X.CL$LVI)
X.RNACL <- data.frame(X.RNA, X.CL)
X.RNACL <- model.matrix(~., data = X.RNACL)

#Base and meta learners
Method <- list(ranger = data.frame(mtry = c(10, 100, 500),
                                   splitrule = c("extratrees", NA, NA),
                                   min.node.size = c(1, 5, 10)),
               xgbTree = data.frame(colsample_bytree = c(0.6, 0.8),
                                    subsample = c(0.5, 1),
                                    nrounds = c(50, 150),
                                    max_depth = c(6, NA),
                                    eta = c(0.3, NA),
                                    gamma = c(0, NA),
                                    min_child_weight = c(1, NA)),
               gbm = data.frame(interaction.depth = c(1, 3, 5),
                                n.trees = c(50, 100, 150),
                                shrinkage = c(0.1, NA, NA),
                                n.minobsinnode = c(10, NA, NA)),
               svmPoly = data.frame(C = c(0.25, 0.5, 1),
                                    scale = c(0.001, 0.01, 0.1),
                                    degree = c(1, NA, NA)),
               glmnet = data.frame(alpha = c(1, 0.8, 0.6, 0.4, 0.2, 0),
                                   lambda = rep(NA, 6)),
               pls = data.frame(ncomp = seq(2, 70, 10))
)

Metamethod <- "ranger"
Method.compared <- c("glmnet", "ranger", "gbm", "xgbTree", "svmPoly", "pls")

Prediction.RNACL <- numeric(N)

for(fold in 1:M){
  
  #Training base and meta models
  Y.train <- factor(Y[Split != fold])
  X.train <- X.RNACL[Split != fold, ]
  
  Basemodel <- train_basemodel(X.train, Y.train, 5, Method, 16)
  Metamodel <- train_metamodel(Basemodel, NULL, Metamethod)
  
  #Predict
  X.test <- X.RNACL[Split == fold, ]
  stacking_train_result <- list(base = Basemodel, meta = Metamodel)
  temp <- stacking_predict(X.test, stacking_train_result)
  Prediction.RNACL[Split == fold] <- temp
  rm(stacking_train_result)
}

v <- table(True = Y, Pred = Prediction.RNACL)
epi.kappa(v, alternative = "greater")


#Methods compared
Prediction.RNACL.single <- matrix(NA, nrow = N, ncol = length(Method.compared))
colnames(Prediction.RNACL.single) <- Method.compared

for(m in Method.compared){
  for(fold in 1:M){
    
    Y.train <- Y[Split != fold]
    X.train <- X.RNACL[Split != fold, ]
    
    result <- train(X.train, Y.train, method = m)
    
    #Predict
    X.test <- X.RNACL[Split == fold, ]
    v <- as.character(predict(result, X.test))
    Prediction.RNACL.single[Split == fold, m] <- v
  }
}

for(m in Method.compared){
  v <- table(True = Y, Pred = Prediction.RNACL.single[, m])
  cat(m, as.numeric(epi.kappa(v, alternative = "greater")$kappa), "\n")
}

