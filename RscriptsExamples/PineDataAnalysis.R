#R scripts for pine data

#These scripts illustrate how experiments in Nukui & Onogi (2023) were conducted.
#But please note that these scripts were not the ones actually used.
#The scripts actually used saved all training results of stacking and 
#methods compared for each trait in different work space.


#Read files#####################################################################
#Files were downloaded from 
##https://academic.oup.com/genetics/article/190/4/1503/6064084#supplementary-data
##files4.xlsx was manually converted to files4.csv
Geno <- read.csv("Snp_Data.csv", header = T, row.names = 1)
Pheno <- read.csv("files4.csv", header = T, row.names = 1)
identical(rownames(Geno), rownames(Pheno))
TRUE


#Edit genotypes#################################################################
#Impute missings with averages
unique(unlist(Geno))
#-9  2  1  0
for(i in 1:ncol(Geno)){
  v <- Geno[, i]
  if(any(v == -9)) v[v == -9] <- mean(v[v != -9])
  Geno[, i] <- v
}
any(unlist(Geno) == -9)
FALSE

#See allele frequencies
Af <- colSums(Geno) / (2*nrow(Geno))
hist(Af)#2 is major allele
range(Af)
#0.4978094 1.0000000

#Output objects
colnames(Geno) <- 1:ncol(Geno)
write.csv(Geno, "Resende2012_Geno.csv")
write.csv(Pheno, "Resende2012_Pheno.csv")


#Read files#####################################################################
X <- as.matrix(read.csv("Resende2012_Geno.csv", header = TRUE, row.names = 1))
Y <- as.matrix(read.csv("Resende2012_Pheno.csv", header = TRUE, row.names = 1))

#Use only polymorphic markers
V <- apply(X, 2, var)
X <- X[, V > 0]

X <- scale(X)
Y <- scale(Y)

any(is.na(X))
FALSE
any(is.na(Y))
FALSE

#See distributions
for(i in 1:ncol(Y)){
  hist(Y[, i], main = paste(i, colnames(Y)[i]))
}


#Create data####################################################################
#Randomly split data to 8:2
N <- nrow(Y)
v <- c(rep(1:10, each = 93)[-c((N + 1):(930))])
#1:8=>training(744),9-10:testing(182)
Split <- sample(v, N, replace = FALSE)

#Confirm the number of samples in each division
for(i in 1:ncol(Y)){
  cat(i, sum(!is.na(Y[, i][Split < 9])), 
      sum(!is.na(Y[, i][Split > 8])), "\n")
}

#Output with split
write.csv(data.frame(Split = Split, Y), "Resende2012_Y.csv")


#Base and meta learners#########################################################
Method <- list(ranger = data.frame(mtry = c(50, 1000, ncol(X)),
                                   splitrule = c("variance",NA,NA),
                                   min.node.size = c(5, 20, 50)),
               xgbTree = data.frame(colsample_bytree = c(0.6, 0.8),
                                    subsample = c(0.5, 1),
                                    nrounds = c(50, 150),
                                    max_depth = c(6, NA),
                                    eta = c(0.3, NA),
                                    gamma = c(0, NA),
                                    min_child_weight = c(1, NA)),
               gbm = data.frame(interaction.depth = c(2, 4, 6),
                                n.trees = c(50, 100, 150),
                                shrinkage = c(0.1, NA, NA),
                                n.minobsinnode = c(10, NA, NA)),
               svmRadial = data.frame(C = c(0.25, 0.5, 1),
                                      sigma = c(1e-5, 1e-4, 1e-3)),
               glmnet = data.frame(alpha = c(1, 0.8, 0.6, 0.4, 0.2, 0),
                                   lambda = rep(NA, 6)),
               pls = data.frame(ncomp = seq(10, 500, 80))
)

Metamethod <- "glmnet"


#Stacking#######################################################################
library(stacking)

Trait <- colnames(Y)
Accuracy_stacking_R <- numeric(length(Trait))
names(Accuracy_stacking_R) <- Trait
Accuracy_stacking_RMSE <- Accuracy_stacking_R

for(trait in Trait){
  
  #Training base and meta models
  Y.train <- Y[Split < 9, trait]
  X.train <- X[Split < 9, ]
  X.train <- X.train[!is.na(Y.train), ]
  Y.train <- Y.train[!is.na(Y.train)]
  
  Basemodel <- train_basemodel(X.train, Y.train, 5, Method, 16)
  #=>requires large memory (>64GB)
  Metamodel <- train_metamodel(Basemodel, NULL, Metamethod)
  
  #Prediction
  Y.test <- Y[Split > 8, trait]
  X.test <- X[Split > 8, ]
  X.test <- X.test[!is.na(Y.test), ]
  Y.test <- Y.test[!is.na(Y.test)]
  stacking_train_result <- list(base = Basemodel, meta = Metamodel)
  temp <- stacking_predict(X.test, stacking_train_result)
  Accuracy_stacking_R[trait] <- cor(Y.test, temp, use = "pairwise.complete.obs")
  Accuracy_stacking_RMSE[trait] <- sqrt(mean((Y.test - temp)^2, na.rm=T))
}


#Methods compared###############################################################
library(caret)

Method.compared <- c("glmnet", "ranger", "gbm", "xgbTree", "svmRadial", "pls")

Accuracy_compared_R <- matrix(0, length(Trait), length(Method.compared))
colnames(Accuracy_compared_R) <- Method.compared
rownames(Accuracy_compared_R) <- Trait
Accuracy_compared_RMSE <- Accuracy_compared_R

for(trait in Trait){
  
  #Split data
  Y.train <- Y[Split < 9, trait]
  X.train <- X[Split < 9, ]
  X.train <- X.train[!is.na(Y.train), ]
  Y.train <- Y.train[!is.na(Y.train)]
  Y.test <- Y[Split > 8, trait]
  X.test <- X[Split > 8, ]
  X.test <- X.test[!is.na(Y.test), ]
  Y.test <- Y.test[!is.na(Y.test)]
  
  for(m in Method.compared){
    
    temp <- train(X.train, Y.train, method = m)
    v <- as.numeric(predict(temp, X.test))
    Accuracy_compared_R[trait, m] <- cor(Y.test, v, use = "pairwise.complete.obs")
    Accuracy_compared_RMSE[trait, m] <- sqrt(mean((Y.test - v)^2, na.rm=T))
  }
}
