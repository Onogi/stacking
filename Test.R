N<-100
P<-200
X<-matrix(rnorm(N*P),N,P)
Y<-rowSums(X[,1:50])+rnorm(N,0,sqrt(0.2*var(rowSums(X[,1:50]))))
Y<-as.numeric(scale(Y))
newX<-matrix(rnorm(N*P),N,P)
newY<-rowSums(newX[,1:50])+rnorm(N,0,sqrt(0.2*var(rowSums(newX[,1:50]))))
newY<-as.numeric(scale(newY))

Nfold<-5
Method<-list(glmnet=data.frame(alpha=c(0,1),lambda=c(1,NA)),
             pls=data.frame(ncomp=50))
core<-3

A<-train_basemodel(X, Y, Nfold, Method, core)
B1<-train_metamodel(A, c(1),"lm")
B2<-train_metamodel(A, NULL, "lm")
B3<-train_metamodel(A, c(2,3), "lm")

Result1<-list(base = A, meta = B1)
Result2<-list(base = A, meta = B2)
Result3<-list(base = A, meta = B3)
Result4<-stacking_train(X,Y,Nfold,Method,"lm",3)

Predict1<-stacking_predict(newX,Result1)
Predict2<-stacking_predict(newX,Result2)
Predict3<-stacking_predict(newX,Result3)
Predict4<-stacking_predict(newX,Result4)
plot(newY,Predict1)
plot(newY,Predict2)
plot(newY,Predict3)
plot(newY,Predict4)

#When core > length(L)
Method<-list(glmnet=data.frame(alpha=c(0,0.2,0.4,1),lambda=c(1,NA,NA,NA)),
             pls=data.frame(ncomp=50))

A<-train_basemodel(X, Y, Nfold, Method, core)
B1<-train_metamodel(A, c(1),"lm")
B2<-train_metamodel(A, NULL, "lm")
B3<-train_metamodel(A, c(2,3), "lm")

Result1<-list(base = A, meta = B1)
Result2<-list(base = A, meta = B2)
Result3<-list(base = A, meta = B3)
Result4<-stacking_train(X,Y,Nfold,Method,"lm",3)

Predict1<-stacking_predict(newX,Result1)
Predict2<-stacking_predict(newX,Result2)
Predict3<-stacking_predict(newX,Result3)
Predict4<-stacking_predict(newX,Result4)
plot(newY,Predict1)
plot(newY,Predict2)
plot(newY,Predict3)
plot(newY,Predict4)

#increase more
Method<-list(glmnet=data.frame(alpha=c(0,0.2,0.4,1),lambda=c(0.1,1,10,NA)),
             pls=data.frame(ncomp=50))

A<-train_basemodel(X, Y, Nfold, Method, core)
B1<-train_metamodel(A, c(1),"lm")
B2<-train_metamodel(A, NULL, "lm")
B3<-train_metamodel(A, c(2,3), "lm")

Result1<-list(base = A, meta = B1)
Result2<-list(base = A, meta = B2)
Result3<-list(base = A, meta = B3)
Result4<-stacking_train(X,Y,Nfold,Method,"lm",3)

Predict1<-stacking_predict(newX,Result1)
Predict2<-stacking_predict(newX,Result2)
Predict3<-stacking_predict(newX,Result3)
Predict4<-stacking_predict(newX,Result4)
plot(newY,Predict1)
plot(newY,Predict2)
plot(newY,Predict3)
plot(newY,Predict4)