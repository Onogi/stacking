train_basemodel_core <- function(repeat.parLapply, division, l, core, x, y, exclude){
  
  library(snow)
  
  x.train <- x[-exclude, ]
  y.train <- y[-exclude]
  
  cl <- makeCluster(core, type="SOCK")
  clusterExport(cl, c("x.train",
                      "y.train",
                      "train"), envir = environment())
  
  train_result <- NULL
  for(rp in 1:repeat.parLapply){
    
    cat(rp, "\n")
    train_result <- c(train_result,
                      parLapply(cl, l[division[, rp]],
                                function(m){
                                  result <- train(x.train, y.train, method = m$method, tuneGrid = m$hyp)
                                  result$trainingData <- NULL
                                  result
                                })
    )
  }
  stopCluster(cl)
  
  train_result
}