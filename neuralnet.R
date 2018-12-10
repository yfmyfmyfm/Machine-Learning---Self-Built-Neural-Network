
args = commandArgs(TRUE)
nueral = function(file, cv, rate, enpochs) {
  library(foreign)
  ######################
  #欠！！！####trycatch reading error
  ######################
  sonar = read.arff(file)
  
  fold = c()
  yy = c()
  pre = c()
  pout = c()
  row = dim(sonar)[1]
  col = length(sonar)
  nu = col-1
  for (i in 1:nu) {
    if (!is.numeric(sonar[,i])) {
      cat(sep = "", "not all variables are numeric! please upload another dataset!", "\n")
      return(NULL)
    }
  }
  if (length(table(sonar[,col])) != 2) {
    cat(sep = "", "the class level is more than 2, cannot apply binary classification", "\n")
    return(NULL)
  }
  if (cv > row) {
    cat(sep = "", "Invalid! cross validation groups are more than sample size!", "\n")
    return(NULL)
  }
  x =  sonar[,-col]
  #seperate class and attributes
  y = matrix(sonar[,length(sonar)], ncol = 1)
  y[y== "Rock"] = 0
  y[y == "Mine"] = 1
  y = as.numeric(y)
  y = t(y)
  x = t(x)
  #construct a list to store the result of cvs
  yfinal = 0
  accuracy = 0
  #calculate sigma
  sigma = function(l) {
    a = 1 / (1+exp(-l))
    return(a)
  }
  
  #set cv group number
  Mine = which(y == 1)
  set.seed(22)
  Mine = sample(Mine,length(Mine))
  Mine = matrix(Mine, ncol = 1)
  Roch = which(y == 0)
  set.seed(1314)
  Roch = sample(Roch, length(Roch))
  Roch = matrix(Roch, ncol = 1)
  renewcv = rbind(Mine, Roch)
  cvm = c()
  start = 0
  for (i in 1:length(renewcv)) {
    if (start >= cv) {
      start = 0
    }
    cvm = rbind(cvm, start)
    start = start + 1
  }
  pointcv = matrix(cbind(renewcv, cvm), ncol= 2, byrow = F)
  ordercv = pointcv[order(pointcv[,1]),]
  cv.id = ordercv[,2]
  
  set.seed(1994)
  w1 = replicate(nu, expr = {runif(nu,-1,1)})
  set.seed(08)
  w2 = matrix(runif(nu, -1, 1), ncol = 1)
  set.seed(1992)
  bias1 = matrix(runif(nu, -1, 1), ncol = 1)
  set.seed(17)
  bias2 = runif(1, -1, 1)
  
  for (i in 0:(cv-1)) { 
    test <- cv.id == i
    train <- cv.id != i
    #initialize w and 2 bias in 2 tiers
    for (j in 1:enpochs) {  
      set.seed(05)
      for (k in sample(1:row)) {
        if (k %in% which(train)) {
          #forth prog
          net1bb = t(w1) %*% x[,k]
          net1 = net1bb + bias1
          z = sigma(net1)
          net2bb = t(w2) %*% z
          net2 = net2bb + bias2
          yhat = sigma(net2)
          error = as.numeric(y[,k] - yhat)
          #back prog
          cw2nb = rate * error * z
          cbias2 = rate * error
          cw1nb = rate * (w2 * error * z * (1-z)) %*% x[,k]
          cbias1 = rate * w2 * error * z * (1-z)
          w1 = w1+cw1nb
          w2 = w2+cw2nb
          bias1 = bias1 + cbias1
          bias2 = bias2 + cbias2
        }
      }
    }
    #predict
    for (k in 1:row) {
      if (k %in% which(test)) {
        net1final = t(w1) %*% x[,k] + bias1
        zfinal = sigma(net1final)
        net2final = t(w2) %*% zfinal + bias2
        yhatfinal = sigma(net2final)
        predict = 0
        if (yhatfinal>=0.5) {
          predict = 1
        }
        if (predict == y[k]) {
          yfinal = yfinal + 1
        }
        fold[k] = i+1
        yy[k] = y[k]
        pre[k] = predict
        pout[k] = yhatfinal
      }
    }
  }
  fold = matrix(fold, ncol = 1, byrow = F)
  fold = data.frame(fold)
  yy = matrix(yy, ncol = 1, byrow = F)
  yy = data.frame(yy)
  pre = matrix(pre, ncol = 1, byrow = F)
  pre = data.frame(pre)
  pout = matrix(pout, ncol = 1, byrow = F)
  pout = data.frame(pout)
  printoutput = cbind(fold, pre, yy, pout)
  printoutput = data.frame(printoutput)
  names(printoutput) = c("fold_of_instance", "predicted_class", "actual_class", "confidence_of_prediction")
  print(printoutput)
  #tp1 = sum(printoutput$predicted_class == 1 & printoutput$actual_class == 1) / sum(printoutput$actual_class == 1)
  #fp1 = sum(printoutput$predicted_class == 1 & printoutput$actual_class == 0) / sum(printoutput$actual_class == 0)
  #t1f1t0f0 = c(tp1, fp1)
  accuracy = yfinal / row
  cat(sep = "", "the accuracy is: ", accuracy, "\n")
  #return(t1f1t0f0)
  #return(accuracy)
}

#f = c(5, 10, 15, 20, 25)
#ep = c(25,50,75,100)
#ac = list()
#ac1 = list()
#for (i in 1:5) { 
#  ac[[i]] = nueral("sonar.arff", 10, ep[i], 0.1,0.5)
#  cat(sep = "   ", ep[i])
#  ac1[[i]] = nueral("sonar.arff", f[i], 50, 0.1,0.5)
#  cat(sep = "   ", f[i])
#}

#lr <- data.frame(matrix(unlist(ac1), ncol= 1, byrow=T))
#lr = cbind(data.frame(f), lr)
#names(lr) = c("X1", "X2")
#plot(lr$X1, lr$X2, xlab = "number of folds", ylab = "accuracy", main = "given epochs = 50, different accuracy with folds = 5,10, 15,20, 25", type = "l")
#points(lr$X1, lr$X2)

#nueral("sonar.arff", 10, 25, 0.1)
nueral(args[1], as.numeric(args[2]), as.numeric(args[3]), as.numeric(args[4]))
