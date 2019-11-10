#####################
#Problem 1: 2, 2, 2, 1, 1
#####################
xTest <- matrix(0, nrow = rows, ncol = cols)
xTrain <- matrix(0, nrow = rows, ncol = cols)

xTrain[,1] <- c(1, 2, 3, 4, 5)
xTrain[,2] <- c(6, 7, 8, 9, 10)
yTrain <- matrix(c(rep(1, 2), rep(2, 3)), nrow = rows, ncol = 1)

xTest[,1] <- c(5, 4, 3, 2, 1)
xTest[,2] <- c(10, 9 , 8 , 7, 6)
yTest <- matrix(c(rep(1, 2), rep(2, 3)), nrow = rows, ncol = 1)
