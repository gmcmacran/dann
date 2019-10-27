# Example 16.1 from experimental design book page 686 and 692
# SSTR in book is B in dann
# SSE in book is W in dann
# SSTO in book is Sigma

rows <- 19
cols <- 2

x0 <- matrix(0, nrow = rows, ncol = cols)

x0[,1] <- c(11, 17, 16, 14, 15, 12, 10, 15, 19, 11, 23, 20, 18, 17, 27, 33, 22, 26, 28)
#x0[,2] <- c(11, 17, 16, 14, 15, 12, 10, 15, 19, 11, 23, 20, 18, 17, 27, 33, 22, 26, 28)
x0[,2] <- 1:rows

y0 <- matrix(c(rep(1, 5), rep(2, 5), rep(3, 4), rep(4, 5)), nrow = rows, ncol = 1)


# Group wise means
grandMean <- matrix(colMeans(x0), nrow = 1, ncol = ncol(x0))
groupMeans <- matrix(0, nrow = length(unique(y0)), ncol = ncol(x0))

for(i in seq_along(1:length(unique(y0))) ){
  groupMeans[i, ] <- mean(x0[which(y0 == unique(y0)[i, ]), ], )
}
groupMeans
grandMean

# Weighted between sum of squares
betweenSS <- matrix(-1, nrow = ncol(grandMean), ncol = ncol(grandMean))
pis <- matrix(c(5, 5, 4, 5), nrow = length(unique(y0)), ncol = 1)

for(i in seq_along(1:nrow(betweenSS))) {
  for(j in seq_along(1:ncol(betweenSS))) {
    temp <- matrix(-1, nrow = nrow(groupMeans), ncol = 1)
    for(k in seq_along(1:nrow(temp))) {
      tempM1 <- groupMeans[k, ] - grandMean
      tempM2 <- groupMeans[k, ] - grandMean

      tempM1 <- matrix(tempM1, nrow = 1, ncol = length(tempM1))
      tempM2 <- matrix(tempM2, nrow = 1, ncol = length(tempM2))
      tempM2 <- t(tempM2)

      temp[k, 1] <- pis[k]*tempM1 %*% tempM2
    }
    betweenSS[i, j] <- colSums(temp)
  }
}
betweenSS

# Weighted within sum of squares
withinSS <- matrix(-1, nrow = ncol(grandMean), ncol = ncol(grandMean))

for(i in seq_along(1:nrow(withinSS))) {
  for(j in seq_along(1:ncol(withinSS))) {
    temp <- matrix(-1, nrow = nrow(x0), ncol = 1)
    for(k in seq_along(1:nrow(temp))) {

      currentGroupMean <- groupMeans[which(y0[k,] == unique(y0))]

      tempM1 <- x0[k, ] - currentGroupMean
      tempM2 <- x0[k, ] - currentGroupMean

      tempM1 <- matrix(tempM1, nrow = 1, ncol = length(tempM1))
      tempM2 <- matrix(tempM2, nrow = 1, ncol = length(tempM2))
      tempM2 <- t(tempM2)


      temp[k, 1] <- tempM1 %*% tempM2
    }
    withinSS[i, j] <- colSums(temp)
  }
}
withinSS

# Total sum of squares

totalSS <- matrix(-1, nrow = ncol(grandMean), ncol = ncol(grandMean))

for(i in seq_along(1:nrow(withinSS))) {
  for(j in seq_along(1:ncol(withinSS))) {
    temp <- matrix(-1, nrow = nrow(x0), ncol = 1)
    for(k in seq_along(1:nrow(temp))) {
      tempM1 <- x0[k, ] - grandMean
      tempM2 <- x0[k, ] - grandMean

      tempM1 <- matrix(tempM1, nrow = 1, ncol = length(tempM1))
      tempM2 <- matrix(tempM2, nrow = 1, ncol = length(tempM2))
      tempM2 <- t(tempM2)


      temp[k, 1] <- tempM1 %*% tempM2
    }
    totalSS[i, j] <- colSums(temp)
  }
}
totalSS

totalSS - (withinSS + betweenSS)

