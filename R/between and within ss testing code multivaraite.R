# Example 16.1 from experimental design book page 686 and 692
# SSTR in book is B in dann
# SSE in book is W in dann
# SSTO in book is Sigma

compiler::enableJIT(3)
rows <- 100
cols <- 50

x0 <- matrix(0, nrow = rows, ncol = cols)

# x0[,1] <- c(11, 17, 16, 14, 15, 12, 10, 15, 19, 11, 23, 20, 18, 17, 27, 33, 22, 26, 28)
# #x0[,2] <- c(11, 17, 16, 14, 15, 12, 10, 15, 19, 11, 23, 20, 18, 17, 27, 33, 22, 26, 28)
# #x0[,2] <- 1:rows
#
# y0 <- matrix(c(rep(1, 5), rep(2, 5), rep(3, 4), rep(4, 5)), nrow = rows, ncol = 1)

for(j in seq_along(1:ncol(x0))){
  x0[,j] <- rnorm(rows, 0, 10)
}

y0 <- matrix(c(rep(0, floor(rows/2)), rep(1, ceiling(rows/2))), nrow = rows, ncol = 1)
mean(y0)

# Group wise means
grandMean <- matrix(colMeans(x0), nrow = 1, ncol = ncol(x0))
groupMeans <- matrix(0, nrow = length(unique(y0)), ncol = ncol(x0))

for(i in seq_along(1:nrow(groupMeans)) ){
  for(j in seq_along(1:ncol(groupMeans))){
    groupMeans[i, j] <- mean(x0[which(y0 == unique(y0)[i, 1]), j] )
  }
}
#groupMeans
#grandMean
head(colMeans(grandMean))

# Weighted between sum of squares
betweenSS <- matrix(-1, nrow = ncol(grandMean), ncol = ncol(grandMean))
#pis <- matrix(c(5, 5, 4, 5), nrow = length(unique(y0)), ncol = 1)
pis <- matrix(c(floor(rows/2), ceiling(rows/2)), nrow = length(unique(y0)), ncol = 1)

dot <- function(a, b){
  return(sum(a*b))
}

for(i in seq_along(1:nrow(betweenSS))) {
  for(j in seq_along(1:ncol(betweenSS))) {
    temp <- matrix(-1, nrow = nrow(groupMeans), ncol = 1)
    for(k in seq_along(1:nrow(temp))) {
      tempV1 <- groupMeans[k, i] - grandMean[1,i]
      tempV2 <- groupMeans[k, j] - grandMean[1,j]

      temp[k, 1] <- pis[k]*dot(tempV1, tempV2)
    }
    betweenSS[i, j] <- colSums(temp)
  }
}
#betweenSS
isSymmetric(betweenSS)
all(diag(betweenSS)>0)

# Weighted within sum of squares
withinSS <- matrix(-1, nrow = ncol(grandMean), ncol = ncol(grandMean))

for(i in seq_along(1:nrow(withinSS))) {
  for(j in seq_along(1:ncol(withinSS))) {
    temp <- matrix(-1, nrow = nrow(x0), ncol = 1)
    for(k in seq_along(1:nrow(temp))) {

      currentGroupMean1 <- groupMeans[which(y0[k,] == unique(y0)), i]
      currentGroupMean2 <- groupMeans[which(y0[k,] == unique(y0)), j]

      tempV1 <- x0[k, i] - currentGroupMean1
      tempV2 <- x0[k, j] - currentGroupMean2

      temp[k, 1] <- dot(tempV1, tempV2)
    }
    withinSS[i, j] <- colSums(temp)
  }
}
#withinSS
isSymmetric(withinSS)
all(diag(withinSS)>0)

# Total sum of squares

totalSS <- matrix(-1, nrow = ncol(grandMean), ncol = ncol(grandMean))

for(i in seq_along(1:nrow(withinSS))) {
  for(j in seq_along(1:ncol(withinSS))) {
    temp <- matrix(-1, nrow = nrow(x0), ncol = 1)
    for(k in seq_along(1:nrow(temp))) {
      tempV1 <- x0[k, i] - grandMean[1,i]
      tempV2 <- x0[k, j] - grandMean[1,j]

      temp[k, 1] <- dot(tempV1, tempV2)
    }
    totalSS[i, j] <- colSums(temp)
  }
}
#totalSS
isSymmetric(totalSS)
all(diag(totalSS)>0)

max(totalSS - (withinSS + betweenSS))

