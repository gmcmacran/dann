# Example 16.1 from experimental design book
# SSTR in book is B in dann
# SSE in book is W in dann
# SSTO in book is Sigma

rows <- 19
cols <- 1

x0 <- matrix(0, nrow = rows, ncol = cols)

x0[,1] <- c(11, 17, 16, 14, 15, 12, 10, 15, 19, 11, 23, 20, 18, 17, 27, 33, 22, 26, 28)

y0 <- matrix(c(rep(1, 5), rep(2, 5), rep(3, 4), rep(4, 5)), nrow = rows, ncol = 1)


# Group wise means
grandMean <- matrix(colMeans(x0), nrow = 1, ncol = ncol(x0))
groupMeans <- matrix(0, nrow = length(unique(y0)), ncol = ncol(x0))

for(i in seq_along(1:length(unique(y0))) ){
  groupMeans[i, ] <- mean(x0[which(y0 == unique(y0)[i, ]), ], )
}
groupMeans
grandMean

pis <- matrix(c(5, 5, 4, 5), nrow = length(unique(y0)), ncol = 1)


betweenSS <- matrix(-1, nrow = ncol(grandMean), ncol = ncol(grandMean))
temp <- matrix(-1, nrow = length(unique(y0)), ncol =  ncol(grandMean))
for(i in seq_along(1:nrow(temp))){
  temp[i, ] <- pis[i, 1]*(groupMeans[i, ] - grandMean) * (groupMeans[i, ] - grandMean)
}
temp

betweenSS[1, 1] <- colSums(temp)
betweenSS
rm(temp)

# Weighted within sum of squares
withinSS <- matrix(-1, nrow = ncol(grandMean), ncol = ncol(grandMean))

lookUp <- cbind(matrix(-1, nrow = nrow(x0), ncol = 1), x0)
for(i in seq_along(1:nrow(lookUp))){
  for(j in seq_along(1:nrow(unique(y0)))){
    lookUp[i, 1] <- ifelse(y0[i,] == unique(y0)[j], groupMeans[j, ],lookUp[i, 1])
  }
}

temp <- matrix(-1, nrow = nrow(x0), ncol =  ncol(groupMeans))
for(i in seq_along(1:nrow(temp))){
  for(j in seq_along(1:ncol(temp)))
    temp[i, ] <- (lookUp[i, 2] - lookUp[i, 1]) * (lookUp[i, 2] - lookUp[i, 1])
}
temp

withinSS <- colSums(temp)
withinSS
rm(temp)

# Total sum of squares
lookUp <- cbind(matrix(rep(grandMean, nrow(x0)), nrow = nrow(x0), ncol = 1), x0)

temp <- matrix(-1, nrow = nrow(x0), ncol =  ncol(groupMeans))
for(i in seq_along(1:nrow(temp))){
  for(j in seq_along(1:ncol(temp)))
    temp[i, ] <- (lookUp[i, 2] - lookUp[i, 1]) * (lookUp[i, 2] - lookUp[i, 1])
}
temp

totalSS <- colSums(temp)
totalSS
rm(temp)

withinSS + betweenSS == totalSS

