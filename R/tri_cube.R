
rows <- 10
cols <- 3

x0 <- matrix(0, nrow = rows, ncol = cols)
x <- matrix(0, nrow = rows, ncol = cols)

x0[,1] <- 1:10
x0[,2] <- 1:10
x0[,3] <- 1:10
y0 <- matrix(c(rep(0, rows/2), rep(1, rows/2)), nrow = rows, ncol = 1)

x[,1] <- 11:20
x[,2] <- 21:30
x[,3] <- 31:40
y <- matrix(c(rep(0, rows/2), rep(1, rows/2)), nrow = rows, ncol = 1)

S <- matrix(0, nrow = cols, ncol = cols)
diag(S) <- 1

h <- 3

# X0 <- x0[1,]
# X <- x[1,]
# VAR <- S

calc_d <- function(X0, X, VAR){
  temp <- VAR^.5 %*% (X0 - X)
  out <- 0
  for(i in seq_along(1:nrow(temp))){
    out <- out + temp[i,1]^2
  }
  out <- out^.5

  return(out)
}

dist <- matrix(-1, nrow = nrow(x0) * nrow(x), ncol = 6)
insertHere <- 0

# Calculate distance
for(i in seq_along(1:nrow(x0)) ){
  for(j in seq_along(1:nrow(x))){
    insertHere <- insertHere + 1
    dist[insertHere, 1] <- i
    dist[insertHere, 2] <- j
    dist[insertHere, 3] <- calc_d(x0[i,], x[j,], S)
  }
}
rm(i, j, insertHere)

#Group wise rank distance
windowStart <- 1 - nrow(x0)
windowEnd <- 0
for(i in seq_along(1:nrow(x0)) ){
  windowStart <- windowStart + nrow(x0)
  windowEnd <- windowEnd + nrow(x0)
  dist[windowStart:windowEnd, 4] <- rank(dist[windowStart:windowEnd, 3])
}
rm(windowStart, windowEnd)

#Group wise tri cube
windowStart <- 1 - nrow(x0)
windowEnd <- 0
for(i in seq_along(1:nrow(x0)) ){
  windowStart <- windowStart + nrow(x0)
  windowEnd <- windowEnd + nrow(x0)

  tempH <- ifelse(dist[windowStart:windowEnd, 4] <= h, dist[windowStart:windowEnd, 3], 0)
  tempH <- max(tempH)
  dist[windowStart:windowEnd, 5] <- ifelse(dist[windowStart:windowEnd, 3] <= tempH,
                                           (1 - (dist[windowStart:windowEnd, 3]/tempH)^3)^3,
                                           0)
}
rm(windowStart, windowEnd)

# Weight matrix
weights <- matrix(-1, nrow = nrow(x0), ncol = 1)

windowStart <- 1 - nrow(x0)
windowEnd <- 0
for(i in seq_along(1:nrow(x0))){
  windowStart <- windowStart + nrow(x0)
  windowEnd <- windowEnd + nrow(x0)

  print(windowStart)

  weights[i, 1] <- sum(dist[windowStart:windowEnd, 5])
}
rm(windowStart, windowEnd)

# Group wise means
unique(y0)

grandMean <- matrix(colMeans(x0), nrow = 1, ncol = ncol(x0))
groupMeans <- matrix(0, nrow = length(unique(y0)), ncol = ncol(x0))


calc_weighted_mean <- function(X0, VAR, WEIGHTS){
  temp <- matrix(0, nrow = nrow(X0), ncol = ncol(X0))
  for( i in seq_along(1:nrow(temp))){
    temp[i, ] <- X0[i, ] %*% VAR
  }

  out <- WEIGHTS * temp / sum(WEIGHTS)
  out <- matrix(out, nrow = 1, ncol = ncol(X0))

  return(out)
}

for(i in seq_along(1:length(unique(y0))) ){
  groupMeans[i, ] <- calc_weighted_mean(x0[which(y0 == unique(y0)[i, ]), ], S, weights[which(y0 == unique(y0)[i, ]), 1])
}

# Weighted between sum of squares
pis <- matrix(-1, nrow = length(unique(y0)), ncol = 1)
for(i in seq_along(1:nrow(pis))){
  pis[i, ] <- sum(weights[which(y0 == unique(y0)[i, ]), 1]) / sum(weights)
}

betweenSS <- matrix(-1, nrow = length(unique(y0)), ncol = ncol(grandMean))
for(i in seq_along(1:nrow(betweenSS))){
  betweenSS[i, ] <- pis[i, ] * (groupMeans[i, ] - grandMean) * (groupMeans[i, ] - grandMean)
}

# Weighted within sum of squares
withinSS <- matrix(-1, nrow = length(unique(y0)), ncol = ncol(grandMean))
for(j in seq_along(1:nrow(withinSS))){
  for(i in seq_along(1:nrow(x0)))
    withinSS[j, ] <- weights[i, ] * (x0[i, ] - groupMeans[j, ]) * (x0[i, ] - groupMeans[j, ]) / sum(weights)
}

S <- withinSS^-.5 %*% t(betweenSS) %*% withinSS^-.5

