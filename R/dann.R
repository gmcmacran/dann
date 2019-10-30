
rows <- 50
cols <- 3

x0 <- matrix(0, nrow = rows, ncol = cols)
x <- matrix(0, nrow = rows, ncol = cols)

x0[,1] <- 1:rows
x0[,2] <- 1:rows
x0[,3] <- 1:rows
y0 <- matrix(c(rep(0, rows/2), rep(1, rows/2)), nrow = rows, ncol = 1)

x[,1] <- 11:60
x[,2] <- 21:70
x[,3] <- 31:80
y <- matrix(c(rep(0, rows/2), rep(1, rows/2)), nrow = rows, ncol = 1)

S <- matrix(0, nrow = cols, ncol = cols)
diag(S) <- 1

h <- min(nrow(rows) / 5, 50)
K <- 5

while(h > K){
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

  #rank distance by point in x0
  windowStart <- 1 - nrow(x)
  windowEnd <- 0
  for(i in seq_along(1:nrow(x0)) ){
    windowStart <- windowStart + nrow(x)
    windowEnd <- windowEnd + nrow(x)
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

    weights[i, 1] <- sum(dist[windowStart:windowEnd, 5])
  }
  rm(windowStart, windowEnd)

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


  #calculaite pis
  pis <- matrix(-1, nrow = length(unique(y0)), ncol = 1)
  for(i in nrow(pis)){
    pis[i,1] <- sum(weights[y0 == unique(y0)[i,1],1])
  }

  dot <- function(a, b){
    return(sum(a*b))
  }

  # Weighted between sum of squares
  betweenSS <- matrix(-1, nrow = ncol(grandMean), ncol = ncol(grandMean))
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
  #all(diag(betweenSS)>0)

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

        temp[k, 1] <- weights[i,1]* dot(tempV1, tempV2)/sum(weights)
      }
      withinSS[i, j] <- colSums(temp)
    }
  }
  #withinSS
  isSymmetric(withinSS)
  all(diag(withinSS)>0)

  # Total sum of squares
  epsilon <- 1
  I <- matrix(0, nrow = ncol(x0), ncol = ncol(x0))
  diag(I) <- 1
  S <- withinSS^(-1/2) %*% (withinSS^(-1/2) %*% betweenSS %*% withinSS^(-1/2) + epsilon*I) %*% withinSS^(-1/2)

  h <- pmax(floor(h *.90), K)
  print(h)
}

#Classes of closest points
dist <- dist[dist[,5]>0,]

classes <- matrix(-1, nrow = nrow(x0), ncol = K)
for(i in seq_along(1:nrow(classes))){
  for(j in seq_along(1:ncol(classes))){
    classes[i,j] <- dist[j]
  }
}




