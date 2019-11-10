####################################################################################
# DANN
#
# Hastie and Tibshirani's Discriminant Adaptive Nearest Neighbors (DANN) 1996
# A R port of See https://github.com/christopherjenness/ML-lib
#
####################################################################################


#Input checking
# No missing
# neighborhood_size <= nrows
# Same number of columns in xTrain and xTest
# all data can be converted to numeric matcies

###################################
# Create some data to work with
###################################
rows <- 20
cols <- 4

xTest <- matrix(0, nrow = rows, ncol = cols)
xTrain <- matrix(0, nrow = rows, ncol = cols)

xTrain[,1] <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
xTrain[,2] <- c(20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
xTrain[,3] <- c(10, 10, 10, 10 ,10, 10, 10, 10, 10 ,10, 10, 10, 10, 10 ,10, 10, 10, 10, 10 ,10)
xTrain[,4] <- c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5)
yTrain <- matrix(c(rep(1, 3), rep(2, 3), rep(3, 3), rep(4, 3), rep(5, 3), rep(6, 3), rep(7, 2)), nrow = rows, ncol = 1)

xTest[,1] <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
xTest[,2] <- c(20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
xTest[,3] <- c(12, 12, 12, 12 ,12, 12, 12, 12, 12 ,12, 12, 12, 12, 12 ,12, 12, 12, 12, 12 ,12)
xTest[,4] <- c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2)
yTest <- matrix(c(rep(1, 3), rep(2, 3), rep(3, 3), rep(4, 3), rep(5, 3), rep(6, 3), rep(7, 2)), nrow = rows, ncol = 1)

colMeans(xTrain)
colMeans(yTrain)

colMeans(xTest)
colMeans(yTest)

# x = np.array([[1.1, 1.5],
#               [1, 2],
#               [3, 3],
#               [5, 2],
#               [1, 4],
#               [9, 6],
#               [8, 8],
#               [8.1, 9],
#               [7.7, 7.1],
#               [6, 12],
#               [10, 6]])
# y = np.array([0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1])


###################################
# Initalize values
###################################
S <- diag(cols)
epsilon <- 1
neighborhood_size <- 5
k <- 3

###################################
# Helper functions
###################################
# Computes the distance between x0 and x1 using the DANN metric
# which is adaptively defined at query locus
# Args:
# x1 (np.array): query point of shape[n_features]
# x2 (np.array): reference point of shape[n_features]
# sigma (np.ndarray): array of shape[n_features, n_features]
DANN_distance <- function(x0, x1, sigma){
  difference <- x0 - x1
  distance <- difference %*% sigma %*% t(difference)
  return(distance)
}

#https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }

  ux <- sort(unique(x))
  return(ux[which.max(tabulate(match(x, ux)))])
}

###################################
# Calculate predictions
###################################
#i <- 3
#i <- 2
predictions <- vector(mode = "numeric", length = nrow(xTest))
for (i in 1:nrow(xTest)) {

  ###########
  # Find neighborhood for x[i,]
  ###########
  distances <- vector(mode = "numeric", length = nrow(xTrain))
  for(j in 1:nrow(xTrain)){
    #distances[j] <- sum( (xTest[i, ]-xTrain[j,])^2) ^ .5
    distances[j] <- sum( (xTrain[j,]-xTest[i, ])^2) ^ .5
  }

  nearest_neighbors <- order(distances)[1:neighborhood_size]
  neighborhood_xTrain <- xTrain[nearest_neighbors, 1:ncol(xTrain), drop = FALSE]
  neighborhood_X_mean <- colMeans(neighborhood_xTrain)
  neighborhood_y <- yTrain[nearest_neighbors, 1 , drop = FALSE]
  neighborhood_classes <- unique(neighborhood_y)

  ###########
  # Between and within matrices
  ###########
  class_frequencies <- vector(mode = "numeric", length = length(neighborhood_classes))
  within_class_cov <- matrix(0, nrow = ncol(xTrain), ncol = ncol(xTrain))
  between_class_cov <- matrix(0, nrow = ncol(xTrain), ncol = ncol(xTrain))

  for (kth in 1:length(neighborhood_classes)){
    target_class <- neighborhood_classes[kth]
    class_indices = which(neighborhood_y == target_class)
    class_frequencies[target_class] = sum(neighborhood_y == target_class) / neighborhood_size

    class_covariance = var(neighborhood_xTrain[class_indices,1:ncol(neighborhood_xTrain), drop = FALSE])
    #Deal with 1 row in class edge case
    if (all(is.na(class_covariance)))
      class_covariance <- matrix(0, nrow = nrow(class_covariance), ncol = ncol(class_covariance))

    within_class_cov = class_covariance * class_frequencies[target_class] + within_class_cov
    class_mean = colMeans(neighborhood_xTrain[class_indices,1:ncol(neighborhood_xTrain), drop = FALSE])
    between_class_cov <- outer(class_mean - neighborhood_X_mean, class_mean - neighborhood_X_mean) *
      class_frequencies[target_class] + between_class_cov
  }

  # W* = W^-.5
  # B* = W*BW*
  W_star <- within_class_cov^.5
  #Deal with NA case
  for(kth in 1:ncol(W_star)){
    W_star[which(is.na(W_star[,kth])),kth] <- 0
  }
  W_star <- MASS::ginv(W_star)
  B_star <- W_star %*% between_class_cov %*% W_star
  I = diag(ncol(xTrain))

  sigma <- W_star %*% (B_star + epsilon * I) %*% W_star

  ###########
  # DANN distance using sigma
  ###########
  distances <- vector(mode = "numeric", length = nrow(xTrain))
  for( kth in 1:length(distances))
    distances[kth] <- DANN_distance(xTest[i, 1:ncol(xTest), drop = FALSE], xTrain[kth, 1:ncol(xTrain), drop = FALSE ], sigma)
  nearest <- order(distances, length(distances):1)[1:k]
  predictions[i] = mode(yTrain[nearest])
}

predictions <- matrix(predictions, nrow = length(predictions), ncol = 1)
all(predictions == yTest)
mean(predictions == yTest)

predictions
