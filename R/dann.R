####################################################################################
# DANN
#
# A R port of See https://github.com/christopherjenness/ML-lib
####################################################################################


###################################
# Create some data to work with
###################################
rows <- 19
cols <- 2

xTest <- matrix(0, nrow = rows, ncol = cols)
xTrain <- matrix(0, nrow = rows, ncol = cols)

xTrain[,1] <- c(11, 17, 16, 14, 15, 12, 10, 15, 19, 11, 23, 20, 18, 17, 27, 33, 22, 26, 28)
xTrain[,2] <- c(11, 17, 16, 14, 15, 12, 10, 15, 19, 11, 23, 20, 18, 17, 27, 33, 22, 26, 28)
yTrain <- matrix(c(rep(1, 5), rep(2, 5), rep(3, 4), rep(4, 5)), nrow = rows, ncol = 1)

xTest[,1] <- c(11, 17, 16, 14, 15, 12, 10, 15, 19, 11, 23, 20, 18, 17, 27, 33, 22, 26, 28)
xTest[,2] <- c(11, 17, 16, 14, 15, 12, 10, 15, 19, 11, 23, 20, 18, 17, 27, 33, 22, 26, 28)
yTest <- matrix(c(rep(1, 5), rep(2, 5), rep(3, 4), rep(4, 5)), nrow = rows, ncol = 1)


###################################
# Initalize values
###################################
S <- diag(cols)
epsilon <- 1
neighborhood_size <- 10
k <- 3

predictions <- vector(mode = "numeric", length = nrow(xTest))

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

  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

###################################
# Calculate predictions
###################################
for (i in 1:nrow(xTest)) {

  distances <- vector(mode = "numeric", length = nrow(xTrain))
  for(j in 1:nrow(xTrain)){
    distances[j] <- sum( (xTest[i, ]-xTrain[j,])^2) ^ .5
  }

  nearest_neighbors <- order(distances)[1:neighborhood_size]
  neighborhood_xTrain <- xTrain[nearest_neighbors, 1:ncol(xTrain), drop = FALSE]
  neighborhood_X_mean <- colMeans(neighborhood_xTrain)
  neighborhood_y <- yTrain[nearest_neighbors, 1 , drop = FALSE]
  neighborhood_classes <- unique(neighborhood_y)

  class_frequencies <- vector(mode = "numeric", length = length(neighborhood_classes))
  within_class_cov <- matrix(0, nrow = ncol(xTrain), ncol = ncol(xTrain))
  between_class_cov <- matrix(0, nrow = ncol(xTrain), ncol = ncol(xTrain))

  for (k in 1:length(neighborhood_classes)){
    target_class <- neighborhood_classes[k]
    class_indices = which(neighborhood_y == target_class)
    class_frequencies[target_class] = sum(neighborhood_y == target_class) / neighborhood_size

    class_covariance = var(neighborhood_xTrain[class_indices,1:ncol(neighborhood_xTrain), drop = FALSE])

    #Deal with 1 row edge case
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
  for(k in 1:ncol(W_star)){
    W_star[which(is.na(W_star[,k])),k] <- 0
  }
  B_star <- W_star %*% between_class_cov %*% W_star
  I = diag(ncol(xTrain))

  sigma <- W_star %*% (B_star + epsilon * I) %*% W_star

  distances <- vector(mode = "numeric", length = nrow(xTrain))
  for( k in 1:length(distances))
    distances[k] <- DANN_distance(xTest[i, 1:ncol(xTest), drop = FALSE], xTrain[k, 1:ncol(xTrain), drop = FALSE ], sigma)
  nearest <- order(distances)[1:k]
  predictions[i] = mode(yTrain[nearest])
}

predictions <- matrix(predictions, nrow = length(predictions), ncol = 1)
all(predictions == yTest)
mean(predictions == yTest)
