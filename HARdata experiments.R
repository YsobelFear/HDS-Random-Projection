install.packages("Matrix")
install.packages("irlba")
install.packages("mclust")
library(Matrix)
library(irlba)
library(mclust)
#Reyes-Ortiz, J., Anguita, D., Ghio, A., Oneto, L., & Parra, X. (2013). Human Activity Recognition Using Smartphones [Dataset]. UCI Machine Learning Repository. https://doi.org/10.24432/C54S4K.
pdf("C:/Users/yushi/OneDrive/Documents/DURHAM MATHS WORK/yr 4/HDS/HDS project/clustering_RP_vs_PCA_200.pdf",
    width = 6.0,
    height = 4.5,
    paper = "special")

# Select X_train.txt from the extracted train folder
X_train <- as.matrix(read.table(file.choose()))

# Select y_train.txt
y_train <- scan(file.choose())

# Select X_test.txt from the test folder
X_test <- as.matrix(read.table(file.choose()))

# Select y_test.txt
y_test <- scan(file.choose())

set.seed(50)
# Combine datasets
X <- rbind(X_train, X_test)
true_labels <- c(y_train, y_test)

# Check dimensions
dim(X)
table(true_labels)


# Calculate PCA once for maximum k = 200

pca <- prcomp_irlba(X, n = 200,
                    center = TRUE, scale. = FALSE) #only need to calc pca princ. components once

# Use the same 200 observations for every k
idx <- sample(1:nrow(X), 200)

##########################################################################
compare_methods <- function(k) {
  
  ###############################################
  # Gaussian Random Projection
  
  p <- ncol(X)
  
  R <- matrix(
    rnorm(p * k, mean = 0, sd = 1 / sqrt(k)),
    nrow = p,
    ncol = k
  )
  
  Z_rp <- X %*% R
  
  ###############################################
  # PCA - reuse previously calculated components
  
  Z_pca <- pca$x[, 1:k, drop = FALSE]
  
  ###############################################
  # Compare pairwise distance preservation
  
  # Calculate original distances
  d_original <- as.vector(dist(as.matrix(X[idx, ])))
  
  # Calculate distances after RP
  d_rp <- as.vector(dist(as.matrix(Z_rp[idx, ])))
  
  # Calculate distances after PCA
  d_pca <- as.vector(dist(Z_pca[idx, ]))
  
  # Exclude pairs with zero original distance
  valid <- d_original > 0
  
  # Calculate mean relative distance distortion
  rp_error <- mean(abs(d_rp[valid] - d_original[valid]) /
                     d_original[valid])
  
  pca_error <- mean(abs(d_pca[valid] - d_original[valid]) /
                      d_original[valid])
  
  ###############################################
  # Plot pairwise distance preservation
  
  par(mfrow = c(1, 2))
  
  # RP
  plot(d_original[valid], d_rp[valid],
       pch = 16, cex = 0.3,
       col = rgb(0, 0, 1, 0.2),
       xlab = "Original distance",
       ylab = "Projected distance",
       main = paste("RP, k =", k))
  
  abline(0, 1, col = "red", lwd = 2)
  
  # PCA
  plot(d_original[valid], d_pca[valid],
       pch = 16, cex = 0.3,
       col = rgb(0, 0, 1, 0.2),
       xlab = "Original distance",
       ylab = "Projected distance",
       main = paste("PCA, k =", k))
  
  abline(0, 1, col = "red", lwd = 2)
  
  par(mfrow = c(1, 1))
  
  ###############################################
  # Return results
  
  return(list(
    k = k,
    Z_rp = Z_rp,
    Z_pca = Z_pca,
    rp_error = rp_error,
    pca_error = pca_error
  ))
  
}

##########################################################################
#apply to many k
k_values <- c(2, 5, 10, 20, 30, 40, 50, 75, 100, 125, 150, 175, 200)

results <- lapply(k_values, compare_methods)

# Plot mean distortion against k
rp_errors <- sapply(results, function(x) x$rp_error)
pca_errors <- sapply(results, function(x) x$pca_error)

plot(k_values, rp_errors,
     type = "o", pch = 16,
     col = "blue",
     ylim = range(c(rp_errors, pca_errors)),
     xlab = "Number of dimensions (k)",
     ylab = "Mean relative distance distortion",
     main = "Distance distortion vs dimensionality")

lines(k_values, pca_errors,
      type = "o", pch = 16, col = "red")

legend("topright",
       legend = c("RP", "PCA"),
       col = c("blue", "red"),
       lty = 1, pch = 16)




###################################################################################################
#implementing k-means clustering

# K-means clustering on original dataset

system.time({
  km_original <- kmeans(
    as.matrix(X),
    centers = 6,
    nstart = 10,
    iter.max = 100,
    algorithm = "Lloyd"
  )
})

km_original

#################################################################################
# K-means on RP dataset

# Store K-means results for each k
km_results <- vector("list", length(k_values))
names(km_results) <- paste0("k_", k_values)

Z_results <- vector("list", length(k_values))
names(Z_results) <- paste0("k_", k_values)

for (k in k_values) {
  # Gaussian Random Projection
  # Reuse previously calculated Random Projection
  Z_rp <- results[[which(k_values == k)]]$Z_rp
  
  Z_results[[paste0("k_", k)]] <- Z_rp
  
  # K-means clustering
  km_rp <- kmeans(
    as.matrix(Z_rp),
    centers = 6,
    nstart = 10,
    iter.max = 100,
    algorithm = "Lloyd"
  )
  
  # Store results
  km_results[[paste0("k_", k)]] <- km_rp
  
}

#km_results[["k_50"]]$cluster       # Cluster assignments
#km_results[["k_50"]]$centers       # Cluster centres
#km_results[["k_50"]]$tot.withinss  # Within-cluster sum of squares

# Projected data for k = 2
Z_2 <- as.matrix(Z_results[["k_2"]])

# K-means cluster assignments
clusters_2 <- km_results[["k_2"]]$cluster

# Plot
plot(
  Z_2[, 1], Z_2[, 2],
  col = clusters_2,
  pch = 19,
  cex = 0.5,
  xlab = "Random Projection Dimension 1",
  ylab = "Random Projection Dimension 2",
  main = "K-means Clustering after RP (k = 2)"
)

legend(
  "topright",
  legend = paste("Cluster", 1:6),
  col = 1:6,
  pch = 19
)


table(
  Actual = true_labels,
  Cluster = km_results[["k_50"]]$cluster
)
#################################################################################
#k-means on PCA

# K-means on existing PCA projections
k_values_pca <- k_values
km_pca_results <- vector("list", length(k_values_pca))
names(km_pca_results) <- paste0("k_", k_values_pca)

for (k in k_values_pca) {
  
  Z_pca <- results[[which(k_values == k)]]$Z_pca
  
  km_pca <- kmeans(
    as.matrix(Z_pca),
    centers = 6,
    nstart = 10,
    iter.max = 100,
    algorithm = "Lloyd"
  )
  
  km_pca_results[[paste0("k_", k)]] <- km_pca
}

############################################################################ #
#calculate ARIs #####
#original dataset ARIs
ARI_original <- adjustedRandIndex(
  true_labels,
  km_original$cluster
)

print(ARI_original)

#RP ARIs
ARI_rp <- sapply(km_results, function(km) {
  adjustedRandIndex(true_labels, km$cluster)
})

print(ARI_rp)

#pca ARIs
ARI_pca <- sapply(km_pca_results, function(km) {
  adjustedRandIndex(true_labels, km$cluster)
})

print(ARI_pca)

#############################################################################
#plotting results kmeans clustering original, rp, pca
plot(
  k_values_pca,
  ARI_rp[paste0("k_", k_values_pca)],
  type = "o", pch = 16, col = "blue",
  ylim = range(c(ARI_rp, ARI_pca, ARI_original)),
  xlab = "Projected dimension (k)",
  ylab = "Adjusted Rand Index",
  main = "Clustering performance: RP vs PCA"
)

lines(k_values_pca, ARI_pca,
      type = "o", pch = 16, col = "darkgreen")

abline(h = ARI_original, col = "red", lty = 2)

legend("bottomright",
       legend = c("RP", "PCA", "Original data"),
       col = c("blue", "darkgreen", "red"),
       lty = c(1, 1, 2),
       pch = c(16, 16, NA))

dev.off()

