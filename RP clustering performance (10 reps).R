
##########################################################################
#run HARdata "experiments.R first" and keep Global Environment
# Repeated Random Projection experiment
pdf("C:/Users/yushi/OneDrive/Documents/DURHAM MATHS WORK/yr 4/HDS/HDS project/RP_clustering_repeated.pdf",
    width = 6.0,
    height = 4.5,
    paper = "special")

B <- 10  # no. repetitions
set.seed(123)

# store ARIs for each repetition and k
ARI_repeated <- matrix(
  NA,
  nrow = B,
  ncol = length(k_values)
)

colnames(ARI_repeated) <- paste0("k_", k_values)

##########################################################################
# repeat RP and k-means

for (i in seq_along(k_values)) {
  
  k <- k_values[i]
  
  for (b in 1:B) {
    
    # Generate new Gaussian RP matrix
    R <- matrix(
      rnorm(ncol(X) * k, sd = 1 / sqrt(k)),
      nrow = ncol(X),
      ncol = k
    )
    
    # Project dataset
    Z_rp <- X %*% R
    
    # K-means clustering
    km <- kmeans(
      as.matrix(Z_rp),
      centers = 6,
      nstart = 10,
      iter.max = 100,
      algorithm = "Lloyd"
    )
    
    # Calculate ARI
    ARI_repeated[b, i] <- adjustedRandIndex(
      true_labels,
      km$cluster
    )
  }
}

##########################################################################
# Calculate mean and standard deviation

ARI_mean <- colMeans(ARI_repeated)
ARI_sd <- apply(ARI_repeated, 2, sd)

print(ARI_mean)
print(ARI_sd)

##########################################################################
# plot mean RP ARI with standard deviation

plot(
  k_values, ARI_mean,
  type = "o",
  pch = 16,
  col = "blue",
  ylim = range(c(
    ARI_mean - ARI_sd,
    ARI_mean + ARI_sd
  )),
  xlab = "Projected dimension (k)",
  ylab = "Adjusted Rand Index",
  main = "RP clustering performance (10 repetitions)"
)

# error bars: mean +/- 1 standard deviation
arrows(
  k_values, ARI_mean - ARI_sd,
  k_values, ARI_mean + ARI_sd,
  angle = 90,
  code = 3,
  length = 0.04,
  col = "blue"
)

abline(h = ARI_original, col = "red", lty = 2)

legend("bottomright",
       legend = c("Mean RP ARI (+/- 1 SD)", "Original-data ARI"),
       col = c("blue", "red"),
       lty = c(1, 2),
       pch = c(16, NA))

##########################################################################

dev.off()
