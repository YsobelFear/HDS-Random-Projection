library(irlba)
##########################################################################
# Computational efficiency experiment
# Original data vs Gaussian RP vs PCA
# K-means uses Lloyd's algorithm throughout

#first run "HARdata experiments.R" and keep global Environment

set.seed(123)

# Number of repetitions
B_time <- 3

# Dimensions to compare
k_values_time <- c(10, 50, 100, 200)

# Number of clusters
n_clusters <- 6

##########################################################################
# Store timing results for each repetition

timing_raw <- data.frame(
  Method = character(),
  k = integer(),
  Repetition = integer(),
  Reduction_time = numeric(),
  Kmeans_time = numeric(),
  Total_time = numeric()
)

##########################################################################
# K-means clustering on original dataset

for (b in 1:B_time) {
  
  ###############################################
  # Measure original-data clustering time
  
  time_original <- system.time({
    
    km_original_time <- kmeans(
      as.matrix(X),
      centers = n_clusters,
      nstart = 10,
      iter.max = 100,
      algorithm = "Lloyd"
    )
    
  })["elapsed"]
  
  ###############################################
  # Store original-data results
  
  timing_raw <- rbind(
    timing_raw,
    data.frame(
      Method = "Original",
      k = ncol(X),
      Repetition = b,
      Reduction_time = 0,
      Kmeans_time = unname(time_original),
      Total_time = unname(time_original)
    )
  )
  
}

##########################################################################
# Random Projection and PCA timing experiments

for (k in k_values_time) {
  
  for (b in 1:B_time) {
    # Gaussian Random Projection
    
    # Time generating R and projecting X
    time_rp <- system.time({
      
      R_time <- matrix(
        rnorm(ncol(X) * k, mean = 0, sd = 1 / sqrt(k)),
        nrow = ncol(X),
        ncol = k
      )
      
      Z_rp_time <- X %*% R_time
      
    })["elapsed"]
    
    # PCA
    
    # Recalculate PCA from scratch for each k
    time_pca <- system.time({
      
      pca_time <- prcomp_irlba(
        X,
        n = k,
        center = TRUE,
        scale. = FALSE
      )
      
      Z_pca_time <- pca_time$x
      
    })["elapsed"]
    
    ###############################################
    # K-means clustering after RP
    
    time_km_rp <- system.time({
      
      km_rp_time <- kmeans(
        as.matrix(Z_rp_time),
        centers = n_clusters,
        nstart = 10,
        iter.max = 100,
        algorithm = "Lloyd"
      )
      
    })["elapsed"]
    
    ###############################################
    # K-means clustering after PCA
    
    time_km_pca <- system.time({
      
      km_pca_time <- kmeans(
        as.matrix(Z_pca_time),
        centers = n_clusters,
        nstart = 10,
        iter.max = 100,
        algorithm = "Lloyd"
      )
      
    })["elapsed"]
    
    ###############################################
    # Store RP results
    
    timing_raw <- rbind(
      timing_raw,
      data.frame(
        Method = "RP",
        k = k,
        Repetition = b,
        Reduction_time = unname(time_rp),
        Kmeans_time = unname(time_km_rp),
        Total_time = unname(time_rp + time_km_rp)
      )
    )
    
    ###############################################
    # Store PCA results
    
    timing_raw <- rbind(
      timing_raw,
      data.frame(
        Method = "PCA",
        k = k,
        Repetition = b,
        Reduction_time = unname(time_pca),
        Kmeans_time = unname(time_km_pca),
        Total_time = unname(time_pca + time_km_pca)
      )
    )
    
    cat("Completed k =", k, "repetition =", b, "\n")
    
  }
  
}

##########################################################################
# Calculate mean computation times across repetitions

timing_summary <- aggregate(
  cbind(Reduction_time, Kmeans_time, Total_time) ~
    Method + k,
  data = timing_raw,
  FUN = mean
)

##########################################################################
# Arrange rows in the required order

# Original dataset first
original_row <- timing_summary[
  timing_summary$Method == "Original",
]

# RP and PCA for each k
reduced_rows <- timing_summary[
  timing_summary$Method != "Original",
]

reduced_rows$Method <- factor(
  reduced_rows$Method,
  levels = c("RP", "PCA")
)

reduced_rows <- reduced_rows[
  order(reduced_rows$k, reduced_rows$Method),
]

# Combine into final table
timing_table <- rbind(
  original_row,
  reduced_rows
)

# Convert method names back to characters
timing_table$Method <- as.character(timing_table$Method)

##########################################################################
# Round times to 3 decimal places

timing_table[, 3:5] <- round(
  timing_table[, 3:5],
  3
)

# Remove row names
rownames(timing_table) <- NULL

##########################################################################
# Print final table

print(timing_table, row.names = FALSE)

##########################################################################
# Save results

# Individual measurements from each repetition
write.csv(
  timing_raw,
  "timing_raw_results.csv",
  row.names = FALSE
)

# Final 9-row, 5-column table for report
write.csv(
  timing_table,
  "timing_report_table.csv",
  row.names = FALSE
)







###################################################################################################
#exporting tables
install.packages("gridExtra")  # Only needed once
library(gridExtra)
library(grid)

output_folder <- "C:/Users/yushi/OneDrive/Documents/DURHAM MATHS WORK/yr 4/HDS/HDS project/"

##########################################################################
# Table 1: Raw timing results (27 measurements)

raw_display <- timing_raw
raw_display[, 4:6] <- round(raw_display[, 4:6], 3)

pdf(paste0(output_folder, "timing_raw.pdf"),
    width = 10, height = 12)

grid.table(raw_display, rows = NULL)

dev.off()

##########################################################################
# Table 2: Mean timing results

summary_display <- timing_summary
summary_display[, 3:5] <- round(summary_display[, 3:5], 3)

pdf(paste0(output_folder, "timing_summary.pdf"),
    width = 10, height = 5)

grid.table(summary_display, rows = NULL)

dev.off()

##########################################################################
# Table 3: Final report table

pdf(paste0(output_folder, "timing_table.pdf"),
    width = 10, height = 5)

grid.table(timing_table, rows = NULL)

dev.off()