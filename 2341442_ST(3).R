
# Load required library
library(ggplot2)

# INPUT DATA


K <- 20            # Total number of clusters
sigma_c <- 12      # Between-cluster standard deviation
B <- 2             # Sampling bias
E <- 3             # Desired margin of error

C_c <- 500         # Cost per cluster
t_c <- 2           # Time per cluster (hours)


# BIAS ADJUSTMENT
sigma_adj <- sqrt(sigma_c^2 + B^2)

# REQUIRED NUMBER OF CLUSTERS

k <- (sigma_adj / E)^2
k <- ceiling(k)


# COST AND TIME CALCULATION
total_cost <- k * C_c
total_time <- k * t_c


# OUTPUT RESULTS
cat("Required number of clusters (k):", k, "\n")
cat("Total survey cost:", total_cost, "\n")
cat("Total survey time (hours):", total_time, "\n")


# DESIGN OF EXPERIMENT (PLOT)
design <- data.frame(
  Cluster = paste0("C", 1:K),
  Status = c(rep("Selected", k), rep("Not Selected", K - k))
)

ggplot(design, aes(x = Cluster, fill = Status)) +
  geom_bar() +
  labs(title = "Cluster Sampling Design – Sampling 2",
       x = "Clusters",
       y = "Selection Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))