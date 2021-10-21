
# Load the data
heart_disease <- read.csv("datasets/heart_disease_patients.csv")

# Print the first ten rows
head(heart_disease, 10)


# Evidence that the data should be scaled?
summary(heart_disease)

# Remove id
heart_disease <- heart_disease[ , !(colnames(heart_disease) %in% c("id"))]

# Scaling data and saving as a data frame
scaled <- scale(heart_disease)

# What do the data look like now?
summary(scaled)

# Set the seed so that results are reproducible
seed_val  <- 10
set.seed(seed_val)

# Select a number of clusters
k <- 5

# Run the k-means algorithm
first_clust = kmeans(scaled, centers = k, nstart = 1)

first_clust

# How many patients are in each cluster?
first_clust$size

# Set the seed
seed_val <- 38
set.seed(seed_val)

# Select a number of clusters and run the k-means algorithm
k2 <- 5
second_clust = kmeans(scaled, centers = k2, nstart = 1)

# How many patients are in each cluster?
second_clust$size

# Add cluster assignments to the data
heart_disease["first_clust"] <- first_clust$cluster
heart_disease["second_clust"] <- second_clust$cluster

# Load ggplot2
library(ggplot2)

# Create and print the plot of age and chol for the first clustering algorithm
plot_one  <- ggplot(heart_disease, aes(x = age, y = chol, color = as.factor(first_clust))) + 
   geom_point()
plot_one 

# Create and print the plot of age and chol for the second clustering algorithm
plot_two <- ggplot(heart_disease, aes(x = age, y = chol, color = as.factor(second_clust))) +
   geom_point()
plot_two

# Execute hierarchical clustering with complete linkage
hier_clust_1 <- hclust(dist(scaled), method = "complete")

# Print the dendrogram
plot(hier_clust_1)

# Get cluster assignments based on number of selected clusters
hc_1_assign <- cutree(hier_clust_1, k = 5)

hc_1_assign

# Execute hierarchical clustering with single linkage
hier_clust_2 <- hclust(dist(scaled), method = "single")

# Print the dendrogram
plot(hier_clust_2)

# Get cluster assignments based on number of selected clusters
hc_2_assign <- cutree(hier_clust_2, k = 5)

hc_2_assign


# Add assignment of chosen hierarchical linkage
heart_disease["hc_clust"] <- hc_1_assign


# Remove the sex, first_clust, and second_clust variables
hd_simple <- heart_disease[, !colnames(heart_disease) %in% c("sex", "first_clust", "second_clust")]

# Get the mean and standard deviation summary statistics
clust_summary <- do.call(data.frame, aggregate(. ~ hc_clust, data = hd_simple, function(x) c(avg = mean(x), sd = sd(x))))
clust_summary

# Plot age and chol
plot_one <- ggplot(hd_simple, aes(x = age, y = chol, color = as.factor(hc_clust))) +
  geom_point()
plot_one

# Plot oldpeak and trestbps
plot_two <- ggplot(hd_simple, aes(x = oldpeak, y = trestbps, color = as.factor(hc_clust))) +
  geom_point()
plot_two

# Add TRUE if the algorithm shows promise, add FALSE if it does not
explore_kmeans <- FALSE
explore_hierarch_complete <- TRUE
explore_hierarch_single <- FALSE