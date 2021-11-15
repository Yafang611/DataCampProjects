
# Load the tidyverse
library(tidyverse)

# Read in the dataset
argentina <- read_csv("datasets/argentina.csv")

# Inspect the first rows of the dataset
nrow(argentina)
head(argentina)


# Add gdp_per_capita column to argentina
argentina <- argentina %>% 
  mutate(gdp_per_cap = gdp / pop) 

# Find the four richest provinces
( rich_provinces  <- argentina %>% 
 arrange(desc(gdp_per_cap)) %>%
 select(province, gdp_per_cap) %>%
 top_n(4))
 
# Find the provinces with populations over 1 million
( bigger_pops <- argentina %>% 
 arrange(desc(pop)) %>%
 select(province, pop) %>%
 filter(pop >= 1000000))


# Select numeric columns and cast to matrix
argentina_matrix  <- argentina  %>% 
  select_if(is.numeric) %>%  
  as.matrix()

# Print the first lines of the result
head(argentina_matrix)


# Load FactoMineR
library(FactoMineR)

# Apply PCA and print results
( argentina_pca  <- PCA(argentina_matrix, scale.unit = TRUE) )

# Load factoextra
library(factoextra)
# Set the size of plots in this notebook
options(repr.plot.width=7, repr.plot.height=5)

# Plot the original variables and the first 2 components and print the plot object.
( pca_var_plot <- fviz_pca_var(argentina_pca) )

( argentina_pca$eig )

# Sum the variance preserved by the first two components. Print the result.
( variance_first_two_pca <- argentina_pca$eig[1, 2] + argentina_pca$eig[2, 2] )



# Visualize Dim2 vs. Dim1
fviz_pca_ind(argentina_pca, title = "Provinces - PCA")

student_plot <- last_plot()
soln_plot <- fviz_pca_ind(soln_argentina_pca, title = "Provinces - PCA") 



# Set seed to 1234 for reproducibility
set.seed(1234)

# Create an intermediate data frame with pca_1 and pca_2
argentina_comps <- tibble(pca_1 = argentina_pca$ind$coord[ ,1],  
                          pca_2 = argentina_pca$ind$coord[ ,2])


# Cluster the observations using the first 2 components and print its contents
( argentina_km <- kmeans(argentina_comps, centers = 4, nstart = 20, iter.max = 50) )

# Convert assigned clusters to factor
clusters_as_factor <- factor(argentina_km$cluster)

# Plot individulas colored by cluster
fviz_pca_ind(argentina_pca, 
             title = "Clustered Provinces - PCA", 
             habillage = clusters_as_factor) 

# Load ggrepel
library(ggrepel)

# Add cluster column to argentina
argentina <- argentina %>%
               mutate(cluster=clusters_as_factor)

# Make a scatterplot of gdp vs. cluster, colored by cluster
ggplot(argentina, aes(cluster, gdp, color = cluster)) +
  geom_point() +
  geom_text_repel(aes(label = province), show.legend = FALSE) +
  labs(x = "Cluster", y = "GDP")


# Make a scatterplot of GDP per capita vs. cluster, colored by cluster
ggplot(argentina, aes(cluster, gdp_per_cap, color = cluster)) +
  geom_point() +
  geom_text_repel(aes(label = province), show.legend = FALSE) +
  labs(x = "Cluster", y = "GDP per capita")



# Make scatterplot of poverty vs. cluster, colored by cluster
ggplot(argentina, aes(cluster, poverty, color = cluster)) +
  geom_point() +
  labs(x = "Cluster", y = "Poverty rate") +
  geom_text_repel(aes(label = province), show.legend = FALSE)

# Assign pilot provinces to the most diverse group
pilot_provinces <- 3

