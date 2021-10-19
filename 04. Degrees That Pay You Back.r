
# Load relevant packages
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(cluster)
library(factoextra)

# Read in the dataset
degrees <- read_csv("datasets/degrees-that-pay-back.csv", col_names=c("College.Major", 
                                                                      "Starting.Median.Salary", 
                                                                      "Mid.Career.Median.Salary", 
                                                                      "Career.Percent.Growth", 
                                                                      "Percentile.10", 
                                                                      "Percentile.25", 
                                                                      "Percentile.75", 
                                                                      "Percentile.90"), skip=1)

# Display the first few rows and a summary of the data frame
degrees[1,]
summary(degrees)

# These packages need to be loaded in the first @tests cell. 
library(testthat) 
library(IRkernel.testthat)

sol_degrees <- read_csv('datasets/degrees-that-pay-back.csv', col_names=c('College.Major',
                    'Starting.Median.Salary','Mid.Career.Median.Salary','Career.Percent.Growth',
                    'Percentile.10','Percentile.25','Percentile.75','Percentile.90'), skip=1)


# Clean up the data
degrees_clean <- degrees %>% 
    mutate_at(vars(-College.Major), function(x) as.numeric(gsub("[\\$,]","",x))) %>%
    mutate(Career.Percent.Growth = Career.Percent.Growth/100)
        
str(degrees_clean)

sol_degrees_clean <- sol_degrees %>% 
    mutate_at(vars(Starting.Median.Salary:Percentile.90), 
              function(x) as.numeric(gsub("[\\$,]","",x))) %>%
    mutate(Career.Percent.Growth = Career.Percent.Growth/100)
    

# Select and scale the relevant features and store as k_means_data
k_means_data <- degrees_clean %>%
    select(Starting.Median.Salary, Mid.Career.Median.Salary, 
           Percentile.10, Percentile.90) %>%
    scale()

# Run the fviz_nbclust function with our selected data and method "wss"
elbow_method <- fviz_nbclust(k_means_data, FUNcluster = kmeans, method = "wss")

# View the plot
elbow_method

sol_k_means_data <- sol_degrees_clean %>%
    select(Starting.Median.Salary, Mid.Career.Median.Salary, 
           Percentile.10, Percentile.90) %>%
    scale()

sol_elbow <- fviz_nbclust(sol_k_means_data, kmeans, method = "wss")


# Run the fviz_nbclust function with the method "silhouette" 
silhouette_method <- fviz_nbclust(k_means_data, 
                                  FUNcluster = kmeans, method = "silhouette")

# View the plot
silhouette_method

sol_silhouette <- fviz_nbclust(sol_k_means_data, kmeans, method = "silhouette")


# Use the clusGap function to apply the Gap Statistic Method
gap_stat <- clusGap(k_means_data, FUN = kmeans, nstart = 25, K.max = 10, B = 50)

# Use the fviz_gap_stat function to vizualize the results
gap_stat_method <- fviz_gap_stat(gap_stat)

# View the plot
gap_stat_method

sol_gap_stat <- clusGap(sol_k_means_data, FUN = kmeans, nstart = 25, 
                    K.max = 10, B = 50)

sol_gap_stat_method <- fviz_gap_stat(sol_gap_stat)


# Set a random seed
set.seed(1988)

# Set k equal to the optimal number of clusters
num_clusters <- 3

# Run the k-means algorithm 
k_means <- kmeans(k_means_data, centers = num_clusters, 
                  iter.max = 15, nstart = 25)

# Label the clusters of degrees_clean
degrees_labeled <- degrees_clean %>%
    mutate(clusters = k_means$cluster)

str(degrees_labeled)

set.seed(111)
sol_k_means <- kmeans(sol_k_means_data, 3, iter.max = 15, nstart = 25)

# Add back the cluster labels to degrees
sol_degrees_labeled <- sol_degrees_clean %>%
    mutate(clusters = sol_k_means$cluster)

# Graph the clusters by Starting and Mid Career Median Salaries
career_growth <- ggplot(degrees_labeled, aes(Starting.Median.Salary, 
                                             Mid.Career.Median.Salary,
                                            color = factor(clusters))) +
geom_point() +
xlab("Starting.Median.Salary") +
ylab("Mid.Career.Median.Salary") +
ggtitle("Starting Median Salary vs. Mid Career Median Salary") +
scale_x_continuous(labels = scales::dollar) +
scale_y_continuous(labels = scales::dollar)

# View the plot
career_growth

sol_career_growth <- ggplot(sol_degrees_labeled, aes(x=Starting.Median.Salary,y=Mid.Career.Median.Salary,
    color=factor(clusters))) +
    geom_point(alpha=4/5,size=6) +
    scale_x_continuous(labels = scales::dollar) +
    scale_y_continuous(labels = scales::dollar) +
    xlab('Starting Median Salary') +
    ylab('Mid Career Median Salary') +
    scale_color_manual(name="Clusters",values=c("#EC2C73","#29AEC7", 
                    "#FFDD30")) +
    ggtitle('Clusters by Starting vs. Mid Career Median Salaries')


# Use the gather function to reshape degrees and 
# use mutate() to reorder the new percentile column
degrees_perc <- degrees_labeled %>%
    select(College.Major,Percentile.10, Percentile.25, Mid.Career.Median.Salary, 
           Percentile.75, Percentile.90, clusters) %>%
    gather(key = "percentile", value = "salary", -c(College.Major, clusters)) %>%
    mutate(percentile = factor(percentile, levels = c("Percentile.10", "Percentile.25", "Mid.Career.Median.Salary", 
           "Percentile.75", "Percentile.90")))

str(degrees_perc)

sol_degrees_perc <- sol_degrees_labeled %>%
    select(College.Major, Percentile.10, Percentile.25, 
           Mid.Career.Median.Salary, Percentile.75, 
           Percentile.90, clusters) %>%
    gather(key=percentile, value=salary, -c(College.Major, clusters)) %>%
    mutate(percentile=factor(percentile,levels=c('Percentile.10','Percentile.25',
            'Mid.Career.Median.Salary','Percentile.75','Percentile.90')))

# Graph the majors of Cluster 1 by percentile
cluster_1 <- degrees_perc %>%
               filter(clusters == 1) %>%
               ggplot(aes(x = percentile, y = salary, 
                          group = College.Major, color = College.Major)) +
               geom_point() +
               geom_line() +
               ggtitle("Cluster 1: The Liberal Arts") +
               theme(axis.text.x = element_text(size = 7, angle = 25))

# View the plot
cluster_1

sol_cluster_1 <- ggplot(sol_degrees_perc[sol_degrees_perc$clusters==1,], aes(x=percentile,y=salary, 
                    group=College.Major, color=College.Major, order=salary)) +
                    geom_point() +
                    geom_line() +
                    ggtitle('Cluster 1:  The Liberal Arts') +
                    theme(axis.text.x = element_text(size=7, angle=25)) 

# Modify the previous plot to display Cluster 2
cluster_2 <- degrees_perc %>%
               filter(clusters == 2) %>%
               ggplot(aes(x = percentile, y = salary, 
                          group = College.Major, color = College.Major)) +
               geom_point() +
               geom_line() +
               ggtitle("Cluster 2: The Goldilocks") +
               theme(axis.text.x = element_text(size = 7, angle = 25))


# View the plot
cluster_2

sol_cluster_2 <- ggplot(sol_degrees_perc[sol_degrees_perc$clusters==2,], aes(x=percentile,y=salary, 
                    group=College.Major, color=College.Major, order=salary)) +
                    geom_point() +
                    geom_line() +
                    ggtitle('Cluster 2:  The Goldilocks') +
                    theme(axis.text.x = element_text(size=7, angle=25)) 


# Modify the previous plot to display Cluster 3
cluster_3 <- degrees_perc %>%
               filter(clusters == 3) %>%
               ggplot(aes(x = percentile, y = salary, 
                          group = College.Major, color = College.Major)) +
               geom_point() +
               geom_line() +
               ggtitle("Cluster 3: The Over Achievers") +
               theme(axis.text.x = element_text(size = 7, angle = 25))


# View the plot
cluster_3

sol_cluster_3 <- ggplot(sol_degrees_perc[sol_degrees_perc$clusters==3,], aes(x=percentile,y=salary, 
                    group=College.Major, color=College.Major, order=salary)) +
                    geom_point() +
                    geom_line() +
                    ggtitle('Cluster 2:  The Over Achievers') +
                    theme(axis.text.x = element_text(size=7, angle=25)) 

# Sort degrees by Career.Percent.Growth
degrees_labeled %>%
  select(College.Major, Career.Percent.Growth) %>%
  arrange(desc(Career.Percent.Growth)) %>%
  head(2)

# Identify the two majors tied for highest career growth potential
highest_career_growth <- c('Math','Philosophy')
