#Load the dataset into the code
df <- read.csv("PremDataSet.csv", header = TRUE, stringsAsFactors = FALSE)
#Structure and missing values check
str(df)  
summary(df)  
# Check for missing values
anyNA(df)    
# Convert all columns except "Teams" to numeric
df[, -1] <- lapply(df[, -1], as.numeric)  
#Convert team names to factors as this will be useful for classification and grouping
df$Teams <- as.factor(df$Teams) 
#Renaming the columns for readability
colnames(df) <- c("Team", "xG_21_22", "Shots_21_22", "xG_22_23", "Shots_22_23", 
                  "xG_23_24", "Shots_23_24", "Goals_21_22", "Goals_22_23", "Goals_23_24")
#Scaling the data for clustering
df_scaled <- scale(df[, c("xG_21_22", "Shots_21_22", "xG_22_23", "Shots_22_23",
                          "xG_23_24", "Shots_23_24", "Goals_21_22", "Goals_22_23", "Goals_23_24")])
#Summary statistic
summary(df)
#Correlation matrix to find relationship betweeen xG,Shots and Goals
library(corrplot)
corrplot(cor(df[, c("xG_21_22", "Shots_21_22", "xG_22_23", "Shots_22_23",
                    "xG_23_24", "Shots_23_24", "Goals_21_22", "Goals_22_23", "Goals_23_24")]), 
         method = "color", type = "upper")
#Visualizing xG vs Goals with scatter plots for all 3 seasons
library(ggplot2)
ggplot(df, aes(x = xG_21_22, y = Goals_21_22, label = Team)) +
  geom_point(aes(color = Team), size = 3) +
  geom_text(vjust = -0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  
  theme_minimal() +
  labs(title = "xG vs Goals (2021/22 Season)", x = "Expected Goals (xG)", y = "Actual Goals")

ggplot(df, aes(x = xG_22_23, y = Goals_22_23, label = Team)) +
  geom_point(aes(color = Team), size = 3) +
  geom_text(vjust = -0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  
  theme_minimal() +
  labs(title = "xG vs Goals (2022/23 Season)", x = "Expected Goals (xG)", y = "Actual Goals")

ggplot(df, aes(x = xG_23_24, y = Goals_23_24, label = Team)) +
  geom_point(aes(color = Team), size = 3) +
  geom_text(vjust = -0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  
  theme_minimal() +
  labs(title = "xG vs Goals (2023/24 Season)", x = "Expected Goals (xG)", y = "Actual Goals")
#Clustering/K Clustering - Finding Patterns in Team Performance
df_scaled21_22 <- scale(df[, c("xG_21_22", "Shots_21_22", "Goals_21_22")])
df_scaled22_23 <- scale(df[, c("xG_22_23", "Shots_22_23", "Goals_22_23")])
df_scaled23_24 <- scale(df[, c("xG_23_24", "Shots_23_24", "Goals_23_24")])
#Find Optimal Clusters- Elbow Method, This helps determine the optimal number of clusters. 
library(factoextra)
fviz_nbclust(df_scaled, kmeans, method = "wss")  # Finds best K value
#based on the elbow graph k=4 looks a good option 
set.seed(123)
kmeans_result <- kmeans(df_scaled, centers = 4)  # Adjusted k value to 4 based on Elbow Method
df$Cluster <- as.factor(kmeans_result$cluster)
#Visualizing the clusters
ggplot(df, aes(x = xG_21_22, y = Goals_21_22, color = Cluster, label = Team)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5) +
  theme_minimal() +
  labs(title = "Clustering of Premier League Teams (21/22)", x = "xG", y = "Goals")
ggplot(df, aes(x = xG_22_23, y = Goals_22_23, color = Cluster, label = Team)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5) +
  theme_minimal() +
  labs(title = "Clustering of Premier League Teams (22/23)", x = "xG", y = "Goals")
ggplot(df, aes(x = xG_23_24, y = Goals_23_24, color = Cluster, label = Team)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5) +
  theme_minimal() +
  labs(title = "Clustering of Premier League Teams (23/24)", x = "xG", y = "Goals")
#Classification
#Random Forest Model
#Creating a new target variable
df$Efficiency <- ifelse(df$Goals_23_24 / df$xG_23_24 > 1, "Efficient", "Inefficient")
df$Efficiency <- as.factor(df$Efficiency)
library(rpart)
tree_model23_24 <- rpart(Efficiency ~ xG_23_24 + Shots_23_24 + Goals_23_24, data = df, method = "class")
tree_model22_23 <- rpart(Efficiency ~ xG_22_23 + Shots_22_23 + Goals_22_23, data = df, method = "class")
tree_model21_22 <- rpart(Efficiency ~ xG_21_22 + Shots_21_22 + Goals_21_22, data = df, method = "class")
library(rpart.plot)
rpart.plot(tree_model21_22)
rpart.plot(tree_model22_23)
rpart.plot(tree_model23_24)
install.packages("randomForest")
library(randomForest)
rf_model23_24 <- randomForest(Efficiency ~ xG_23_24 + Shots_23_24 + Goals_23_24, data = df, ntree = 500)
print(rf_model23_24)
rf_model22_23 <- randomForest(Efficiency ~ xG_22_23 + Shots_22_23 + Goals_22_23, data = df, ntree = 500)
print(rf_model22_23)
rf_model21_22 <- randomForest(Efficiency ~ xG_21_22 + Shots_21_22 + Goals_21_22, data = df, ntree = 500)
print(rf_model21_22)



