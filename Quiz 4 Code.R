bike.sales <- read_csv("C:/Users/cdobo/OneDrive/Documents/DSS 445 RScripts/Datasets/BikeSales_MLR-1.csv")
churn <- read_csv("C:/Users/cdobo/OneDrive/Documents/DSS 445 RScripts/Datasets/Churn_Performance-1.csv")
fit.center <- read_csv("C:/Users/cdobo/OneDrive/Documents/DSS 445 RScripts/Datasets/Fitness_Cluster-1.csv")
stream <- read_csv("C:/Users/cdobo/OneDrive/Documents/DSS 445 RScripts/Datasets/Streaming_Logit-1.csv")



bike.sales <- read_csv("C:/Users/cdobo/OneDrive/Documents/DSS 445 RScripts/Datasets/BikeSales_MLR-1.csv")

model <- lm(DailySales ~ AdSpend + FootTraffic + Weekend, data = bike.sales)
summary(model)


fit.center <- read_csv("C:/Users/cdobo/OneDrive/Documents/DSS 445 RScripts/Datasets/Fitness_Cluster-1.csv")

fitness1 <- na.omit(fit.center)
fitness1 <- scale(fitness1)
head(fitness1)

k3.fit <- kmeans(fitness1, centers = 3, nstar = 25)
k3.fit
str(k3.fit)
fviz_cluster(k3.fit, data = fitness1, ggtheme = theme_minimal())
