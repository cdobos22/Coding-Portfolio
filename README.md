# Data Science and Analytics

### Education
Finance, B.S.
Business Intelligence and Analytics, B.S.


### Projects
# Dygraphs Research and Analysis
library(dygraphs)
lungDeaths <- cbind(mdeaths, fdeaths)
dygraph(lungDeaths)

dygraph(lungDeaths) %>% dyRangeSelector()


dygraph(lungDeaths) %>%
  dySeries("mdeaths", label = "Male") %>%
  dySeries("fdeaths", label = "Female") %>%
  dyOptions(stackedGraph = T) %>%
  dyRangeSelector(height = 20)

hw <- HoltWinters(ldeaths)
predicted <- predict(hw, n.ahead = 72, prediction.interval = TRUE)

dygraph(predicted, main = "Predicted Lung Deaths (UK)") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))  

  # Different Analysis (logistic/linear models, k clusters)
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
  
