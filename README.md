# Data Science and Analytics

# Education
Finance, B.S.
Business Intelligence and Analytics, B.S.


# Projects
### Dygraphs Research and Analysis
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

### Different Analysis (logistic/linear models, k clusters)
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

### Logistic Regression Model
house.data <- read_excel("C:/Users/cdobo/OneDrive/Documents/DSS 445 RScripts/Datasets/HW7 Data.xlsx")
head(house.data)
summary(house.data)

house.data$urban.dummy <- ifelse(house.data$location == "Urban", 1, 0)
house.data$suburban.dummy <- ifelse(house.data$location == "Suburban", 1, 0)


house.model1 <- lm(price ~ square_feet + bedrooms + bathrooms + lot_size + year_built + garage + urban.dummy + suburban.dummy, data = house.data)
summary(house.model1)

house.model2 <- lm(price ~ square_feet + bedrooms + bathrooms + lot_size + garage + urban.dummy + suburban.dummy, data = house.data)
summary(house.model2)

house.model3 <- lm(price ~ square_feet + bedrooms + bathrooms + lot_size + urban.dummy + suburban.dummy, data = house.data)
summary(house.model3)

house.model4 <- lm(price ~ square_feet + bedrooms + bathrooms + urban.dummy + suburban.dummy, data = house.data)
summary(house.model4)


house.model5 <- lm(price ~ square_feet + bedrooms + bathrooms + urban.dummy, data = house.data)
summary(house.model5)

house.model6 <- lm(price ~ square_feet + bedrooms + urban.dummy, data = house.data)
summary(house.model6)

summary(house.data)

plot(house.data$square_feet, house.data$price,
     main = "Price vs House Square Footage",
     xlab = "House Square Footage",
     ylab = "Price of House",
     pch = 19)
