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
