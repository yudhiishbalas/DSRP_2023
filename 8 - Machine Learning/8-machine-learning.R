data <- read.csv("data/Numeric.csv")
library(dplyr)
#View(data)
dataset <- select(data, -Drive, -Make, -Model)
dataframe <- select(data, -Make, -Model)
dataset <- na.omit(dataset)
dataframe <- na.omit(dataframe)
#View(dataset)

pcas <- prcomp(dataset, scale. = T)
summary(pcas)

pcas$rotation^2

var_percs <- as.data.frame(pcas$rotation^2)

pca_vals <- as.data.frame(pcas$x)
pca_vals$Drive <- na.omit(data)$Drive

library(ggplot2)
ggplot(pca_vals, aes(PC1, PC2, color = Drive)) +
  geom_point() +
  labs(x = "PC1",y = "PC2",title = "PCA Plot") +
  theme_minimal()

dataNumeric <- mutate(dataframe, Drive = as.integer(factor(Drive)))
colnames(dataNumeric) <- c("Battery","Acceleration","TopSpeed","Range","Efficiency","Charging","Drive","Seats","Euros","Pounds")
library(reshape2)
dataCors <- dataNumeric |>
  cor() |>
  melt() |>
  as.data.frame()

ggplot(dataCors, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  labs(x = "Variables",y = "Variables",title = "Correlation Plot") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",midpoint = 0)

#View(dataNumeric)
#high correlation
ggplot(dataNumeric, aes(x = Pounds, y = Euros)) +
  geom_point() +
  labs(x = "Price in UK (£)",y = "Price in Germany (€)",title = "Very High Correlation Scatter Plot") +
  theme_minimal()

ggplot(dataNumeric, aes(x = TopSpeed, y = Euros)) +
  geom_point() +
  labs(x = "Top Speed (km/h)",y = "Price in Germany (€)",title = "High Correlation Scatter Plot") +
  theme_minimal()

ggplot(dataNumeric, aes(x = TopSpeed, y = Pounds)) +
  geom_point() +
  labs(x = "Top Speed (km/h)",y = "Price in UK (£)",title = "High Correlation Scatter Plot") +
  theme_minimal()

#low correlation
ggplot(dataNumeric, aes(x = TopSpeed, y = Acceleration)) +
  geom_point() +
  labs(x = "Top Speed (km/h)",y = "Acceleration (sec)",title = "Low Correlation Scatter Plot") +
  theme_minimal()

#no correlation
ggplot(dataNumeric, aes(x = Seats, y = Drive)) +
  geom_point() +
  labs(x = "Number of Seats",y = "Drivetrain",title = "No Correlation Scatter Plot") +
  theme_minimal()


#linear regression for Efficiency
library(rsample)
set.seed(41)
reg_split <- initial_split(dataNumeric, prop = .75)  
reg_train <- training(reg_split)
reg_test <- testing(reg_split)

library(parsnip)
lm_fit <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression") |>
  fit(Efficiency ~ Range + Charging,
      data = reg_train)
lm_fit$fit
summary(lm_fit$fit)

forest_reg_fit <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("regression") |>
  fit(Efficiency ~ ., data = reg_train)

forest_reg_fit$fit

install.packages("yardstick")
library(yardstick)
reg_results <- reg_test
reg_results$lm_pred <- predict(lm_fit, reg_test)$.pred
reg_results$forest_pred <- predict(forest_reg_fit, reg_test)$.pred

yardstick::mae(reg_results, Efficiency, lm_pred)
yardstick::mae(reg_results, Efficiency, forest_pred)

yardstick::rmse(reg_results, Efficiency, lm_pred)
yardstick::rmse(reg_results, Efficiency, forest_pred)
