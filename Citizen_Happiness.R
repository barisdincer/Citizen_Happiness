knitr::opts_chunk$set(echo = TRUE)
library(readr)
library(ggplot2)
library(tinytex)

options(tinytex.verbose = TRUE)


data2019 <- read_csv("C:/Users/baris/OneDrive/Desktop/2019_Counrties_Dataset.csv")


summary(data2019)
colnames(data2019)
dim(data2019)
str(data2019)


# Clean your data and prepare for the modeling

anyNA(data2019)


# fit your model 1 for the data

model <- lm(Score ~ `GDP per capita` + `Social support` + `Healthy life expectancy`, data = data2019)
coefficients <- coef(model)
p_values <- summary(model)$coefficients[,4]


coefficients1 <- coefficients[-1]
variable_names <- names(coefficients1)
data <- data.frame(variable = variable_names, coefficient = coefficients1)

ggplot(data, aes(x = variable, y = coefficient)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  labs(x = "Variable", y = "Coefficient") +
  theme_minimal()


par(mfrow=c(1,3))
plot(data2019$`GDP per capita`, data2019$Score, main = "GDP per Capita vs. Happiness", xlab = "GDP per Capita", ylab = "Happiness")
abline(model, col = "red")

plot(data2019$`Social support`, data2019$Score, main = "Social Support vs. Happiness", xlab = "Social Support", ylab = "Happiness")
abline(model, col = "red")

plot(data2019$`Healthy life expectancy`, data2019$Score, main = "Healthy Life Expectancy vs. Happiness", xlab = "Healthy Life Expectancy", ylab = "Happiness")
abline(model, col = "red")


# fit your model 2 for the data
model2 <- lm(Score ~ `Freedom to make life choices` + `Generosity` + `Perceptions of corruption`, data = data2019)

summary(model2)

coefficients2 <- coef(model2)
p_values2 <- summary(model2)$coefficients[,4]

coefficients2 <- coefficients2[-1]
variable_names <- names(coefficients2)
data <- data.frame(variable = variable_names, coefficient = coefficients2)
ggplot(data, aes(x = variable, y = coefficient)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  labs(x = "Variable", y = "Coefficient") +
  theme_minimal()

par(mfrow=c(1,3))
plot(data2019$`Freedom to make life choices`, data2019$Score, main = "Freedom to Make Life Choices vs. Happiness", xlab = "Freedom to Make Life Choices", ylab = "Happiness")
abline(model, col = "red")

plot(data2019$Generosity, data2019$Score, main = "Generosity vs. Happiness", xlab = "Generosity", ylab = "Happiness")
abline(model, col = "red")

plot(data2019$`Perceptions of corruption`, data2019$Score, main = "Perceptions of Corruption vs. Happiness", xlab = "Perceptions of Corruption", ylab = "Happiness")
abline(model, col = "red")

# Evaluate Model Performance
set.seed(123)
train_indices <- sample(1:nrow(data2019), 0.8 * nrow(data2019))
train_data <- data2019[train_indices, ]
test_data <- data2019[-train_indices, ]  

model1 <- lm(Score ~ `GDP per capita` + `Social support` + `Healthy life expectancy`, data = train_data)

model2 <- lm(Score ~ `Freedom to make life choices` + `Generosity` + `Perceptions of corruption`, data = train_data)

model1_predictions <- predict(model1, newdata = test_data)

model2_predictions <- predict(model2, newdata = test_data)

model1_r2 <- cor(test_data$Score, model1_predictions)^2

model2_r2 <- cor(test_data$Score, model2_predictions)^2

cat("Model 1 R-squared:", model1_r2, "\n")

cat("Model 2 R-squared:", model2_r2, "\n")