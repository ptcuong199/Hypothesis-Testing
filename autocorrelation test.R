# autocorrelation
### This is an autocorrelation testing function that returns the plot "y vs x", the residual plot, summary of the linear regression model, and results 
# from Durbin-Watson test for autocorrelation. As a means of practicing R programming, I used tidy evaluation for creating a quoting function. 
# You will see or might have seen other quoting functions that I created. They include the norm_test() function (testing for normality) and 
# the homo_skes() function (testing for homoskedasticity).

### Definition:
# data : takes an argument containing the data of interest
# x: takes the name or the expression of the independent variable as an argument, instead of a string character.
# y : takes the name or the expression of the dependent variable as an argument, instead of a string character
# usage: takes a logical argument. As DEFAULT, usage is set to FALSE, which let the function does its job. When set to TRUE, the function returns the packages and the related functions being used within that function.

library(gridExtra)
library(lmtest)
library(broom)
library(dplyr)
library(ggplot2)

auto_cor <- function(data, x, y, usage = FALSE) {
  x_1 <- enquo(x)
  y_1 <- enquo(y)
  x_2 <- deparse(substitute(x))
  y_2 <- deparse(substitute(y))
  data_1 <- deparse(substitute(data)) 
  data_1 <- get(data_1)
  fit <- lm(data_1[[y_2]]~data_1[[x_2]])
  summary <- tidy(fit, conf.int = TRUE)
  
  plot <- data_1 %>% ggplot(aes(!!x_1, !!y)) + geom_point(alpha = 0.5)
  data_1 <- data_1 %>% mutate(residual = !!y - predict(fit)) 
  residual_plot <- data_1 %>% ggplot(aes(!!x_1, residual)) + geom_point(alpha = 0.5)
  graph <- grid.arrange(plot, residual_plot, ncol =1)
  
  #Checking assumption for Durbin-Watson test. Assumptions is: the errors are normally distributed with a mean of 0.
  shapiro.francia <- sf.test(x = data_1$residual)
  mean_res <- mean(data_1$residual)
  if (mean_res < 1 & mean_res > -1 & shapiro.francia$p.value > 0.05) {
    durbin_watson <- dwtest(fit, alternative = "two.sided")
  } else {print("Durbin-Watson test cannot perform on non-normal data with a non-zero mean")}
  
  results <- list(summary, graph, durbin_watson)
  
  if(usage == TRUE){
    tidy <- print("broom package required for using tidy() function")
    dw <- print("lmtest package required for using Durbin-Watson test for autocorrelation")
    grid <- print("gridExtra package required for displaying 2 graphs adjacent to each other")
    other <- print("dpyr, ggplot2 packages are also required")
    codes <- list(tidy, dw, grid, other)
    return(codes)
  } else {
    return(results)
  }
}
  
  
