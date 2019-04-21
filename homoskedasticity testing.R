# homoskedasticity
### This is a normality testing function that returns quantile-quantile plot, hisogram, and results from Shapiro-Francia test for normality. 
# As a means of practicing R programming, I used tidy evaluation for creating a quoting function. You will see or might have seen other quoting 

# funcions that I created. They include the homo_skes() function (testing for homoskedasticity) and the auto_cor() function (testing for autocorrelation).



### Definition:

# data : takes an argument containing the data of interest

# y : takes a name or an expression as an argument, instead of a string character

# usage: takes a logical argument. As DEFAULT, usage is set to FALSE, which let the function does its job. When set to TRUE, the function returns the packages and the related functions being used within that function.

install.packages("broom")
library(broom)
install.packages("lmtest")
library(lmtest)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

homo_skes <- function(data, x, y, usage = FALSE) {
  x_1 <- enquo(x)
  y_1 <- enquo(y)
  x_2 <- deparse(substitute(x))
  y_2 <- deparse(substitute(y))
  data_1 <- deparse(substitute(data)) 
  data_1 <- get(data_1)
  fit <- lm(data_1[[y_2]]~data_1[[x_2]])
  summary <- tidy(fit, conf.int = TRUE)
  
  residual_plot <- data_1 %>% mutate(residual = !!y_1 - predict(fit)) %>% 
    ggplot(aes(!!x_1, residual)) + 
    geom_point(alpha = 0.5)
  
  goldfeld_quandt <- gqtest(formula = fit, alternative = "two.sided")
  breusch_pagan <- bptest(formula = fit)
  results <- list(summary, residual_plot, goldfeld_quandt, breusch_pagan)
  if(usage == TRUE){
    tidy <- print("broom package required for using tidy() function")
    gq <- print("lmtest package required for using Goldfeld-Quandt test for normality")
    other <- print("dpyr, ggplot2 packages are also required")
    codes <- list(tidy, gq, other)
    return(codes)
  } else {
    return(results)
  }
}

# If p > 0.05 in Goldenfeld-Quandt test, homoskedasticity is present
