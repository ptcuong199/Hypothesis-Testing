# normality testing

### This is a normality testing function that returns quantile-quantile plot, hisogram, and results from Shapiro-Francia test for normality. 
# As a means of practicing R programming, I used tidy evaluation for creating a quoting function. You will see or might have seen other quoting 
# funcions that I created. They include the homo_skes() function (testing for homoskedasticity) and the auto_cor() function (testing for autocorrelation).

### Definition:
# data : takes an argument containing the data of interest
# y : takes a name or an expression as an argument, instead of a string character
# usage: takes a logical argument. As DEFAULT, usage is set to FALSE, which let the function does its job. When set to TRUE, the function returns the packages and the related functions being used within that function.

library(ggplot2)
library(dplyr)
library(gridExtra)
library(nortest)

### CAUTION: y must be input as if it is a vector, which means the name of the column in entered without "".
# Example: norm.text(vit, effort) instead of norm.text(vit, "effort")

norm.test <- function(data, y, usage = FALSE) {
  # Codes for using ggplot in a function
  y_1 <- enquo(y)
  params <- data %>% summarize(mean = mean(!!y_1), sd = sd(!!y_1))
  qqplot <- data %>% ggplot(aes(sample = !!y_1)) + geom_qq(dparams = params) + geom_abline()
  # If you want the y-axis to have frequency counts, then the normal curve needs to be scaled 
  # according to the number of observations and the binwidth.

  hist <- ggplot(aes(x = !!y_1), data = data) + 
    geom_histogram(binwidth = 1, position = 'stack', stat = 'bin') 
  graph <- grid.arrange(qqplot, hist, ncol =1)
 
  y_2 <- deparse(substitute(y))
  data_1 <- deparse(substitute(data)) 
  data_1 <- get(data_1)
  ### substitute() will lookup all the object names provided to it, and if it finds a value for that name, it will substitute the name for its value Adv-R. 
  # deparse() returns an expression as a string
  shapiro.francia <- sf.test(x = data_1[[y_2]])
  results <- list(graph, shapiro.francia)
  if(usage == TRUE){
    grid <- print("gridExtra package required for displaying 2 graphs adjacent to each other")
    sf <- print("nortest package required for using Shapiro-Francia test for normality")
    other <- print("dpyr, ggplot2 packages are also required")
    codes <- list(grid, sf,other)
    return(codes)
  } else {
    return(results)
  }
}

### https://stackoverflow.com/questions/29182228/plotting-normal-curve-over-histogram-using-ggplot2-code-produces-straight-line
### https://stackoverflow.com/questions/54377005/applicate-function-on-dataframe-in-r 
### https://stackoverflow.com/questions/46834655/whats-the-difference-between-substitute-and-quote-in-r 
### https://www.r-bloggers.com/programming-with-dplyr-by-using-dplyr/ 
