# normality, multicollinearity, homogeneity of variances

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
  
  # Codes for using a function inside a function when y is not a character string
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

geom_histogram(binwidth = 1, aes(y = ...density...)) + 
  stat_function(fun = dnorm, col = "red", args = list(mean = mean(x), sd = sd(y))) +
  scale_x_continuous(limits = c(0, max(x))) # to draw a normal curve over a histogram

# If p > 0.05 in shapiro-francia test, the distribution of the data equals the normal distribution