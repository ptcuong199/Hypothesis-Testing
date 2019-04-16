anova <- function(data, ind.var, dep.var, within, tukey = FALSE) {
  data <- data
  ind.var <- ind.var
  dep.var <- dep.var
  within <- within
  if(length(within) > 0 & sum(names(data) == "Subject") == 0) {
    print("Data containing within-subjects variables requires a Subject column. If the column exists, please rename it to 'Subject'")
  } else {
    subject <- data$Subject
  }
  if(length(dep.var) > 1) {
    print("MANOVA required for data with more than 1 dependent variables")
  }
  if(length(ind.var == 3)) {
    dv <- unlist(data[dep.var])
    iv_1 <- unlist(data[ind.var[1]])
    iv_2 <- unlist(data[ind.var[2]])
    iv_3 <- unlist(data[ind.var[3]])
    
    if(length(within) == 0) {
      model <- aov(dv~iv_1*iv_2*iv_3)
      results <- summary(model)
    }
    if(length(within) == 1) {
      w <- unlist(data[within])
      model <- aov(dv~iv_1*iv_2*iv_3*w + Error(subject/w))
      results <- summary(model)
    }
    if(length(within) == 2) {
      w_1 <- unlist(data[within[1]])
      w_2 <- unlist(data[within[2]])
      model <- aov(dv~iv_1*iv_2*iv_3*w_1*w_2 + Error(subject/(w_1*w_2)))
      results <- summary(model)
    }
    if(length(within) > 2) {
      print("Function not built for more than 2 within-subjects variables")
    }
  }
  if(length(ind.var) == 4) {
    dv <- unlist(data[dep.var])
    iv_1 <- unlist(data[ind.var[1]])
    iv_2 <- unlist(data[ind.var[2]])
    iv_3 <- ulist(data[ind.var[3]])
    iv_4 <- unlist(data[ind.var[4]])
    
    if(length(within) == 0) {
      model <- aov(dv~iv_1*iv_2*iv_3*iv_4)
      results <- summary(model)
    }
    if(length(within) == 1) {
      w <- unlist(data[within])
      model <- aov(dv~iv_1*iv_2*iv_3*iv_4*w + Error(subject/w))
      results <- summary(model)
    }
    if(length(within) == 2) {
      w_1 <- unlist(data[within[1]])
      w_2 <- unlist(data[within[2]])
      model <- aov(dv~iv_1*iv_2*iv_3*iv_4*w_1*w_2 + Error(subject/(w_1*w_2)))
      results <- summary(model)
    }
    if(length(within) > 2) {
      print("Function not built for more than 2 within-subjects variables")
    }
  } 
  if(results[[1]][["Pr(>F)"]][1] > 0.05 | tukey == TRUE) {
    post_hoc <- TukeyHSD(model)
    info <- list(results, post_hoc)
    return(info)
  } else {
    return(results)
  }
}

### unlist(data_frame["name_of_the_column"]) == data_frame$name_of_the_column. 
# If dep.var contains a string character of name_of_the_column, we can do unlist(data_frame[dep.var]) to retrieve the results similar to
# data_frame$name_of_the_column
