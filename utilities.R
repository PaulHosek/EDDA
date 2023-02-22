check_normality <- function(column_names, data, groups = NULL){
  shapiros = vector("list",length(column_names))
  par(mfrow=c(length(column_names),3+length(groups)/2))
  for (i in 1:length(column_names)){
    col_name <- column_names[[i]]
    col <- data[[col_name]]
    hist(col, xlab = col_name, main = paste("Histogram of", col_name))
    qqnorm(col)
    qqline(col)
    if (missing(groups)){
      boxplot(col, ylab=col_name, main = paste("Boxplot of", col_name))
    }else{
      for (g in groups){
        boxplot(col ~ data[[g]], ylab=col_name, xlab = g, 
                main = paste("Boxplot of", col_name, "by", g))
      }
    }
    shapiros[[i]] <- shapiro.test(col)
  }
  return(shapiros)
}

check_vars <- function(anova_model){
  par(mfrow=c(1,2))
  qqnorm(residuals(anova_model), main = "Normal Q-Q Plot of Residuals")
  plot(fitted(anova_model), residuals(anova_model), 
       main = "Residuals vs Fitted Values")
}
