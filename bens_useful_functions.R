##Useful R functions


# This function loads a data frame and cleans it of all NA values.
# It returns the same data frame with only complete cases. 
keep_complete <- function(x){
  x[complete.cases(x),]}


# This function loads a data frame with 'year_quarter_numeric' as a vector name
# It returns the same data frame with two extra vectors: 
        # a date variable equal to the date value of the quarter (i.e. 20172 would be converted to "2017-04-01")
        # a date variable equal to stylized quarterly version of the quarter (i.e. 20172 would be converted to "2017 Q2")
yq_to_date = function(x){
  x =  x %>% dplyr::mutate(yearqtr = zoo::as.Date(zoo::as.yearqtr(x$year_quarter_numeric, format = "%Y%q")),
                           yearq =  zoo::as.yearqtr(x$year_quarter_numeric, format = "%Y%q"));
}



install_useful = function(){
  useful = c("Hmisc", "RJDBC", "tidyverse", "lubridate"," zoo", "gridExtra", "scales", "parallel", "data.table", "broom", "tidyquant", "timekit", "timeDate", "doMC");
  j = c(Hmisc, RJDBC, tidyverse, lubridate, zoo, gridExtra, scales, parallel, data.table, broom, tidyquant, timekit, timeDate, doMC);
  for(i in useful){
    install.packages(i);
    library(j);
    
  }
}


roundr <-
  function(x, digits=1){
    if(digits < 1)
      stop("This is intended for the case digits >= 1.")
    
    if(length(digits) > 1) {
      digits <- digits[1]
      warning("Using only digits[1]")
    }
    
    tmp <- sprintf(paste("%.", digits, "f", sep=""), x)
    
    # deal with "-0.00" case
    zero <- paste0("0.", paste(rep("0", digits), collapse=""))
    tmp[tmp == paste0("-", zero)] <- zero
    
    tmp
  }
