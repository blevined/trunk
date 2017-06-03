#' The Keep Complete function
#' 
#' This function takes a dataset with NA values and returns that dataset cleaned of all rows that contain an NA value.
#' 
#' @param df The dataframe you want to clean!
#' @keywords complete.cases, data cleaning, tidying
#' @export
#' @examples
#' keep_complete()
#' mtcars[mtcars>160] <- NA
#' mtcars = keep_complete(mtcars)
#' print(mtcars)



# The function's output is a 
keep_complete <- function(df){
  df[complete.cases(df),]
  }

