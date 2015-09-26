#' Overrides base \code{hist} to work on dataframes.  
#' Iterates interactively, plots histograms & barcharts of all columns in batch. 
#'
#' @examples {
#'  data(mtcars)
#'  hist(mtcars)
#'  hist(iris)
#' }
hist.data.frame <- function(df, mfrows = 4, mfcols = 4, ...) {
  par(mfrow = c(mfrows, mfcols)) # TODO: feels ugly here
  graphs_per_pane <- mfrows * mfcols
  for(i in seq_len(NCOL(df))) {
    col_name <- colnames(df)[i]
    col <- df[, i]
    if (is(col, 'numeric')) 
      hist(col, main = col_name, ...)
    else if(is(col, 'factor'))  
      barplot(table(col), main = col_name, ...) 
    else 
      message(col_name, 'will be skipped as a character type') 

    # Pause for user input 
    if (i %% graphs_per_pane == 0) readkey() 
  }
  # TODO: can this be on.exit?
  par(mfrow = c(1, 1)) 
}

readkey <- function()
{
    message(crayon::blue("Press [enter] to continue"))
    line <- readline()
}
