# 'author@tahmina  
# ' The `add_scores` function adds a new column of conservation scores to a dataframe, ensuring proper alignment.  
# ' The length of the `scores` vector must match the number of rows in the dataframe.  
# ' If they do not match, the function stops with an error.  
# '  
# ' @param df A dataframe to which the scores will be added.  
# ' @param scores A numeric vector of conservation scores, with one score for each row in the dataframe.  
# ' @param column_name (Optional) A string specifying the name of the new column to store the scores.  
# ' Defaults to "conserv."  
# ' @return A dataframe with the new column added.  
# ' @examples  
# ' pi_scores <- subset_conserv_scores(conservation_score, 1057, 1114)  
# ' df_b2ar <- add_scores(df_b2ar, pi_scores)  

add_scores <- function(df, scores, column_name = "conserv") {
  # Validate inputs
  if (!is.data.frame(df)) {
    stop("The input 'df' must be a dataframe.")
  }
  if (!is.numeric(scores) || length(scores) == 0) {
    stop("The 'scores' must be a non-empty numeric vector.")
  }
  if (length(scores) != nrow(df)) {
    stop(paste("The number of scores (", length(scores), 
               ") must match the number of rows in the dataframe (", nrow(df), ").", sep = ""))
  }
  
  # Add the scores as a new column
  df[[column_name]] <- scores
  
  return(df)
}

## Example usage:
## Subset conservation scores for the PI region
# pi_scores <- subset_conserv_scores(conservation_score, 1057, 1114)

## Add conservation scores to the dataframe
# df_b2ar <- add_scores(df_b2ar, pi_scores, column_name = "pi_conservation")

## Print the updated dataframe
# print(head(df_b2ar))

#df_b2ar<- add_scores(df_b2ar, pi_scores)