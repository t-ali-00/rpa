# 'author@Tahmina  
# ' The `subset_conserv_scores` function extracts conservation scores for a specific region of interest,  
# ' such as a Protein Interface (PI), from a vector of residue conservation scores.  
# '  
# ' @param scores A numeric vector of conservation scores, where each index corresponds to a residue position.  
# ' @param start_position An integer indicating the starting position (inclusive) of the region of interest.  
# ' @param end_position An integer indicating the ending position (inclusive) of the region of interest.  
# ' @return A numeric vector containing the conservation scores for the specified range.  
# ' @examples  
# ' conserv_scores <- c(0.85, 0.92, 0.78, 0.95, 0.89)  
# ' pi_scores <- subset_conserv_scores(conserv_scores, 2, 4)  
# ' print(pi_scores) # Returns: c(0.92, 0.78, 0.95)  

subset_conserv_scores <- function(scores, start_position, end_position) {
  # Validate inputs
  if (!is.numeric(scores) || length(scores) == 0) {
    stop("The 'scores' vector must be numeric and non-empty.")
  }
  if (!is.numeric(start_position) || !is.numeric(end_position)) {
    stop("Start and end positions must be numeric.")
  }
  if (start_position < 1 || end_position > length(scores)) {
    stop("Start and end positions must fall within the range of the 'scores' vector.")
  }
  if (start_position > end_position) {
    stop("Start position must be less than or equal to the end position.")
  }
  
  # Subset the conservation scores
  subset_scores <- scores[start_position:end_position]
  
  return(subset_scores)
}

# Example usage:
# Define a vector of conservation scores
# conserv_scores <- c(0.85, 0.92, 0.78, 0.95, 0.89)

# Extract conservation scores for positions 2 through 4
# pi_scores <- subset_conserv_scores(conserv_scores, 2, 4)

# Print the subset
# print(pi_scores) # Output: c(0.92, 0.78, 0.95)
