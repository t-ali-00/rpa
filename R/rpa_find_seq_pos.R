# 'author@tahmina  
# ' The `find_seq_pos` function identifies the start and end positions of a target sequence  
# ' within a multiple sequence alignment (MSA). This ensures accurate mapping of the sequence,  
# ' even with alignment shifts due to insertions or deletions.  
# '  
# ' @param alignment A matrix representing the MSA, where each row corresponds to a sequence.  
# ' @param seq A character string of the target sequence to locate within the alignment.  
# ' @return A list with start and end positions of the target sequence in each aligned sequence.  
# '         If the target sequence is not found, it returns NA for that sequence.  

find_seq_pos <- function(alignment, seq) {
  # Validate inputs
  if (!is.matrix(alignment)) {
    stop("Alignment data must be a matrix.")
  }
  if (!is.character(seq) || nchar(seq) == 0) {
    stop("Target sequence must be a non-empty character string.")
  }
  
  # Collapse each row of the alignment matrix into a single string
  sequences <- apply(alignment, 1, paste, collapse = "")
  
  # Initialize a list to store the start and end positions for each sequence
  positions <- list()
  
  # Iterate over each sequence in the alignment
  for (i in seq_along(sequences)) {
    aligned_seq <- sequences[i]
    
    # Find the start position of the target sequence in the aligned sequence
    start_pos <- regexpr(seq, aligned_seq)[1]
    
    if (start_pos > 0) {
      # If the sequence is found, calculate the end position
      end_pos <- start_pos + nchar(seq) - 1
      positions[[i]] <- c(start = start_pos, end = end_pos)
    } else {
      # If not found, assign NA for start and end positions
      positions[[i]] <- c(start = NA, end = NA)
    }
  }
  
  # Return the list of positions
  return(positions)
}

# Example Usage:
# Read the alignment file and extract the alignment matrix
# align <- read.fasta("aln.fa")          # Read alignment file
# alignment_matrix <- align$ali          # Extract alignment matrix

# Define the target sequence to locate
# target_seq <- "NTASIAQARKLVEQLKMEANIDRIKVSKAAADLMAYCEAHAKEDPLLTPVPASENPFR"

# Find the start and end positions of the target sequence
# seq_positions <- find_seq_pos(alignment_matrix, target_seq)

# Print the positions
# print(seq_positions)

