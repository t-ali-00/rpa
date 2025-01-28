# 'author@tahmina
# Extracts and prints the amino acid sequence from a dataframe of residue IDs. 
# Useful for displaying the sequence of a protein interface after trimming a PDB file.


pseq_pi <- function(df) {
  # Extract residue IDs
  residues <- df$resid
  
  # Concatenate the amino acids into a single string
  sequence <- paste(residues, collapse = "")
  
  # Print the sequence
  print(sequence) 
}

