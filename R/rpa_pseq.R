# 'author@tahmina
# Retrieves and prints the amino acid sequence from a PDB file or object. 
# Accepts a PDB ID or a PDB object, extracts the sequence in one-letter codes, and prints it.


pseq <- function(input) {
  if (is.character(input)) {
    # Read the PDB file using PDB ID
    pdb <- read.pdb(input)
  } else {
    # Use the direct PDB object
    pdb <- input
  }
  
  # Get the amino acid sequence in the 1-letter form
  seq <- aa321(pdb$seqres)
  
  # Concatenate the sequence
  pseq <- paste(seq, sep = "", collapse = "")
  
  # Print the sequence
  print(pseq)
}