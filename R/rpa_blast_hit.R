# 'author@tahmina  
# ' The `blast_hit` function performs a BLAST search against the PDB database to find protein structures  
# ' similar to an input sequence. The function returns a table of BLAST hits, including details such as  
# ' sequence identity, chain information, and alignment scores. Optionally, the results can be filtered  
# ' based on a specified range of sequence identity.  
# '  
# ' @param prot_seq Character string representing the protein sequence to search.  
# ' @param identity Optional numeric vector of length 2 specifying the range of sequence identity  
# '                 for filtering results (e.g., `identity = c(30, 90)` filters hits with 30–90% identity).  
# ' @return A data frame of BLAST hits, with optional filtering applied. 
# ' wraps around {bio3d}

blast_hit <- function(prot_seq, identity = NULL) {
  # Perform BLAST search using the input protein sequence
  # - database = "pdb": Query the PDB database for protein structures.
  # - time.out = NULL: No timeout for the search.
  # - chain.single = TRUE: Retrieve hits for single chains only.
  blast_table <- blast.pdb(prot_seq, database = "pdb", time.out = NULL, chain.single = TRUE)
  
  # Extract the table of BLAST hits from the search results
  blast_hit_table <- blast_table$hit.tbl
  
  # Filter the hits based on the provided sequence identity range, if specified
  if (!is.null(identity)) {
    if (length(identity) != 2 || identity[1] > identity[2]) {
      stop("The 'identity' parameter must be a numeric vector of length 2 with the format c(min, max).")
    }
    blast_hit_table <- blast_hit_table[blast_hit_table$identity >= identity[1] & 
                                         blast_hit_table$identity <= identity[2], ]
  }
  
  # Return the filtered or unfiltered BLAST hit table
  return(blast_hit_table)
}

# ' Example usage:  
# ' Perform a BLAST search for a given protein sequence and filter results for 30–90% sequence identity:  
# ' hits <- blast_hit(prot_seq = "MVLSPADKTNVKAA", identity = c(30, 90))  
