# author@tahmina
# Description: This function extracts secondary structure (SSE) and solvent accessibility (ACC) 
# data for residues from a PDB file using the DSSP program. The residue-level attributes are 
# then merged with a provided protein interface data frame (pi_df) for protein interface analysis.
# wraps around {bio3d}.

run_dssp <- function(pdb, pi_df, dssp_path = "/C:/Program Files/dssp/mkdssp.exe") {
  # Run DSSP to extract secondary structure and solvent accessibility data
  dssp_data <- dssp(pdb, exefile = dssp_path, resno = FALSE, full = FALSE)
  
  # Create a data frame with ACC and SSE attributes from DSSP output
  dssp_df <- data.frame(Acc = dssp_data$acc, SSE = dssp_data$sse)
  
  # Extract numeric residue numbers from row names
  dssp_df$rownumeric <- as.numeric(sapply(row.names(dssp_df), function(x) {
    strsplit(x, split = '_')[[1]][1]
  }))
  
  # Ensure 'rownumeric' is numeric for compatibility during merging
  dssp_df$rownumeric <- as.numeric(dssp_df$rownumeric)
  
  # Merge DSSP data with the provided protein interface data frame (pi_df) by residue number
  merged_df <- merge(pi_df, dssp_df, by.x = "resno", by.y = "rownumeric", all.x = TRUE)
  
  # Return the merged data frame
  return(merged_df)
}

# Example usage:
# Assuming 'pdb_file' is the PDB file and 'pi_df' contains the 'resno' column
result <- run_dssp(pdb_file, pi_df)
View(result)