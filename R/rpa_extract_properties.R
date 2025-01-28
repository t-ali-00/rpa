#' author@tahmina  
#' The `extract_properties` function extracts atomic-level attributes from a trimmed PDB interface object in Bio3D,  
#' such as residue number (`resno`), residue identity (`resid`), atomic coordinates (`x, y, z`), and chain information.  
#' The extracted attributes are stored in a data frame (`atom_df`) for downstream protein interface analysis.  
#' Optionally, the function can filter the output to include only C-alpha atoms.  

extract_properties <- function(interface_atoms, keep_calpha = FALSE) {
  # Extract residue and atomic attributes from the PDB interface object
  resno <- interface_atoms$atom$resno        # Residue numbers
  resid <- interface_atoms$atom$resid        # Residue identity (3-letter codes)
  chain <- interface_atoms$atom$chain        # Chain identifiers
  elety <- interface_atoms$atom$elety        # Atom types (e.g., CA, N, C)
  x <- interface_atoms$atom$x                # X-coordinate of the atom
  y <- interface_atoms$atom$y                # Y-coordinate of the atom
  z <- interface_atoms$atom$z                # Z-coordinate of the atom
  
  # Create a data frame with extracted atomic properties
  atom_df <- data.frame(resid = resid, chain = chain, elety = elety, 
                        resno = resno, x = x, y = y, z = z)
  
  # Convert residue identity from 3-letter to 1-letter amino acid codes for simplicity
  atom_df$resid <- aa321(atom_df$resid)
  
  if (keep_calpha) {
    # Select indices of C-alpha atoms if only backbone atoms are needed
    ca_inds <- atom.select(interface_atoms, "calpha")$atom
    
    # Filter the atom data frame to keep only rows corresponding to C-alpha atoms
    atom_df <- atom_df[ca_inds, ]
  }
  
  # Return the data frame with extracted and optionally filtered properties
  return(atom_df)
}

#' Example usage:
#' Extract properties from the trimmed PDB interface object, keeping only C-alpha atoms:
#' pi_df <- extract_properties(interface_atoms, keep_calpha = TRUE)

