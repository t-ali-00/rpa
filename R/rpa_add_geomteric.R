# 'author@tahmina
# ' The `add_geometric` function enhances a protein interface dataframe (`pi_df`) by appending columns 
# ' for residue-specific geometric properties (e.g., radius of gyration, steric parameters) sourced from 
# ' the Amino Acid Index database. These properties are matched to residues using their identifiers.

add_geometric <- function(pi_df) {
  # List of geometric properties to be added to the dataframe
  geometric_properties <- c("Radius of gyration of side chain", 
                            "STERIMOL minimum width of the side chain", 
                            "Apparent partial specific volume", 
                            "Smoothed upsilon steric parameter")
  
  # Loop through each property to extract its values and add them to the dataframe
  for (property in geometric_properties) {
    # Locate the property in the Amino Acid Index database
    index <- which(sapply(aa.index, function(x)
      length(grep(property, x$D, ignore.case = TRUE)) != 0))
    
    # If a matching property is found, add it to the dataframe
    if (length(index) > 0) {
      values <- aa.index[[index]]$I  # Extract the property values
      # Add property values to the dataframe, matching by residue identifier
      pi_df[[gsub(" ", "_", tolower(property))]] <- values[pi_df$resid]
    } else {
      warning(paste("Property not found in Amino Acid Index:", property))
    }
  }
  
  # Return the updated dataframe with new geometric properties added
  return(pi_df)
}

# ' Example usage:
# ' Enhance the dataframe by adding geometric properties
# ' pi_df <- add_geometric(pi_df)