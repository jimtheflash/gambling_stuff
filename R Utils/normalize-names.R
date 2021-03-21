
#' @param x Vector of strings to normalize
#' @param key List of elements containing fields "name" and "aliases"
#' @param warn Optional, Boolean -- warn if a name in X was not found in the Key names
#' This is useful if you're passing some value you've never seen before (like all of a sudden,
#' MEM is being labeled MPH and you never considered that as part of the alias set)
#' 
normalize_names <- function(x, key, warn = TRUE) {
  
  # Allow for passing the json file directly, otherwise KEY is assumed to be a list
  if (is.character(key) &&
      grepl(pattern = '\\.json$', x = 'string.json')) {
    cat("Reading JSON file...\n")
    key <- jsonlite::read_json(key)
  } 
  
  # Fun warning times
  # Does every element contain two fields - name and aliases?
  field_check <- sapply(key, function(k) {
    cnames <- names(k)
    # Check on names
    if (!identical(cnames, c('name','aliases'))) {
      stop("Each element of 'key' must contain exactly two fields, 'name' and 'aliases'")
    }
    # If names exist, check types
    if (!is.character(k$name)) stop("Each element 'name' but be a string")
    if (!is.list(k$aliases)) stop("Each element 'aliases' but be a list of strings")
    
    
  })
  
  # Run through and do the work
  for (i in seq_along(key)) {
    # For every element, GSUB 
    patterns <- paste(key[[i]]$aliases, collapse="|")
    x <- gsub(pattern = patterns, 
         replacement = key[[i]]$name, 
         x = x)
  }
  
  # Yell if there is something in the list that was not in the names OR aliases
  # We can just look at Names now, because if it WAS in the aliases, it would
  # have been replaced by a name.  So any leftover item in the list that isn't
  # in "all_names" is something we didn't expect to see
  all_names <- sapply(key, '[[', 'name')
  if (!all(x %in% all_names) && warn == TRUE) {
    msg <- paste0("X contained Strings not found in 'name' - ", 
                  paste(x[!(x %in% all_names)], collapse = ', '))
    warning(msg)
  }
    
  # Return the string
  return(x)
}