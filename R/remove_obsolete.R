#' @title Remove obsolete terms from an ontology
#'
#' @author Daniel Lindholm
#'
#' @description Convenience function for removing obsolete terms from an ontology.
#' @param ontology a list (of class ontology) in which to look
#' @return Outputs a list with obsolete terms removed.
#' @export
remove_obsolete <- function(ontology){
  if(class(ontology) != "ontology") stop("Please provide an ontology")
  result <- ontology[!find_entry(ontology, "is_obsolete", "true")]
  return(structure(result, class = "ontology"))
}
