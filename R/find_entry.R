#' @title Find an ontology term based on value of an element
#'
#' @author Daniel Lindholm
#'
#' @description Searches for a string in an element of an ontology and returns
#' a logical vector, which could be used to subset the ontology.
#' @param ontology a list (of class ontology) in which to look
#' @param element in which element of the ontology should the search be done, in
#' disease ontology, you could e.g. provide "xref" to look at cross-references, or
#' "id" to look at specific entries
#' @param string a character string to search for, e.g. "ICD10CM:I10".
#' @export

find_entry <- function(ontology, element, string){
  if(class(ontology) != "ontology") stop("Please provide an ontology")
  if(!is.character(element)) stop("Please provide an element in which to search for string")
  if(!is.character(string)) stop("Please provide a character string to search for")
  result <- logical()
  for (i in 1:length(ontology)){
    result[i] <- string %in% ontology[[i]][[element]]
  }
  return(result)
}
