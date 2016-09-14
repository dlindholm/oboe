#' @title Map ICD, OMIM, SNOMED or any cross-reference code to an ontology identifier
#'
#' @author Daniel Lindholm
#'
#' @description Will look up a code within the "xref" part of an ontology database
#' and translate to an ontology identifier. If several identifiers are found, the
#' one that is deepest in the tree will be returned. If there are multiple entries
#' within the same depth, the first identifier in the list will be returned.
#' @param ontology a list (of class ontology) in which to look
#' @param code the code to look-up
#' @param from Which classification is 'code' apart of? Defaults to "ICD10CM",
#' other options (when using the disease ontology database) include "OMIM", "MSH",
#' "SNOMEDCT_US_2016-03-01", etc.
#' @return Outputs a character string containing the mapped identifier.
#' @export

map_to_id <- function(ontology, code, from = "ICD10CM", root = "DOID:4"){
  if(class(ontology) != "ontology") stop("Please provide an ontology")
  if(!is.character(code)) stop("Please provide code as a character string")
  code_combined <- paste0(from, ":", code)
  x <- ontology[find_entry(ontology, "xref", code_combined)]
  if(length(x) > 1){
    x_id <- character()
    depth <- numeric()
    for(counter in 1:length(x)){
      x_id[counter] <- x[[counter]]$id
      depth[counter] <- get_depth(ontology, x_id[counter], root)
    }
    result <- data.frame(id=as.character(x_id), depth)
    return(as.character(result[result$depth == max(result$depth), ]$id))
  }
  if(length(x) == 1){
    return(x[[1]]$id)
  }
  if(length(x) == 0){
    return(NA)
  }
}
