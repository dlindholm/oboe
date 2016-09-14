#' @title Get position in tree for a specific term
#'
#' @author Daniel Lindholm
#'
#' @description Looks up where in the ontology tree (at which depth) a term is.
#' @param ontology A list (of class ontology) in which to look
#' @param id The identifier of the term to look up (character string)
#' @param root Identifier of the root.
#' @return Outputs a numeric value, corresponding to the depth, where 1 is the root.
#' @export

get_depth <- function(ontology, id, root = "DOID:4"){
  if(class(ontology) != "ontology") stop("Please provide an ontology")
  if(!is.character(id)) stop("Please provide id as a character string")
  if(!is.character(root)) stop("Please provide root as a character string")
  x <- get_parents(ontology, id, root, include_entry = TRUE)
  return(nrow(x))
}

