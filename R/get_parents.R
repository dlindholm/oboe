#' @title Get parent nodes of a specific ontology term
#'
#' @author Daniel Lindholm
#'
#' @description Will look up in the hierarchy until the root is reached.
#' @param ontology A list (of class ontology) in which to look
#' @param id The identifier of the term to look up (character string)
#' @param root The identifier of the root entry. Defaults to "DOID:4", which is
#' the term "disease" in the disease ontology (http://www.disease-ontology.org/).
#' @param include_entry Should the term to look up be included in the results?
#' Defaults to FALSE
#' @return Outputs a dataframe with identifiers, descriptions, and depth.
#' @export

get_parents <- function(ontology, id, root = "DOID:4", include_entry = FALSE){
  if(class(ontology) != "ontology") stop("Please provide an ontology")
  if(!is.character(id)) stop("Please provide id as a character string")
  if(!is.character(root)) stop("Please provide root as a character string")
  # Get the first entry
  first_entry <- ontology[find_entry(ontology, "id", id)][[1]]
  split_entry <- c(first_entry$id, first_entry$name)
  first_split_entry <- split_entry
  # Loop until root is reached
  counter <- 0
  ids <- character()
  texts <- character()
  while(split_entry[1] != root){
    entry <- doid[find_entry(doid, "id", split_entry[1])][[1]]$is_a
    split_entry <- unlist(strsplit(entry, " ! "))
    counter <- counter + 1
    ids[counter] <- split_entry[1]
    texts[counter] <- split_entry[2]
  }
  result <- data.frame(id = rev(as.character(ids)),
                       text = rev(as.character(texts)),
                       depth = 1:length(ids))
  if(include_entry){
    initial_entry <- data.frame(id = first_split_entry[1],
                                text = first_split_entry[2],
                                depth = length(ids) + 1)
    result <- rbind(result, initial_entry)
  }
  return(result)
}
