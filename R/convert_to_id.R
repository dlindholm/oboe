

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
