#' @title Read and parse obo (Open Biomedical Ontologies) files
#'
#' @author Daniel Lindholm
#'
#' @description Reads and parses obo files.
#' @param obo_file a character vector pointing to the obo file to read.
#' @return Outputs a list of class 'ontology'.
#' @examples
#' # This example downloads the most recent disease ontology obo file
#' # and converts it to an R list:
#'
#' doid <- read_obo("http://purl.obolibrary.org/obo/doid.obo")
#' head(doid)
#'
#' @export

read_obo <- function(obo_file){
  library(parallel)
  if(.Platform$OS.type != "unix") cores <- 1
  else cores <- detectCores()
  #Read file
  obo <- readLines(obo_file)
  #Remove quotes from read file
  obo <- gsub("\"", "", obo)
  #Remove header and divide terms into list elements
  obo <- obo[seq(which(obo == "[Term]")[1], length(obo))]
  start_element <- grep("^\\[.+\\]$", obo)
  obo <- split(obo, rep.int(seq_along(start_element),
                            diff(c(start_element, length(obo) + 1))))
  #Select only Terms
  obo <- obo[sapply(obo, function(elem) elem[1] == "[Term]")]
  #Remove [Term] from list
  obo <- mclapply(obo, function(x) x[x != "[Term]"], mc.cores = cores)
  #Remove blank lines from list
  obo <- mclapply(obo, function(x) x[x != ""], mc.cores = cores)
  #Create empty list
  result <- list()
  #Split values in each term at first occurence of ": " in each string
  obo <- mclapply(obo, function(x) regmatches(x, regexpr(": ", x), invert = TRUE), mc.cores = cores)
  #Parse variables into nice list
  parse_vars <- function(x){
    #x <- lapply(x, function(x) regmatches(x, regexpr(": ", x), invert = TRUE))
    d <- list()
    for(i in 1:length(x)){
      if(x[[i]][[1]] %in% names(d)){
        d[[x[[i]][[1]]]][length(d[[x[[i]][[1]]]])+1] <- x[[i]][[2]]
      }
      else
        d[[x[[i]][[1]]]] <- x[[i]][[2]]
    }
    return(d)
  }
  for(i in 1:length(obo)){
    result[[i]] <- parse_vars(obo[[i]])
  }
return(structure(result, class = "ontology"))
}


