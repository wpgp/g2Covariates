#' Get a file name from the path
#'
#' @param f file path
#' @noRd
get_fn_without_ext <- function(f){
  sub('\\..*$', '', basename(f))
}


#' Get a file name from the path
#'
#' @param f file path
#' @noRd
getExtension <- function(f){
  ex <- strsplit(basename(f), split="\\.")[[1]]
  return(ex[-1])
}

#' Get list of neighboring countries
#'
#' @rdname get_global_nb
#' @importFrom jsonlite fromJSON
#' @return A list of neighboring countries
#' @export
get_global_nb <- function(){

  x <- jsonlite::fromJSON("data/Global_nb_db.json")

  return(x)
}


