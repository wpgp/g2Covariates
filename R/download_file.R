#' Functions to download a file from WorldPop data repository using http or ftp
#'
#' @param f_remote A character, path to a remote file
#' @param dest_file A character, path where the file will be sved
#' @param ftp_srv Boolean, If TRUE then WorldPop ftp server will be used otherwise http
#' @param method Method to be used for downloading files.
#'  Current download methods are "internal", "wininet" (Windows only) "libcurl",
#' "wget" and "curl", and there is a value "auto"
#' @param quiet If TRUE, suppress status messages (if any), and the progress bar.
#'
#' @rdname download_file
#' @importFrom utils download.file
#' @noRd
#'

download_file <- function(f_remote=NULL,
                          dest_file=NULL,
                          ftp_srv=FALSE,
                          method="auto",
                          quiet=TRUE){


  if (ftp_srv){
    url_srv <- file.path("ftp://ftp.worldpop.org/GIS/Covariates/Global_2015_2030")
  }else{
    url_srv <- file.path("https://data.worldpop.org/GIS/Covariates/Global_2015_2030")
  }

  f_remote <- file.path(url_srv, f_remote)

  checkStatus <- tryCatch(
    {
      utils::download.file(f_remote, destfile=dest_file, mode="wb",quiet=quiet, method=method)

    },
    error=function(cond){
      message(paste("URL does not seem to exist:", f_remote))
      message("Here's the original error message:")
      message(cond)
    },
    warning=function(cond){
      message(paste("URL caused a warning:", f_remote))
      message("Here's the original warning message:")
      message(cond)
    },
    finally={
      ##
    }
  )

  if(inherits(checkStatus, "error") | inherits(checkStatus, "warning")){
    return(NULL)
  } else{
    return(invisible(TRUE))
  }

}
