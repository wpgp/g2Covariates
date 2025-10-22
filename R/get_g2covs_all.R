## 
## Function developed by Dr. Somnath Chaudhuri
#' Function to download a geospatial covariate for multiple years
#'
#' @rdname g2c_download_years
#' @param covariate A character, name of the covariate to download
#' @param ISO A character, ISO code(s) of a specified country
#' @param years Numerical, the years of the covariate
#' @param output_dir Character vector containing the path to the directory for
#'        writing output files. Default is the temp directory.
#' @param rst_mask path to mask file (Default NULL)
#' @param quiet If TRUE, suppress status messages (if any), and the progress bar.
#' @param ...	Additional arguments:\cr
#'        \code{ftp_srv}: Boolean, If TRUE then WorldPop ftp server will
#'        be used otherwise http.\cr
#'        \code{method}: Method to be used for downloading files. Current download
#'        methods are "internal", "wininet" (Windows only) "libcurl",  "wget" and "curl",
#'        and there is a value "auto"
#'        \code{cells_touches}: 	logical. If TRUE, all cells touched by lines or ,
#'        polygons are affected, not just those on the line render path
#'         or whose center point is within the polygon.\cr
#'        \code{keep_tmp}: 	logical. If TRUE, do not remove ,
#'        any temporal files downloaded.\cr
#' @return A character, path to downloaded file
#' @export
#' @examples
#' \dontrun{
#' g2c_download_years(covariate=NULL,
#'                    ISO=NULL,
#'                    years=c(2021,2024),
#'                    rst_mask=NULL,
#'                    output_dir=NULL,
#'                    quiet=TRUE)
#' }
g2c_download_years <- function(covariate, ISO, years, output_dir, rst_mask = NULL, quiet = FALSE, ...) {
  for (yr in years) {
    message("Downloading ", covariate, " for year ", yr)
    tryCatch({
      g2c_download(
        covariate = covariate,
        ISO = ISO,
        prj_year = yr,
        ftp_srv = FALSE,            ## if you want to avoid downloading from ftp
        output_dir = output_dir,
        rst_mask = rst_mask,
        quiet = quiet,
        ...
      )
    }, error = function(e) {
      message("Failed: ", covariate, " year ", yr, " | ", e$message)
    })
  }
}



#========================================================================

## 
## Function developed by Dr. Somnath Chaudhuri
#' Function to download all geospatial covariates for a year
#'
#' @rdname g2c_download_all_covs
#' @param ISO A character, ISO code(s) of a specified country
#' @param year Numerical, the year of the covariate
#' @param output_dir Character vector containing the path to the directory for
#'        writing output files. Default is the temp directory.
#' @param rst_mask path to mask file (Default NULL)
#' @param quiet If TRUE, suppress status messages (if any), and the progress bar.
#' @param ...	Additional arguments:\cr
#'        \code{ftp_srv}: Boolean, If TRUE then WorldPop ftp server will
#'        be used otherwise http.\cr
#'        \code{method}: Method to be used for downloading files. Current download
#'        methods are "internal", "wininet" (Windows only) "libcurl",  "wget" and "curl",
#'        and there is a value "auto"
#'        \code{cells_touches}: 	logical. If TRUE, all cells touched by lines or ,
#'        polygons are affected, not just those on the line render path
#'         or whose center point is within the polygon.\cr
#'        \code{keep_tmp}: 	logical. If TRUE, do not remove ,
#'        any temporal files downloaded.\cr
#' @return A character, path to downloaded file
#' @export
#' @examples
#' \dontrun{
#' g2c_download_all_covs(ISO=NULL,
#'                       year=2021,
#'                       rst_mask=NULL,
#'                       output_dir=NULL,
#'                       quiet=TRUE)
#' }


g2c_download_all_covs <- function(ISO, year, output_dir, rst_mask = NULL, quiet = FALSE, ...) {
  
  all_covs <- get_names_covariates()
  
  for (cov in all_covs) {
    message("Downloading: ", cov)
    tryCatch({
      g2c_download(
        covariate = cov,
        ISO = ISO,
        prj_year = year,
        ftp_srv = FALSE,       ## avoids FTP by default
        rst_mask = rst_mask,
        output_dir = output_dir,
        quiet = quiet,
        ...
      )
    }, error = function(e) {
      message("Failed: ", cov, " | ", e$message)
    })
  }
}





## 
## Function developed by Dr. Somnath Chaudhuri
#' Function to download all geospatial covariates for multiple year
#'
#' @rdname g2c_download_all
#' @param ISO A character, ISO code(s) of a specified country
#' @param years Numerical, the years of the covariate
#' @param output_dir Character vector containing the path to the directory for
#'        writing output files. Default is the temp directory.
#' @param rst_mask path to mask file (Default NULL)
#' @param quiet If TRUE, suppress status messages (if any), and the progress bar.
#' @param ...	Additional arguments:\cr
#'        \code{ftp_srv}: Boolean, If TRUE then WorldPop ftp server will
#'        be used otherwise http.\cr
#'        \code{method}: Method to be used for downloading files. Current download
#'        methods are "internal", "wininet" (Windows only) "libcurl",  "wget" and "curl",
#'        and there is a value "auto"
#'        \code{cells_touches}: 	logical. If TRUE, all cells touched by lines or ,
#'        polygons are affected, not just those on the line render path
#'         or whose center point is within the polygon.\cr
#'        \code{keep_tmp}: 	logical. If TRUE, do not remove ,
#'        any temporal files downloaded.\cr
#' @return A character, path to downloaded file
#' @export
#' @examples
#' \dontrun{
#' g2c_download_all(ISO=NULL,
#'                  years=2021,
#'                  rst_mask=NULL,
#'                  output_dir=NULL,
#'                  quiet=TRUE)
#' }

# Define Function
g2c_download_all <- function(ISO, years, output_dir, rst_mask = NULL, quiet = FALSE, ...) {
  all_covs <- get_names_covariates()
  
  for (cov in all_covs) {
    for (yr in years) {
      message("Downloading ", cov, " for year ", yr)
      tryCatch({
        g2c_download(
          covariate = cov,
          ISO = ISO,
          prj_year = yr,
          rst_mask = rst_mask,
          quiet = quiet,
          ftp_srv = FALSE,          ## if you want to avoid downloading from ftp
          output_dir = output_dir
        )
      }, error = function(e) {
        message("Failed: ", cov, " year ", yr, " | ", e$message)
      })
    }
  }
}
