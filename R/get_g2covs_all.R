

## One COVARIATE for ALL AVAILABLE YEARS

# Define function
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

## All COVARIATES for a ONE YEAR with USER OWN Shapefiles

g2c_download_all_covs <- function(ISO, year, output_dir, rst_mask = NULL, quiet = FALSE) {
  
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
        quiet = quiet
      )
    }, error = function(e) {
      message("Failed: ", cov, " | ", e$message)
    })
  }
}





#========================================================================

## All COVARIATES for ALL AVAILABLE YEARS

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
