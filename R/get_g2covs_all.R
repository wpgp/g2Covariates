#========================================================================
# https://github.com/wpgp/g2Covariates    GITHUB Repository

## Install packages
install.packages("devtools")  # Install devtools if not already installed

library(devtools)
install_github("wpgp/g2Covariates")

# NOTE: install.packages(c("terra", "jsonlite")) if not installed
#========================================================================

# Initialise library
library(g2Covariates)
library(sf)

# Set paths
dir_path <- "add your directory path"
output_path <- paste0(dir_path, "/add your output storage path")

input_path <- paste0(dir_path, "/add your boundary shapefile path")
# shp_file <- paste0(input_path, "/TAM_10_12.shp")
#========================================================================

## Package overview (Example using Thailand - THA)

# Get names of all WorldPop covariates
get_names_covariates()

# Get names of neighbouring countries
nb_countries <- get_global_nb()
nb_countries[["THA"]]

#========================================================================

## One COVARIATE for a particular YEAR with WorldPop BOUNDARY Shapefiles

# Night Light Intensity
g2c_download(covariate = "viirs_nvf", 
             ISO = "THA", 
             prj_year = 2018,  # Need to add (currently one year at a time)
             ftp_srv = FALSE,       ## if you want to avoid downloading from ftp
             rst_mask = NULL,
             output_dir= output_path
)

# Building Count
g2c_download(covariate = "building_count_gl_T_0_5", 
             ISO = "THA", 
             prj_year = 2018,  # 2015, 2020, 2030
             ftp_srv = FALSE,       ## if you want to avoid downloading from ftp
             rst_mask = NULL,
             output_dir= output_path
)

#========================================================================

## One COVARIATE for a particular YEAR with USER OWN Shapefiles

#1 nonthaburi_boundary.shp

#2 nonthaburi_adm2.shp

#3 pilotEA_10_12_withData.shp   #### NOTE issues in gaps between polygons

# Specify shapefile
shp_file <- paste0(input_path, "/add your shapefile")
shp <- st_read(shp_file)
plot(shp$geometry)

# Elevation
g2c_download(covariate = "Elevation", 
             ISO = "THA", 
             prj_year = 2018,  
             ftp_srv = FALSE,                  ## if you want to avoid downloading from ftp
             rst_mask = shp_file,              # Single polygon   Multi-polygon
             output_dir= output_path
)

#========================================================================

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


# Function call
g2c_download_years(
  covariate = "viirs_nvf",   # viirs_nvf
  ISO = "THA",
  years = 2015:2024,
  output_dir = output_path
)

#========================================================================

## All COVARIATES for a ONE YEAR with USER OWN Shapefiles

# Get the list of all covariates
all_covs <- get_names_covariates()

# Function call
for (cov in all_covs) {
  message("Downloading: ", cov)
  tryCatch({
    g2c_download(
      covariate = cov,
      ISO = "THA",
      prj_year = 2020,
      ftp_srv = FALSE,              ## if you want to avoid downloading from ftp
      rst_mask = shp_file, 
      output_dir = output_path
      # quiet = FALSE
    )
  }, error = function(e) {
    message("Failed: ", cov, " | ", e$message)
  })
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


# Function call
g2c_download_all(
  ISO = "THA",
  years = 2015:2016,
  output_dir = output_path
)
