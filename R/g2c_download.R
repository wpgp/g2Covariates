#' Functions to download various geospatial covariates datasets from WorldPop,
#'
#' @rdname g2c_download
#' @param covariate A character, name of the covariate to download
#' @param ISO A character, ISO code(s) of a specified country
#' @param prj_year Numerical, the year of the covariate
#' @param output_dir Character vector containing the path to the directory for
#'        writing output files. Default is the temp directory.
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
#' g2c_download(covariate=NULL,
#'              ISO=NULL,
#'              prj_year=2024,
#'              rst_mask=NULL,
#'              output_dir=NULL,
#'              quiet=TRUE)
#' }
g2c_download <- function(covariate=NULL,
                         ISO=NULL,
                         prj_year=2024,
                         rst_mask=NULL,
                         output_dir=NULL,
                         quiet=FALSE,
                         ...){


  '%!in%' <- function(x,y)!('%in%'(x,y))

  # covariate <- "built_S_dist"
  # ISO <- "MLI"
  # prj_year=2015
  # output_dir <- "E:\\WorldPop\\R_Script\\Covariates_Downloading\\output"
  # ftp_srv=FALSE
  # method="auto"
  # quiet=FALSE
  # rst_mask <- "E:\\WorldPop\\test_QGIS\\RWA_input\\mastergrid\\rwa_level1_100m_2015_2030.tif"
  # rst_mask <- "E:\\WorldPop\\R_Script\\Covariates_Downloading\\data\\MLI_National_boundary.shp"

  if (!file.exists(output_dir)){
    stop(paste0("Directory does not exist [ ", basename(output_dir), " ]. "), call. = FALSE)
  }

  if (prj_year < 2015){
    stop(paste0( "Year should be not less then 2015  "), call. = FALSE)
  }

  if (is.null(covariate)){
    stop(paste0("Please specify covariate you would like to download"), call. = FALSE)
  }

  if (is.null(ISO)){
    stop(paste0("Please specify ISO of the country"), call. = FALSE)
  }

  global_nb <- get_global_nb()


  if (ISO %!in%  names(global_nb)){
    stop(paste0( "ISO ", ISO , "is not in the list of avalible covairets. Please use a corect ISO of the country"), call. = FALSE)
  }


  if (!is.null(rst_mask) && !file.exists(rst_mask)){
    stop(paste0("File does not exist [ ", basename(rst_mask), " ]. "), call. = FALSE)
  }



  if (covariate %!in% get_names_covariates()) {
    stop(paste0(covariate, " does not exist. Please check avalible covariates by using function [ covariates_aval() ]"), call. = FALSE)
  }

  rst_mask_f_extention <- getExtension(rst_mask)

  if (rst_mask_f_extention %!in% c("gpkg", "tif", "shp")){
    stop(paste0(basename(rst_mask), " is ot vector or raster file. Only 'gpkg', 'tif' or 'shp' are accepted."), call. = FALSE)
  }

  if (!is.null(rst_mask) ){
    if (rst_mask_f_extention == "tif"){
      x_rst <- terra::rast(rst_mask)
      stopifnot(terra::hasValues(x_rst))
    }else{
      x_rst <- terra::vect(rst_mask)
    }
  }

  ### checking  arguments fo
  args <- list(...)

  if ("method" %in% names(args)){
    method <- args[["method"]]
  }else{
    method <- "auto"
  }

  if ("ftp_srv" %in% names(args)){
    ftp_srv <- args[["ftp_srv"]]
  }else{
    ftp_srv <- TRUE
  }

  if ("cells_touches" %in% names(args)){
    cells_touches <- args[["cells_touches"]]
  }else{
    cells_touches <- FALSE
  }

  if ("keep_tmp" %in% names(args)){
    keep_tmp <- args[["keep_tmp"]]
  }else{
    keep_tmp <- FALSE
  }


  if (is.null(rst_mask)){

    file_remote <- get_path_covariates(ISO, covariate, prj_year)
    dest_file <- file.path(output_dir, paste0(get_fn_without_ext(file_remote),".tif"))

    if (!file.exists(dest_file)){
      if (!quiet) print(paste0("Downloading ", get_fn_without_ext(file_remote)))
      download_file(f_remote=file_remote,
                         dest_file=dest_file,
                         ftp_srv=ftp_srv,
                         method=method,
                         quiet=quiet)
    }

  }else{

    nb_countries <- global_nb[[ISO]]
    countries_download <- c(ISO, nb_countries)
    org_name <- get_fn_without_ext(get_path_covariates(ISO, covariate, prj_year))

    fs_donwloaded <- list()


    for (country in countries_download){

      file_remote <- get_path_covariates(country, covariate, prj_year)
      dest_file <- file.path(output_dir, paste0(get_fn_without_ext(file_remote),"_tmp.tif"))
      fs_donwloaded <- append(fs_donwloaded, dest_file, after = length(fs_donwloaded))

      if (!file.exists(dest_file)){
        if (!quiet) print(paste0("Downloading ", get_fn_without_ext(file_remote)))
        download_file(f_remote=file_remote,
                           dest_file=dest_file,
                           ftp_srv=ftp_srv,
                           method=method,
                           quiet=quiet)
      }
    }

    vrt_file_db <- tempfile(pattern = "file_", tmpdir = output_dir, fileext = ".vrt")
    fs_donwloaded <- unlist(fs_donwloaded)
    if(!file.exists(vrt_file_db)) {
      if (!quiet) print(paste0("Start creating VFT file ", get_fn_without_ext(file_remote)))
      invisible(terra::vrt(fs_donwloaded, vrt_file_db))
    }

    f_out <- file.path(output_dir, paste0(org_name,".tif"))

    if (!file.exists(f_out)){

      rst_info_df <- rst_info(file.path(output_dir, paste0(org_name,"_tmp.tif")))

      NAflag=rst_info_df$NAflag
      datatype=rst_info_df$Type


      rst_crop_tmp <- terra::crop( terra::rast(vrt_file_db), terra::ext(x_rst) )

      if (rst_mask_f_extention %in% c("shp", "gpkg")){

        x_rst$IDUNQ <- seq.int(0, terra::nrow(x_rst), 1)
        mask_file_rasterized <- file.path(output_dir, paste0("mask_",org_name,".tif"))

        mask_rasterized <- terra::rasterize(x_rst,
                                            rst_crop_tmp,
                                            field="IDUNQ",
                                            background=8888,
                                            touches=cells_touches,
                                            update=FALSE,
                                            cover=FALSE,
                                            overwrite=FALSE,
                                            wopt=list(gdal=c("COMPRESS=LZW", "BIGTIFF=YES", "BLOCKXSIZE=512", "BLOCKYSIZE=512", "TILED=YES" )))

        terra::NAflag(mask_rasterized) <- 8888

        terra::writeRaster(mask_rasterized,
                           mask_file_rasterized,
                           overwrite=TRUE,
                           gdal=c("COMPRESS=LZW","BLOCKXSIZE=512", "BLOCKYSIZE=512", "TILED=YES", "BIGTIFF=YES"),
                           datatype="INT4S",
                           NAflag=-9999)

        rst_crop_tmp[is.na(mask_rasterized)] <- NA

        rm(mask_rasterized)
        rm(x_rst)
        invisible(gc())

      }else{

        rst_crop_tmp[is.na(x_rst)] <- NA

        rm(x_rst)
        invisible(gc())

      }


      terra::NAflag(rst_crop_tmp) <- as.numeric(NAflag)

      terra::writeRaster(rst_crop_tmp,
                         f_out,
                         overwrite=TRUE,
                         gdal=c("COMPRESS=LZW","BLOCKXSIZE=512", "BLOCKYSIZE=512", "TILED=YES", "BIGTIFF=YES"),
                         datatype=datatype,
                         NAflag=as.numeric(NAflag))

      if (!quiet) print(paste0("Completed downloading covariate ", get_fn_without_ext(f_out)))
    }

    if (!keep_tmp){

      for (f in fs_donwloaded){
        if (file.exists(f)) invisible(file.remove(f))
      }

    }
    if (file.exists(vrt_file_db)) invisible(file.remove(vrt_file_db))

  }


  return(invisible(TRUE))

}
