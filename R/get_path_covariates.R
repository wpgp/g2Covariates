#' Functions to get a  path to the covaraite on the remote server
#'
#' @param ISO character. ISO of the country
#'        (see \href{https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3}{country codes}).
#'        Default one is NPL (Nepal)
#' @param covariate character. Name of the cvariate
#' @param prj_year Numeric. The year of the covaraites requred
#' @param gnames is logical. TRUE or FALSE: flag indicating whether to print the
#' names of the all coaraites. Default is \code{gnames} = FALSE.
#' @rdname get_path_covariates
#' @return A character, path to the covaraite on the remote server
#' @noRd
get_path_covariates <- function(ISO="NPL",
                                covariate=NULL,
                                prj_year=2024,
                                gnames=FALSE){

  '%!in%' <- function(x,y)!('%in%'(x,y))

  if (is.null(prj_year)) prj_year <- 2024

  covariates <- list(
    "Coastline" = file.path(ISO, "Coastline", "v1", "Distance", paste0(tolower(ISO),"_coastline_dst_100m_v1.tif")),
    "Elevation" = file.path(ISO, "Elevation", "MERIT_DEM", "v1", paste0(tolower(ISO),"_elevation_merit103_100m_v1.tif")),
    "Slope" = file.path(ISO, "Slope", "MERIT_DEM", "v1", paste0(tolower(ISO),"_slope_merit103_100m_v1.tif")),
    "Built_surface" = file.path(ISO, "BuiltSettlement", "v1", "Built_surface", paste0(tolower(ISO),"_built_S_GHS_U_wFGW_100m_v1_",prj_year,".tif")),
    "Built_volume" = file.path(ISO, "BuiltSettlement", "v1", "Built_volume", paste0(tolower(ISO),"_built_V_GHS_U_wFGW_100m_v1_",prj_year,".tif")),
    "Built_surface_NRES" = file.path(ISO, "BuiltSettlement", "v1", "Built_surface_NRES", paste0(tolower(ISO),"_built_S_NRES_GHS_U_wFGW_100m_v1_",prj_year,".tif")),
    "Built_volume_NRES" = file.path(ISO, "BuiltSettlement", "v1", "Built_volume_NRES", paste0(tolower(ISO),"_built_V_NRES_GHS_U_wFGW_100m_v1_",prj_year,".tif")),
    "built_S_dist" = ifelse(prj_year > 2030,
                            file.path(ISO, "BuiltSettlement", "v1", "Distance", paste0(tolower(ISO),"_built_S_dist_2030_GHS_MGW_100m_v1.tif")),
                            file.path(ISO, "BuiltSettlement", "v1", "Distance", paste0(tolower(ISO),"_built_S_dist_",prj_year,"_GHS_MGW_100m_v1.tif"))),
    "esalc_11_dst" = ifelse(prj_year > 2022,
                            file.path(ISO, "Land_Cover", "ESA", "v1", "2022",  paste0(tolower(ISO),"_esalc_11_dst_2022_100m_v1.tif")),
                            file.path(ISO, "Land_Cover", "ESA", "v1", prj_year,  paste0(tolower(ISO),"_esalc_11_dst_",prj_year,"_100m_v1.tif"))),
    "esalc_40_dst" = ifelse(prj_year > 2022,
                            file.path(ISO, "Land_Cover", "ESA", "v1", "2022",  paste0(tolower(ISO),"_esalc_40_dst_2022_100m_v1.tif")),
                            file.path(ISO, "Land_Cover", "ESA", "v1", prj_year,  paste0(tolower(ISO),"_esalc_40_dst_",prj_year,"_100m_v1.tif"))),
    "esalc_130_dst" = ifelse(prj_year > 2022,
                             file.path(ISO, "Land_Cover", "ESA", "v1", "2022",  paste0(tolower(ISO),"_esalc_130_dst_2022_100m_v1.tif")),
                             file.path(ISO, "Land_Cover", "ESA", "v1", prj_year,  paste0(tolower(ISO),"_esalc_130_dst_",prj_year,"_100m_v1.tif"))),
    "esalc_140_dst" = ifelse(prj_year > 2022,
                             file.path(ISO, "Land_Cover", "ESA", "v1", "2022",  paste0(tolower(ISO),"_esalc_140_dst_2022_100m_v1.tif")),
                             file.path(ISO, "Land_Cover", "ESA", "v1", prj_year,  paste0(tolower(ISO),"_esalc_140_dst_",prj_year,"_100m_v1.tif"))),
    "esalc_150_dst" = ifelse(prj_year > 2022,
                             file.path(ISO, "Land_Cover", "ESA", "v1", "2022",  paste0(tolower(ISO),"_esalc_150_dst_2022_100m_v1.tif")),
                             file.path(ISO, "Land_Cover", "ESA", "v1", prj_year,  paste0(tolower(ISO),"_esalc_150_dst_",prj_year,"_100m_v1.tif"))),
    "esalc_160_dst" = ifelse(prj_year > 2022,
                             file.path(ISO, "Land_Cover", "ESA", "v1", "2022",  paste0(tolower(ISO),"_esalc_160_dst_2022_100m_v1.tif")),
                             file.path(ISO, "Land_Cover", "ESA", "v1", prj_year,  paste0(tolower(ISO),"_esalc_160_dst_",prj_year,"_100m_v1.tif"))),
    "esalc_190_dst" = ifelse(prj_year > 2022,
                             file.path(ISO, "Land_Cover", "ESA", "v1", "2022",  paste0(tolower(ISO),"_esalc_190_dst_2022_100m_v1.tif")),
                             file.path(ISO, "Land_Cover", "ESA", "v1", prj_year,  paste0(tolower(ISO),"_esalc_190_dst_",prj_year,"_100m_v1.tif"))),
    "esalc_200_dst" = ifelse(prj_year > 2022,
                             file.path(ISO, "Land_Cover", "ESA", "v1", "2022",  paste0(tolower(ISO),"_esalc_200_dst_2022_100m_v1.tif")),
                             file.path(ISO, "Land_Cover", "ESA", "v1", prj_year,  paste0(tolower(ISO),"_esalc_200_dst_",prj_year,"_100m_v1.tif"))),
    "esalc_210_dst" = ifelse(prj_year > 2022,
                             file.path(ISO, "Land_Cover", "ESA", "v1", "2022",  paste0(tolower(ISO),"_esalc_210_dst_2022_100m_v1.tif")),
                             file.path(ISO, "Land_Cover", "ESA", "v1", prj_year,  paste0(tolower(ISO),"_esalc_210_dst_",prj_year,"_100m_v1.tif"))),
    "viirs_fvf" = ifelse(prj_year > 2023,
                     file.path(ISO, "VIIRS", "v1", "fvf",  paste0(tolower(ISO),"_viirs_fvf_2023_100m_v1.tif")),
                     file.path(ISO, "VIIRS", "v1", "fvf",  paste0(tolower(ISO),"_viirs_fvf_",prj_year,"_100m_v1.tif"))),
    "viirs_nvf" = ifelse(prj_year > 2023,
                         file.path(ISO, "VIIRS", "v1", "nvf",  paste0(tolower(ISO),"_viirs_nvf_2023_100m_v1.tif")),
                         file.path(ISO, "VIIRS", "v1", "nvf",  paste0(tolower(ISO),"_viirs_nvf_",prj_year,"_100m_v1.tif"))),
    "dist_inland_water" = file.path(ISO, "Inland_water", "esa_worldcover", "v1", "Dist", paste0(tolower(ISO),"_dist_inland_water_100m_esa_2021_v1.tif")),
    "pct_inland_water" = file.path(ISO, "Inland_water", "esa_worldcover", "v1", "pct", paste0(tolower(ISO),"_inland_water_pct_100m_v1.tif")),
    "Precipitation" = ifelse(prj_year > 2023,
                             file.path(ISO, "Climate", "Precipitation", "TerraClim", "v1", paste0(tolower(ISO),"_ppt_2023_yravg_tc_100m_v1.tif")),
                             file.path(ISO, "Climate", "Precipitation", "TerraClim", "v1", paste0(tolower(ISO),"_ppt_",prj_year,"_yravg_tc_100m_v1.tif"))),
    "Temperature" = ifelse(prj_year > 2023,
                           file.path(ISO, "Climate", "Temperature", "TerraLST", "v1", paste0(tolower(ISO),"_tavg_2023_tlst_100m_v1.tif")),
                           file.path(ISO, "Climate", "Temperature", "TerraLST", "v1", paste0(tolower(ISO),"_tavg_",prj_year,"_tlst_100m_v1.tif"))),
    "WDPA_cat1" = ifelse(prj_year > 2022,
                    file.path(ISO, "WDPA", "v1", "cat1", paste0(tolower(ISO),"_WDPA_pre2022_cat1_dist_100m_v1.tif")),
                    file.path(ISO, "WDPA", "v1", "cat1", paste0(tolower(ISO),"_WDPA_pre",prj_year,"_cat1_dist_100m_v1.tif"))),
    "WDPA_cat0" = ifelse(prj_year > 2022,
                         file.path(ISO, "WDPA", "v1", "cat0", paste0(tolower(ISO),"_WDPA_pre2022_cat0_dist_100m_v1.tif")),
                         file.path(ISO, "WDPA", "v1", "cat0", paste0(tolower(ISO),"_WDPA_pre",prj_year,"_cat0_dist_100m_v1.tif"))),
    "highway_dist_osm" = file.path(ISO, "OSM", "v1", paste0(tolower(ISO),"_highway_dist_osm_2023_100m_v1.tif")),
    "rd_intrs_dist_osm" = file.path(ISO, "OSM", "v1", paste0(tolower(ISO),"_rd_intrs_dist_osm_2023_100m_v1.tif")),
    "waterbodies_dist_osm" = file.path(ISO, "OSM", "v1", paste0(tolower(ISO),"_waterbodies_dist_osm_2023_100m_v1.tif")),
    "watermask" = file.path(ISO, "Inland_water", "esa_worldcover", "v1", "pct", paste0(tolower(ISO),"_inland_water_pct_100m_v1.tif")),
    "building_count_gl_T_0_5"= if (prj_year > 2023) file.path(ISO, "buildings", "gl_v2_5", "T_0_5", "count", paste0(tolower(ISO),"_buildings_count_2023_glv2_5_t0_5_C_100m_v1.tif"))
    else if (prj_year < 2016) file.path(ISO, "buildings", "gl_v2_5", "T_0_5", "count", paste0(tolower(ISO),"_buildings_count_2016_glv2_5_t0_5_C_100m_v1.tif"))
    else file.path(ISO, "buildings", "gl_v2_5", "T_0_5", "count", paste0(tolower(ISO),"_buildings_count_",prj_year,"_glv2_5_t0_5_C_100m_v1.tif")),
    "building_perimeter_gl_T_0_5"= if (prj_year > 2023) file.path(ISO, "buildings", "gl_v2_5", "T_0_5", "perimeter", paste0(tolower(ISO),"_buildings_perimeter_2023_glv2_5_t0_5_C_100m_v1.tif"))
    else if (prj_year < 2016) file.path(ISO, "buildings", "gl_v2_5", "T_0_5", "perimeter", paste0(tolower(ISO),"_buildings_perimeter_2016_glv2_5_t0_5_C_100m_v1.tif"))
    else file.path(ISO, "buildings", "gl_v2_5", "T_0_5", "perimeter", paste0(tolower(ISO),"_buildings_perimeter_",prj_year,"_glv2_5_t0_5_C_100m_v1.tif")),
    "building_surface_gl_T_0_5"= if (prj_year > 2023) file.path(ISO, "buildings", "gl_v2_5", "T_0_5", "surface", paste0(tolower(ISO),"_buildings_surface_2023_glv2_5_t0_5_C_100m_v1.tif"))
    else if (prj_year < 2016) file.path(ISO, "buildings", "gl_v2_5", "T_0_5", "surface", paste0(tolower(ISO),"_buildings_surface_2016_glv2_5_t0_5_C_100m_v1.tif"))
    else file.path(ISO, "buildings", "gl_v2_5", "T_0_5", "surface", paste0(tolower(ISO),"_buildings_surface_",prj_year,"_glv2_5_t0_5_C_100m_v1.tif")),
    "building_volume_gl_T_0_5"= if (prj_year > 2023) file.path(ISO, "buildings", "gl_v2_5", "T_0_5", "volume", paste0(tolower(ISO),"_buildings_volume_2023_glv2_5_t0_5_C_100m_v1.tif"))
    else if (prj_year < 2016) file.path(ISO, "buildings", "gl_v2_5", "T_0_5", "volume", paste0(tolower(ISO),"_buildings_volume_2016_glv2_5_t0_5_C_100m_v1.tif"))
    else file.path(ISO, "buildings", "gl_v2_5", "T_0_5", "volume", paste0(tolower(ISO),"_buildings_volume_",prj_year,"_glv2_5_t0_5_C_100m_v1.tif")),
    "building_varh_gl_T_0_5"= if (prj_year > 2023) file.path(ISO, "buildings", "gl_v2_5", "T_0_5", "varh", paste0(tolower(ISO),"_buildings_varh_2023_glv2_5_t0_5_C_100m_v1.tif"))
    else if (prj_year < 2016) file.path(ISO, "buildings", "gl_v2_5", "T_0_5", "varh", paste0(tolower(ISO),"_buildings_varh_2016_glv2_5_t0_5_C_100m_v1.tif"))
    else file.path(ISO, "buildings", "gl_v2_5", "T_0_5", "varh", paste0(tolower(ISO),"_buildings_varh_",prj_year,"_glv2_5_t0_5_C_100m_v1.tif")),
    "building_count_ms_PIB"= file.path(ISO, "buildings", "ms", "PIB", "v1_1", paste0(tolower(ISO),"_buildings_count_PIB_ms_100m_v1_1.tif")),
    "building_cv_area_ms_PIB"= file.path(ISO, "buildings", "ms", "PIB", "v1_1", paste0(tolower(ISO),"_buildings_cv_area_PIB_ms_100m_v1_1.tif")),
    "building_cv_length_ms_PIB"= file.path(ISO, "buildings", "ms", "PIB", "v1_1", paste0(tolower(ISO),"_buildings_cv_length_PIB_ms_100m_v1_1.tif")),
    "building_density_ms_PIB"= file.path(ISO, "buildings", "ms", "PIB", "v1_1", paste0(tolower(ISO),"_buildings_density_PIB_ms_100m_v1_1.tif")),
    "building_mean_area_ms_PIB"= file.path(ISO, "buildings", "ms", "PIB", "v1_1", paste0(tolower(ISO),"_buildings_mean_area_PIB_ms_100m_v1_1.tif")),
    "building_mean_length_ms_PIB"= file.path(ISO, "buildings", "ms", "PIB", "v1_1", paste0(tolower(ISO),"_buildings_mean_length_PIB_ms_100m_v1_1.tif")),
    "building_total_area_ms_PIB"= file.path(ISO, "buildings", "ms", "PIB", "v1_1", paste0(tolower(ISO),"_buildings_total_area_PIB_ms_100m_v1_1.tif")),
    "building_total_length_ms_PIB"= file.path(ISO, "buildings", "ms", "PIB", "v1_1", paste0(tolower(ISO),"_buildings_total_length_PIB_ms_100m_v1_1.tif")),
    "building_count_ms_BCB"= file.path(ISO, "buildings", "ms", "BCB", "v1_1", paste0(tolower(ISO),"_buildings_count_BCB_ms_100m_v1_1.tif")),
    "building_cv_area_ms_BCB"= file.path(ISO, "buildings", "ms", "BCB", "v1_1", paste0(tolower(ISO),"_buildings_cv_area_BCB_ms_100m_v1_1.tif")),
    "building_cv_length_ms_BCB"= file.path(ISO, "buildings", "ms", "BCB", "v1_1", paste0(tolower(ISO),"_buildings_cv_length_BCB_ms_100m_v1_1.tif")),
    "building_density_ms_BCB"= file.path(ISO, "buildings", "ms", "BCB", "v1_1", paste0(tolower(ISO),"_buildings_density_BCB_ms_100m_v1_1.tif")),
    "building_mean_area_ms_BCB"= file.path(ISO, "buildings", "ms", "BCB", "v1_1", paste0(tolower(ISO),"_buildings_mean_area_BCB_ms_100m_v1_1.tif")),
    "building_mean_length_ms_BCB"= file.path(ISO, "buildings", "ms", "BCB", "v1_1", paste0(tolower(ISO),"_buildings_mean_length_BCB_ms_100m_v1_1.tif")),
    "building_total_area_ms_BCB"= file.path(ISO, "buildings", "ms", "BCB", "v1_1", paste0(tolower(ISO),"_buildings_total_area_BCB_ms_100m_v1_1.tif")),
    "building_total_length_ms_BCB"= file.path(ISO, "buildings", "ms", "BCB", "v1_1", paste0(tolower(ISO),"_buildings_total_length_BCB_ms_100m_v1_1.tif"))
  )

  covariates_list <- names(covariates)


  if (gnames){

    return(covariates_list)

  }else{

    if (covariate %!in% covariates_list) {
      stop(paste0(covariate, " does not exist. Please check avalible covariates by using function [ get_names_covariates() ]"), call. = FALSE)
    }

    covariate_path <- covariates[[covariate]]
    return(covariate_path)
  }


}



#' Functions to get all names of the available covaraites
#'
#' @rdname get_names_covariates
#' @return List, List of the names of the available covaraites
#' @export
#' @examples
#' \dontrun{
#' get_names_covariates()
#' }
get_names_covariates <- function(){

  x <- get_path_covariates(ISO="NPL", covariate=NULL, prj_year=2024, gnames=TRUE)

  return(x)

}
