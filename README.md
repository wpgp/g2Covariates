[![DOI](https://zenodo.org/badge/971251461.svg)](https://doi.org/10.5281/zenodo.15267942)

# g2Covariates

g2c_download is an R package that provides easy access to various WorldPop geospatial covariates.

## Installation

To install the package from GitHub, first ensure you have the `devtools` package installed:

```r
install.packages("devtools")  # Install devtools if not already installed
devtools::install_github("wpgp/g2Covariates")
```

## Usage

Once installed, load the package in your R session:

```r
library(g2Covariates)
```

### Functions avaliable
- `g2c_download(covariate, ISO, prj_year, rst_mask, output_dir)`
  
Downaloding covariate for "ISO" country for "prj_year" year using "rst_mask" as mask.

### Function to print all covariates avalible 

 *get_names_covariates()*

 ```
 [1] "Coastline"            "Elevation"            "Slope"                "Built_surface"        "Built_volume"         "Built_surface_NRES"  
 [7] "Built_volume_NRES"    "built_S_dist"         "esalc_11_dst"         "esalc_40_dst"         "esalc_130_dst"        "esalc_140_dst"       
[13] "esalc_150_dst"        "esalc_160_dst"        "esalc_190_dst"        "esalc_200_dst"        "esalc_210_dst"        "viirs"               
[19] "dist_inland_water"    "Precipitation"        "Temperature"          "WDPA"                 "highway_dist_osm"     "rd_intrs_dist_osm"   
[25] "waterbodies_dist_osm" "watermask"            "building_count_gl"    "building_count_ms"       
```

### Function to get neighbor countries 

 ```
nb_countries <- get_global_nb()
nb_countries[["NPL"]]
[1] "CHN" "IND"
 ```

## Dependencies

This package requires the following R packages:
- `terra`
- `jsonlite`

Ensure they are installed before using the package:

```r
install.packages(c("terra", "jsonlite"))
```

## Contributing

Contributions are welcome! If you'd like to improve this package, please:
1. Fork the repository.
2. Make changes in a new branch.
3. Submit a pull request.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Citation

If you use g2Covariates in your research, please cite:

*Bondarenko M., Priyatikanto R., Zhang W., McKeen T., Nosatiuk B. and Tejedor-Garavito N. (2025). g2Covariates is an R package that provides easy access to various WorldPop geospatial covariates. (1.0.0) [Computer software]. GitHub. https://github.com/wpgp/g2Covariates*


```bibtex
@software{g2Covariates,
	title        = {g2Covariates is an R package that provides easy access to various WorldPop geospatial covariates.},
	author       = {Bondarenko M., Priyatikanto R., Zhang W., McKeen T., Nosatiuk B. and Tejedor-Garavito N..},
	year         = 2025,
	month        = 4,
	publisher    = {GitHub},
	doi          = {10.5281/zenodo.15267943},
	url          = {https://github.com/wpgp/g2Covariates},
	version      = {1.0.0}
}
```

## Getting Help

- Issues & Support: https://github.com/wpgp/g2Covariates/issues
- WorldPop SDI: https://sdi.worldpop.org
