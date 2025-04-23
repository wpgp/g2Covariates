#' Function to get datatype
#'
#' @param dtype A character, datatype
#' @rdname getDType
#' @noRd
#'
getDType <- function(dtype) {
  if (!(dtype %in% c('Byte', 'UInt16', 'Int16', 'UInt32','Int32', 'Float32', 'Float64', 'CInt16', 'CInt32', 'CFloat32', 'CFloat64'))) {
    return ('FLT4S')
  } else if  (dtype == 'Byte') {
    return('INT1U')
  } else if  (dtype == 'UInt16') {
    return('INT2U')
  } else if  (dtype == 'Int16' | dtype == 'CInt16') {
    return('INT2S')
  } else if  (dtype == 'UInt32') {
    return('INT4U')
  } else if  (dtype == 'Int32' | dtype == 'CInt32') {
    return('INT4S')
  } else if  (dtype == 'Float32' | dtype == 'CFloat32' ) {
    return('FLT4S')
  } else if  (dtype == 'Float64' | dtype == 'CFloat64' )  {
    return('FLT8S')
  } else {
    return('FLT4S')
  }
}

#' Function to get information of NoData Value and datatype pf the raster
#'
#' @param f is a path to a raster file
#' @param verbose If FALSE, suppress status messages (if any), and the progress bar.
#' @rdname rst_info
#' @importFrom terra describe
#' @noRd
rst_info <- function(f, verbose=FALSE){

  describeX_json <- terra::describe(f)
  nindX <- grep('\\bNoData Value\\b', describeX_json)

  NAflag <- as.numeric(strsplit( describeX_json[ nindX[1] ], "=" )[[1]][2])

  nindX <- grep('\\bType\\b', describeX_json)
  strsplited <- as.character(strsplit( describeX_json[ nindX[1] ], "," )[[1]][1])
  strsplited <- as.character(strsplit( strsplited, "=" )[[1]])
  datatype <- strsplited[length(strsplited)]
  datatype_GDAL <- datatype
  datatype <- getDType(datatype)


  df <- data.frame(NAflag=NAflag,
                     Type=datatype,
                     Type_GDAL=datatype_GDAL)

  if (verbose){
    print(df, quote = TRUE, row.names = FALSE)
  }

  return(df)

}



