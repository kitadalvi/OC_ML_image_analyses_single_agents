### plot_plls

#' Plot powerloglog slope
#' 
#' Plot the density of the image quality metric, powerloglog slope for a 
#' specified channel, for each plate, in the current graphics device.
#' 
#' @param df Dataframe containing a 'ImageQuality_PowerLogLogSlope' column
#' @param channel Name of the channel to be plotted, exactly as written in
#'     Cell Profiler output, as string.
#' @param scale Single logical. If TRUE the scale of all plots will be uniform.
#'     The x-axis limits will be set to the min and max of the powerloglog slope 
#'     for all plates.
#'     
#' @importFrom assertthat assert_that
#' @import ggplot2
#' @importFrom magrittr %>% 
#' 
#' @export
plot_plls <- function(df, channel, scale) {
  
  columnName <- paste("ImageQuality_PowerLogLogSlope_Orig",
                      channel, sep = "")
  
  # check inputs
  assert_that(columnName %in% colnames(df),
              msg = "Make sure channel exists and is spelt correctly")
  
  assert_that(is.logical(scale), length(scale) == 1,
              msg = "Check 'scale' is a single logical")
  
  # make plots
  if (scale) {
    
    min_plls <- min(df[[columnName]], na.rm = TRUE)
    max_plls <- max(df[[columnName]], na.rm = TRUE)
    
    plots <- df %>%
      dplyr::group_by(Metadata_Barcode) %>%
      dplyr::do(plots = ggplot(data = ., aes_string(x = columnName)) + 
                  geom_density(fill = "#7570b3", alpha = 0.5) +
                  coord_cartesian(xlim = c(min_plls,max_plls)) +
                  labs(title = .$Metadata_Barcode[1]) +
                  theme_minimal()
      )
    
  } else {
    
    plots <- df %>%
      dplyr::group_by(Metadata_Barcode) %>%
      dplyr::do(plots = ggplot(data = ., aes_string(x = columnName)) + 
                  geom_density(fill = "#7570b3", alpha = 0.5) +
                  labs(title = .$Metadata_Barcode[1]) +
                  theme_minimal()
      )
    
  }
  
  # print plots
  for (i in 1:length(unique(df$Metadata_Barcode))){
    
    print(plots$plots[[i]])
    
  }
  
}



### FilterImages

#' Filter images
#' 
#' Filter poor quality images (using the powerloglog slope feature). 
#' \code{plotFiltered} plots the number of images filtered for each plate.
#' 
#' @return 
#' \itemize{
#'     \item \code{filterImages} returns the dataframe with poor quality images
#'     (rows) removed.
#'     \item \code{plotFiltered} creates a bar plot in the current graphics
#'     device.
#' }
#' 
#' @param df Dataframe of raw CellProfiler data, where each row is one image
#'     NOT one well.
#' @param num_IQR Number of IQR's below the 25% quantile to place cutoff 
#'     threshold.
#' @param filtered_df The plate ID column from the filtered (for poor quality 
#'     images) dataframe of CellProfiler data, as a dataframe or vector. 
#'     E.g. if the plate ID column from the filtered dataframe 
#'     \code{filtered_df} is called 'Metadata_Barcode', 
#'     \code{filtered_df[,'Metadata_Barcode']} OR 
#'     \code{filtered_df[['Metadata_Barcode']]} can be used.
#' @param num_images The number of images taken per well.
#' @param annot Optional argument. The plate ID column from the annotation 
#'     dataframe, as a vector or dataframe. E.g. if the plate ID column from 
#'     the \code{annot} dataframe is called 'VCFG_Compound_Plate_ID', 
#'     \code{annot[,'VCFG_Compound_Plate_ID']} OR
#'     \code{annot[['VCFG_Compound_Plate_ID']]} can be used. If given, the 
#'     number of filtered images (rows) will be calculated as: 
#'     (number of used wells) * \code{num_images} - (number of images remaining
#'     after filtering). If not given, it will be assumed that all wells were 
#'     used.
#' @param num_wells Number of wells per plate.     
#' 
#' 
#' @importFrom assertthat assert_that
#' @importFrom magrittr %>%
#' @import ggplot2
#' 

#' @describeIn filterImages Filter images (rows) with a powerloglog slope 
#'     value below the threshold ('num_IQR' * IQR below the 25th percentile) in 
#'     ANY channel.
#' @export
filterImages <- function(df, num_IQR) {
  
  # check inputs
  assert_that(is.numeric(num_IQR), length(num_IQR) == 1, 
              msg = "Check 'num_IQR' is a single number")
  
  ## get column names of the powerloglog of all channels
  plls_columns <- colnames(
    df %>%
      dplyr::select(dplyr::starts_with("ImageQuality_PowerLogLogSlope_Orig"))
  )
  
  ## check powerloglog column(s) exist 
  assert_that(length(plls_columns) > 0,
              msg = "Check that there is at least one 'ImageQuality_PowerLogLogSlope_Orig-' 
              column in 'df'")
  
  
  
  # thresholding function
  threshold_fun <- function(group_df){
    
    # obtain only columns of interest
    reduced_df <- group_df[,plls_columns]
    
    # create empty threshold vector
    threshold <- vector(mode = "numeric", length = length(plls_columns))
    
    
    # fill vector with threshold for each channel (i.e. powerloglog column)
    for (i in 1:length(plls_columns)){
      
      threshold[i] <- 
        quantile(reduced_df[,i][[1]], 0.25, na.rm = TRUE) - 
        ( num_IQR * IQR(reduced_df[,i][[1]], na.rm = TRUE) )
      
    }
    
    # for each row in grouped df, check all elements are greater
    # than threshold.
    # Result is vector of TRUEs and FALSEs the length of nrow.
    
    filter_rows <- apply(reduced_df, 1, function(x){
      sum(x > threshold) == length(plls_columns)}
    )
    
    # subset using vector above
    filtered_df <- group_df[filter_rows,]
    
    return(filtered_df)
    
  }
  
  # perform function on grouped df
  
  return(
    df %>%
      dplyr::group_by(Metadata_Barcode) %>%
      dplyr::do(threshold_fun(.)) %>%
      dplyr::ungroup()
  )
  
}


#' @describeIn filterImages Creates bar graph of the number of images filtered 
#'     for each plate.
#' @export
plotFiltered <- function(filtered_df, num_images, annot, num_wells) {
  
  # check inputs
  assert_that(is.numeric(num_images), length(num_images) == 1,
              msg = "Check 'num_images' is single number")
  
  assert_that(is.numeric(num_wells), length(num_wells) == 1,
              msg = "Check 'num_wells' is single number")
  
  if (! is.null(dim(filtered_df))) {
    
    assert_that(dim(filtered_df)[2] == 1, 
                msg = "Check 'filtered_df' only contains one column")
    
  }
  
  if (! missing(annot)) {
    
    if (! is.null(dim(annot))) {
      
      assert_that(dim(annot)[2] == 1, 
                  msg = "Check 'annot' only contains one column")
      
    }
    
  }
  
  
  
  # calculate number of used wells per plate
  if (! missing(annot)) {
    wells <- table(annot)
  } else {
    wells <- num_wells
  }
  
  
  # calculate the number of rows per plate in the filtered data
  images_perPlate <- table(filtered_df)
  
  # calculate the number of filtered images
  num_filtered <- data.frame((wells * num_images) - images_perPlate)
  
  nfilt_columns <- colnames(num_filtered)
  
  # make plot
  ggplot(num_filtered, aes_string(x = nfilt_columns[1], y = nfilt_columns[2])) +
    geom_bar(stat = "identity") +
    labs(title = "Number of images filtered", y = "Number of images",
         x = "Plate") + 
    coord_flip() +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
}













