#' Filter features
#'
#' Filters columns (features) of Cell Profiler data to exclude any columns
#' with NA/NaNs, low variance or are highly correlated.
#'
#' @return A dataframe with the filtered columns removed. If out_cols is set
#'     to TRUE, a list of 2, where the 1st element is the filtered dataframe
#'     and the 2nd element is a vector of the filtered column names.
#'
#'
#' @param df Dataframe of Cell Profiler data to be filtered.
#' @param out_cols Single logical indicating whether the filtered column names
#'     should be output as well.
#' @param NA_cutoff Optional argument. Columns with a total number of NAs or
#'     NaNs greater than the NA_cutoff number will be removed.
#' @param freqCut Passed to \code{caret::nearZeroVar}. The cutoff for the ratio
#'     of the most common value to the second most common value.
#' @param uniqueCut Passed to \code{caret::nearZeroVar}. The cutoff for the
#'     percentage of distinct values out of the number of total samples.
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom magrittr %>%
#'

#' @describeIn filter_NA Filters columns with any NA/NaN's or a total number
#'     above a certain number if 'NA_cutoff' given.
#' @export
filter_NA <- function(df, NA_cutoff, out_cols = FALSE){

  # check inputs
  if (! missing(NA_cutoff)) {
    assert_that(is.numeric(NA_cutoff), length(NA_cutoff) == 1,
                msg = "Check that 'NA_cutoff' is a single number")
  }

  assert_that(is.logical(out_cols), length(out_cols) == 1,
              msg = "Check 'out_cols' is a single logical")


  # calculate total NAs for each column
  col_NAs <- apply(df, 2, function(x) sum(is.na(x)))

  if (sum(col_NAs >= 1) == 0) {

    print("There were no columns with >0 NA's")

  } else if (missing(NA_cutoff)){

    filter_cols <- names(col_NAs)[col_NAs >= 1]
    cat("There were", length(filter_cols), "column(s) removed")

    # keep columns with 0 NA values only
    df <- df[,col_NAs == 0]

  } else {

    filter_cols <- names(col_NAs)[col_NAs > NA_cutoff]
    cat("There were", length(filter_cols), "column(s) removed")


    # filter columns
    df <- df[,col_NAs <= NA_cutoff]

  }

  if (out_cols) {

    return(list(df, filter_cols))

  } else {

    return(df)

  }

}

#' @describeIn filter_NA Filters columns with low variance (e.g. if entire
#'     column consists of the same value) using the \code{caret::nearZeroVar}
#'     function. See
#'     \href{https://cran.r-project.org/web/packages/caret/caret.pdf}{caret
#'     documentation} for details on this function.
#' @export
filter_lowVar <- function(df, freqCut = 95/5, uniqueCut = 10,
                          out_cols = FALSE) {

  # check inputs
  assert_that(is.logical(out_cols), length(out_cols) == 1,
              msg = "Check 'out_cols' is a single logical")

  assert_that(is.numeric(freqCut), length(freqCut) == 1,
              msg = "Check 'freqCut' is a single number")

  assert_that(is.numeric(uniqueCut), length(uniqueCut) == 1,
              msg = "Check 'uniqueCut' is a single number")


  # indicies of columns to filter
  nums <- sapply(df, is.numeric)

  to_filter <- caret::nearZeroVar(df[ , nums], freqCut, uniqueCut)

  if (length(to_filter) == 0) {

    cat("There were no columns with near zero variance")

  } else {

    filter_cols <- colnames(df)[to_filter]
    cat("There were", length(filter_cols), "column(s) removed")

    # filter df
    df <- df[,-to_filter]

  }

  if (out_cols) {

    return(list(df, filter_cols))

  } else {

    return(df)

  }

}

#' @describeIn filter_NA Calculates the Pearson correlation matrix for the
#'     'Median' columns then filters columns that are highly correlated to each
#'     other using \code{caret::findCorrelation}. Note that you should run
#'     \code{filter_lowVar} on your data before filtering for high correlations.
#' @export
filter_cor <- function(df, cor_cutoff, out_cols = FALSE) {

  # check inputs
  assert_that(is.numeric(cor_cutoff), length(cor_cutoff) == 1,
              msg = "Check that 'cor_cutoff' is a single number")

  assert_that(is.logical(out_cols), length(out_cols) == 1,
              msg = "Check 'out_cols' is a single logical")


  # convert to numeric mat
  mat <- as.matrix(
    df %>%
      dplyr::select(dplyr::starts_with(c("Median", "StDev")))
  )

  # calculate correlation matrix
  cor_mat <- cor(mat)

  # find indicies of the columns to remove
  remove_cols <- caret::findCorrelation(cor_mat, cutoff = cor_cutoff)

  # remove these columns
  filt_mat <- mat[,-remove_cols]

  # add the count and annot columns back
  ann_cols <- df %>%
    dplyr::select(- dplyr::starts_with(c("Median", "StDev")))

  final_df <- cbind(ann_cols, as.data.frame(filt_mat))


  # inform user which columns have been removed
  filter_cols <- colnames(mat)[remove_cols]
  cat("There were", length(filter_cols), "column(s) removed")

  if (out_cols) {

    return(list(final_df, filter_cols))

  } else {

    return(final_df)

  }

}

