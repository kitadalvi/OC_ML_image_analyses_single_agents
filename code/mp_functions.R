### calc_mp

# Example: -------------------------------------------------------------------------------------
# This example uses the well-known 'iris' dataset in R.
# > iris$batch <- rep(1, nrow(iris));
# > mpvalue(iris, txlabels="Species", batchlabels="batch", datacols=c(1:4),
#           negctrls="setosa", dirprefix="irisresults/", allbyall=TRUE,
#           outfile="iris_mpvalues.txt", loadingsout=TRUE, pcaout=TRUE,
#           gammaout=TRUE);
#
# The results from iris_mpvalues.txt (spacing adjusted and decimals truncated):
# batch	tx		compared.to	Mahalanobis	mp.value 	rate 	shape	gamma.p.value	gamma.fit.p.value
# 1 		versicolor	setosa 	9.029		0 		12.51	3.155 9.563e-46 		0.878
# 1 		virginica	setosa 	7.359		0 		12.44	3.150 1.237e-36 		0.663
# 1 		virginica	versicolor 	3.018		0 		13.50	3.483 6.499e-15 		0.997
#
#-------------------------------------------------------------------------------------------------

#' Calculate mp-values
#'
#' Calculate multidimensional perterbation value (mp-value) as described in
#' \href{https://journals.sagepub.com/doi/10.1177/1087057112469257}{Hutz et al.}.
#' Briefly, the mp-value can be used to determine whether any two treatments
#' differ from each other and is similar in spirit to the p-value. This
#' function was written by Hutz *et al.*.
#'
#' Minor changes to the code and additional comments added are identified by a
#' 'LL' next to the change.
#'
#'
#' @param dataset Dataframe containing all data where each row is a sample and
#'     each columns is a feature/variable.
#' @param txlabels Name of the column containing the treatment labels, as
#'     string. Each desired treatment group should have a unique label, within
#'     each batch (e.g. a treatment can have the same label in each batch as
#'     long as within one batch, the label uniquely identifies the treatment).
#' @param batchlabels Name of the column containing the batch labels, as string
#' @param datacols Vector of the column indicies that contain the
#'     feature/variable data to be used for calculating the mp-value.
#' @param negctrls The name of the negative controls, in the 'txlabels' column,
#'     as string.
#' @param allbyall Logical indicating whether to do all treatment-treatment
#'     comparisons (TRUE) or only treatment-control comparisons.
#' @param dirprefix Path to the directory to store results in, as string. Do
#'     not include the trailing '/' at the end. This path will be created if it
#'     does not already exist.
#' @param outfile Name of the file to save all the mp-values, as string.
#' @param loadingsout Logical indicating whether the PCA loadings (eigenvectors)
#'     should be output.
#' @param pcaout Logical indicating whether to output PCA values.
#' @param gammaout Logical indicating whether to output gamma distribution
#'     parameters, p-value of goodness of fit and sample p-value according to
#'     the fit distribution.
#'     NOTE: When gammaout=TRUE, warnings like these may be displayed:
# 		 "In dgamma(x, shape, scale, log) : NaNs produced"
# 		 These occur when the gamma distribution parameters are being
# 		 derived and are generally not a problem.
#'
#' @importFrom assertthat assert_that
#'

#' @export
calc_mp <- function(dataset, txlabels, batchlabels, datacols, negctrls,
                    allbyall=FALSE, dirprefix, outfile="all_mp-values.txt",
                    loadingsout=FALSE, pcaout=FALSE, gammaout=FALSE) {

  # check inputs
  assert_that(is.data.frame(dataset),
              msg = "Check that 'dataset' is a dataframe") #LL

  assert_that(is.numeric(datacols),
              msg = "Check that 'datacols' is a numeric vector")


  ## character inputs
  character_args <- list(txlabels = txlabels, batchlabels = batchlabels,
                         negctrls = negctrls, dirprefix = dirprefix,
                         outfile = outfile)

  for (i in 1:length(character_args)){

    msg <- paste("Check that '", names(character_args)[i],
                 "' is a single string",
                 sep = "")

    assert_that(is.character(character_args[[i]]),
                length(character_args[[i]]) == 1,
                msg = msg)

  }

  ## create path
  if (! dir.exists(dirprefix)) {

    dir.create_out <- dir.create(dirprefix, recursive = TRUE)
    ## check dir.create worked
    assert_that(dir.create_out,
                msg = "Could not create your 'dirprefix', check path given")

  }


  ## logical inputs
  logical_args <- list(allbyall = allbyall, loadingsout = loadingsout,
                       pcaout = pcaout, gammaout = gammaout)

  for (i in 1:length(logical_args)){

    msg <- paste("Check that '", names(logical_args)[i],
                 "' is a single logical", sep = "")

    assert_that(is.logical(logical_args[[i]]), length(logical_args[[i]]) == 1,
                msg = msg)

  }

  # create dirs
  if (pcaout) {

    dir_pca <- paste(dirprefix, "/pcaout", sep = "")

    if (! dir.exists(dir_pca)) {

      dir.create_out <- dir.create(dir_pca, recursive = TRUE)
      ## check dir.create worked
      assert_that(dir.create_out,
                  msg = "Could not create '/pcaout' folder in your 'dirprefix'.
                  Check path given")

    }

  }

  if (loadingsout) {

    dir_loadings <- paste(dirprefix, "/loadings", sep = "")

    if (! dir.exists(dir_loadings)) {

      dir.create_out <- dir.create(dir_loadings, recursive = TRUE)
      ## check dir.create worked
      assert_that(dir.create_out,
                  msg = "Could not create '/loadings' folder in your 'dirprefix'.
                  Check path given")

    }

  }

  ###### Actual function starts here  ######

  # Standardizing variable names
  dataset$batch <- as.character(dataset[[batchlabels]]);
  dataset$tx <- as.character(dataset[[txlabels]]);

  # Print all batches:
  cat("All batches:", fill=TRUE);
  cat(unique(dataset$batch), fill=TRUE);

  if (!allbyall) {
    # Limit the dataset to only those batches with at least
    # 1 negative control
    goodbatches <- unique(dataset$batch[dataset$tx == negctrls]);
    # badbatches are batches NOT in goodbatches
    badbatches <- unique(dataset$batch)[!unique(dataset$batch) %in% goodbatches];

    if (length(badbatches) <= 0) {
      cat("All batches contain at least 1 negative control.", fill=TRUE);
    } else {
      cat("The following batches do not contain negative controls and will be removed:",
          badbatches, fill=TRUE);
    }

    dataset <- dataset[dataset$batch %in% goodbatches,];
  }


  # This breaks up the dataset by batch and sends them to be
  # further broken up by treatment
  finalmpvalues <- plyr::ddply(dataset, plyr::.(batch), .batchtotx,
                               allbyall = allbyall, negctrls = negctrls,
                               datacols = datacols, gammaout = gammaout,
                               pcaout = pcaout, loadingsout = loadingsout,
                               dirprefix = dirprefix);

  cat("Writing output file\n");

  outfile <- paste(dirprefix, outfile, sep = "/")

  write.table(finalmpvalues, file=outfile, append=FALSE, sep="\t",
              row.names=FALSE, col.names=TRUE, quote=FALSE);

  }  ## END FUNCTION mpvalue




# FUNCTION: .batchtotx
# This function takes in a section of the dataset
# corresponding to a single batch, identifies negative
# control replicates, breaks the batch-specific dataset
# up by treatments, then sends those to get mp-values.

#' @keywords internal
.batchtotx <- function(fulldata, allbyall, negctrls, datacols, gammaout,
                       pcaout, loadingsout, dirprefix) {

  if (allbyall) {
    # here we want to compare all pairwise tx's/conditions

    alltx <- unique(fulldata$tx);
    currbatch <- fulldata$batch[1];


    for (i in 1:(length(alltx)-1)) {
      negctrls <- alltx[i];
      #LL take the first tx as 'negctrls', as we are comparing all pairwise tx's
      numcomps <- length(alltx) - i;
      #LL compare the tx above to all subsequent unique tx'es, thus the formula
      # for number of comparisons

      cat("running all", numcomps, "comparisons to treatment", negctrls,
          "in batch", currbatch, fill=TRUE);

      ncdf <- fulldata[fulldata$tx == negctrls,]; #LL
      #LL 'negative control df'. Subset only the rows where tx is the ith tx
      # i.e. our 'negctrls'

      comparisontx_data <- fulldata[fulldata$tx %in% alltx[i+1:length(alltx)],];
      #LL data of all the tx's to compare the to 'ith' tx ('negctrl') to
      # i.e. all unique tx's after the ith tx.

      tempmpvalues <- plyr::ddply(comparisontx_data, plyr::.(tx), .txtomp,
                                  ncdf=ncdf, negctrls=negctrls);
      #LL input: df, output: list. Group by the tx column, perform the function
      # .txtomp, with the 'ncdf' and 'negctrls' arguments as given


      if (i == 1) { #LL
        allmpvalues <- tempmpvalues;
        #LL for the first loop - store the mp values
      } else {
        allmpvalues <- rbind(allmpvalues, tempmpvalues);
        #LL growing df of successive mp-values for other comparisons
      }
    } # end of for loop

  } else { #LL if NOT allbyall

    print("onto .batchtotx")

    ncdf <- fulldata[fulldata$tx == negctrls,];
    #LL negative control df, 'negctrls' here is the one input by the user

    txdf <- fulldata[!fulldata$tx == negctrls,];
    #LL all NOT negctrls rows

    allmpvalues <- plyr::ddply(txdf, plyr::.(tx), .txtomp, ncdf=ncdf,
                               negctrls=negctrls, allbyall = allbyall,
                               datacols = datacols, gammaout = gammaout,
                               pcaout = pcaout, loadingsout = loadingsout,
                               dirprefix = dirprefix);
    #LL group by tx, apply function .txtomp to every group. Arguments to .txtomp
    # ncdf and negctrls as given

  }

  if (gammaout) {
    colnames(allmpvalues) <- c("tx", "compared to", "Mahalanobis", "mp-value",
                               "rate", "shape", "gamma p-value",
                               "gamma fit p-value");
  } else {
    colnames(allmpvalues) <- c("tx", "compared to", "Mahalanobis", "mp-value");
    #LL remaining columns will be named 'NA' (as there are more columns in the
    # df than the length of the given vector given)
  }
  return(allmpvalues);
}  ## END FUNCTION .batchtotx



# FUNCTION: .txtomp
# This function takes in a data subset representing replicates of a single
# treatment and replicates of negative controls from the same batch.
# It produces an mp-value based

#' @keywords internal
.txtomp <- function(txsubset, ncdf, negctrls, allbyall, datacols, gammaout,
                    pcaout, loadingsout, dirprefix) {

  # Print the status (which treatment is currently being evaluated)
  currbatch <- txsubset$batch[1];
  currtx <- txsubset$tx[1];


  if (!allbyall) {
    cat("running on treatment", currtx, "in batch", currbatch, fill=TRUE);
  }

  # Combine tx data with negative control data
  newdf <- rbind(txsubset, ncdf);

  # Remove non-numeric columns
  justdata <- newdf[,datacols]; #LL

  justdata_colnames <- colnames(justdata); #LL
  justdata <- apply(justdata, 2, as.numeric);
  #LL change all columns to numeric data type AND apply will convert to matrix

  # Scale data in both dimensions and perform PCA.
  # Remove replicates that have constant values for all
  # variables
  justdata <- t(scale(t(justdata), center=TRUE, scale=TRUE));
  #LL transform - column: sample, row: feature
  #   scales samples, subtracts column means and divides by column sd
  #   transforms back - column: feature, row: sample

  rownames(justdata) <- as.character(newdf$tx);
  #colnames(justdata) <- justdata_colnames;

  # Remove rows with all NAs
  justdata <- justdata[rowSums(is.na(justdata)) < ncol(justdata),];

  # Remove any analyses left with only 1 dimension of data (either only 1 row or
  # only 1 column)
  checkres <- .checkdata(justdata, gammaout = gammaout, negctrls = negctrls);
  if (length(checkres) > 1) {
    return(checkres);
  }

  # Trim columns and rows with the most missing values until the
  # data frame contains no more missing values

  allnas <- sum(is.na(justdata));

  while (allnas > 0) {

    colnas <- colSums(is.na(justdata));
    rownas <- rowSums(is.na(justdata));

    prop_colMax <- length(colnas[colnas == max(colnas)])/length(colnas);
    prop_rowMax <- length(rownas[rownas == max(rownas)])/length(rownas);
    #LL proportion of rows/cols with the max NA number
    #LL The total number of NAs across all cols & rows is the same.
    # If the NAs are concentrated in a small number of rows/columns,
    # the prop of max NA cols/rows will be LOW.

    samplefreqs <- as.data.frame(table(rownames(justdata))); #LL
    names(samplefreqs) <- c("tx", "origfreq");
    #LL number of rows (samples) per tx

    rowsatrisk <- rownames(justdata)[rownas == max(rownas)];
    riskfreqs <- as.data.frame(table(rowsatrisk));
    names(riskfreqs) <- c("tx", "riskremoval");
    #LL number of rows (samples) that will be removed, per tx

    risking <- merge(samplefreqs, riskfreqs, by="tx");
    risking$left <- risking$origfreq - risking$riskremoval;
    #LL number of rows (samples) left after removing the max NA rows

    if (prop_colMax < prop_rowMax | max(risking$left) <= 2) {
      #LL if the NAs are concentrated in columns OR the maximum number of
      # rows (samples) per tx left is <= 2, removed the max NA COLUMNS
      justdata <- justdata[,colnas < max(colnas)];
      #LL remove the max NA columns
    } else {
      justdata <- justdata[rownas < max(rownas),];
    }

    allnas <- sum(is.na(justdata));

    checkres <- .checkdata(justdata, gammaout = gammaout, negctrls = negctrls);
    if (length(checkres) > 1) {
      return(checkres);
    }
  }

  justdata <- justdata[,colSums(is.na(justdata)) == 0];
  #LL removed all columns containing a NA

  checkres <- .checkdata(justdata, gammaout = gammaout, negctrls = negctrls);
  if (length(checkres) > 1) {
    return(checkres);
  }
  #LL if data incorrect size, return single row

  justdata <- justdata[order(rownames(justdata)),];


  txpca <- prcomp(justdata, center=TRUE, scale. = TRUE);
  # LL perform PCA, returns list with: sd of the PC's square roots of the
  # eigenvalues, matrix of variable loadings (columns are eigenvectors)


  # Extract PCA loadings
  eigenvectors <- txpca$rotation;

  # Determine the number of PCs explaining 90% of the
  # total variation and keep only those PCs
  cumpct <- cumsum((txpca$sdev)^2)/sum(txpca$sdev^2);
  #LL vector of the % of variance explained, cumulative

  regpct <- ((txpca$sdev)^2)/sum(txpca$sdev^2);
  #LL vector of the % of variance explained by each PC

  numtokeep <- max(2,length(cumpct[cumpct<=0.90])); #LL
  #LL keep only the dimensions explaining total 90% of variance

  txpca <- txpca$x[,1:numtokeep];
  #LL value of the rotated data, each column is a PC, each row an observation
  # (sample)


  # Format the eigenvectors and weight the eigenvectors for each
  # PC by the percentage of variation it explains.
  # Calculate the sum of variation explained by each
  # variable across all PCs and add that as a column
  finalorder <- eigenvectors[,1:numtokeep];

  weighted <- sweep(finalorder, MARGIN=2, regpct[1:numtokeep], "*");
  #LL multiply the eigenvectors by the proportion of variance explained

  weighted <- abs(weighted);
  weighted <- apply(weighted, 1, sum, na.rm=TRUE);
  #LL add weighted eigenvectors, across each feature (row)
  finalorder <- cbind(finalorder, weighted);
  #LL output is eigenvectors and a weighted column

  # Weight each PC (x) by the percentage of variation it
  # explains.
  txpca <- sweep(txpca, MARGIN=2, regpct[1:numtokeep], "*");

  # Add treatment and batch ID annotation to the data
  txname <- rownames(justdata); #LL
  #rownames(txpca) <- txname #LL not needed
  ptxpca <- cbind(rep(currbatch, nrow(txpca)), rep(currtx, nrow(txpca)), txname,
                  txpca);
  ptxpca <- as.matrix(ptxpca);



  # The following numbered steps calculate the Mahalanobis distance
  # 1. Separate treatments from controls
  controls <- as.matrix(txpca[txname == negctrls,]); #LL
  treatments <- as.matrix(txpca[!(txname == negctrls),]); #LL
  #LL matrix of rotated data. Output of subsetting transposed if only 1 sample
  # in each group.

  # 2. Calculate means for each variable in each group
  if (ncol(controls) == 1 & nrow(controls) > 1) {
    controls <- t(controls);
  }
  if (ncol(treatments) == 1 & nrow(treatments) > 1) {
    treatments <- t(treatments);
  }
  #LL check and transpose due to subsetting (see above)

  vardiffs <- colMeans(treatments) - colMeans(controls);
  #LL PC means

  # 3. Calculate covariance matrix
  # If there are insufficient numbers of replicates, this
  # calculation will fail, so this is checked with the first
  # if/else statement here.
  controls <- scale(controls, scale=FALSE);
  treatments <- scale(treatments, scale=FALSE);
  #LL scale rotated data by subtracting the column means

  if (nrow(treatments) == 1 && nrow(controls) <= 2) {
    mahal <- 0;
    signf <- 1;
  } else if (nrow(controls) == 1 && nrow(treatments) <= 2) {
    mahal <- 0;
    signf <- 1;
  } else {
    if (nrow(treatments) > 1 && nrow(controls) > 1) {
      weightcov <- (nrow(controls)/nrow(txpca))*cov(controls) +
        (nrow(treatments)/nrow(txpca))*cov(treatments);
      #LL weight covariance by proportion of samples
    }
    else if (nrow(controls) > 1) {
      weightcov <- cov(controls);
    }
    else {
      weightcov <- cov(treatments);
    }

    weightcov <- solve(weightcov);
    #vardiffs <- as.matrix(vardiffs);

    # 4. Calculate the Mahalanobis distance using the group mean
    # differences and the covariance matrix.
    mahal <- sqrt(t(vardiffs) %*% (weightcov %*% vardiffs));
    mahal <- as.numeric(mahal);


    # Permute the labels and re-calculate Mahalanobis distances 1000 times.
    permscores <- vector(mode = "numeric", length = 1000); #LL

    for (i in 1:1000) {
      ptxname <- newlabels(rownames(txpca));

      pcontrols <- as.matrix(txpca[ptxname == negctrls,]);
      ptreatments <- as.matrix(txpca[!(ptxname == negctrls),]);

      if (ncol(pcontrols) == 1 & nrow(pcontrols) > 1) {
        pcontrols <- t(pcontrols);
      }
      if (ncol(ptreatments) == 1 & nrow(ptreatments) > 1) {
        ptreatments <- t(ptreatments);
      }

      pvardiffs <- colMeans(ptreatments) - colMeans(pcontrols);

      pcontrols <- scale(pcontrols, scale=FALSE);
      ptreatments <- scale(ptreatments, scale=FALSE);
      if (nrow(ptreatments) > 1 && nrow(pcontrols) > 1) {
        pweightcov <- (nrow(pcontrols)/nrow(txpca))*cov(pcontrols) +
          (nrow(ptreatments)/nrow(txpca))*cov(ptreatments);
      }
      else if (nrow(pcontrols) > 1){
        pweightcov <- cov(pcontrols);
      }
      else {
        pweightcov <- cov(ptreatments);
      }

      pweightcov <- solve(pweightcov);

      pvardiffs <- as.matrix(pvardiffs);
      pmahal <- sqrt(t(pvardiffs) %*% (pweightcov %*% pvardiffs));
      permscores[i] <- pmahal;
    }

    # Calculate the mp-value by determining the number of permutations
    # that produce a Mahalanobis distance higher than the original
    signf <- length(permscores[permscores>=mahal])/length(permscores);
  } #LL end else

  # Gamma distribution significance
  if (gammaout) {
    est <- MASS::fitdistr(permscores,"gamma");
    estrate <- est$estimate[names(est$estimate) %in% "rate"];
    estshape <- est$estimate[names(est$estimate) %in% "shape"];
    gsignf <- pgamma(mahal, shape=estshape, rate=estrate, lower.tail=FALSE);
    estgamma <- rgamma(1000, shape=estshape, rate=estrate);
    fitpval <- wilcox.test(permscores, estgamma, alternative="two.sided");
    fitpval <- fitpval$p.value;
  }

  # For all treatments, output the PCA coordinates and the PCA loadings
  # and send back the Mahalanobis distance and mp-values to the parent
  # function.

  if (!(unique(txsubset$tx) == negctrls)) {
    if (pcaout) {
      ptxpcanames <- c('batch', 'negctrl', 'tx', paste('PC', 1:(ncol(ptxpca)-3),
                                                       sep=""));
      ptxpca <- rbind(ptxpcanames, ptxpca);
      write.table(ptxpca, paste(c(dirprefix, "/pcaout/", currtx, "_vs_",
                                  unique(negctrls), "_", currbatch, ".txt"),
                                collapse=""),
                  row.names=FALSE, col.names=FALSE, quote=FALSE, sep="\t");
    }
    if (loadingsout) {
      nn <- c('readout', colnames(finalorder));
      finalorder <- cbind(rownames(finalorder), finalorder);
      finalorder <- rbind(nn, finalorder);
      write.table(finalorder, paste(c(dirprefix, "/loadings/loadings_", currtx,
                                      "_vs_", unique(negctrls), "_", currbatch,
                                      ".txt"), collapse=""),
                  row.names=FALSE, col.names=FALSE, quote=FALSE, sep="\t");
    }
    if (gammaout) {
      return(matrix(c(unique(negctrls), mahal, signf, estrate, estshape, gsignf,
                      fitpval), 1, 7));
    }
    else {
      return(matrix(c(unique(negctrls), mahal, signf), 1, 3));
    }
  }
} ## END FUNCTION .txtomp


# FUNCTION: .checkdata
# This function takes in data and checks to make sure that (1) it is a
# matrix (not a vector) and (2) that the matrix has the minimum number
# of columns and rows to be used for this analysis.

#' @keywords internal
.checkdata <- function(x, gammaout, negctrls) {
  baddata <- FALSE;
  numtx <- length(unique(rownames(x))); #LL

  if (! is(x, c("matrix"))) { #LL
    baddata <- TRUE;
  } else if (nrow(x) < 3 | ncol(x) < 2  | numtx < 2) {
    #LL req at least 3 samples (nrow), at least 2 features, and at least 2
    # conditison (incl negcontrol)
    baddata <- TRUE;
  }

  if (baddata) {
    if (gammaout) {
      y <- (matrix(c(unique(negctrls), 0, NA, NA, NA, NA, NA),
                   1, 7));
    }
    else {
      y <- (matrix(c(unique(negctrls), 0, NA),
                   1, 3));
    }
  }
  else {
    y <- 1;
  }
  return(y);
}


# FUNCTION: newlabels
# This function takes in labels and permutes them, but does
# not allow the resulting permutation to be identical to the
# original.

#' @keywords internal
newlabels <- function(x) {
  y <- sample(x);

  while (identical(y, x)) {
    y <- sample(x);
  }

  return(y);
}  ## END FUNCTION newlabels


### clean_mp

#' Clean MP data
#'
#' If each unique treatment and concentration was treated as a separate
#' treatment in \code{calc_mp}, the "tx" column of the output would contain the
#' treatment and concentration concatenated together. This function takes the
#' dataframe output by \code{calc_mp} and outputs the dataframe with two extra
#' columns; 'conc' which contains just the concentration and 'compound', which
#' contains just the compound name.
#'
#' Note that this function assumes that each concatenated string has the
#' treatment first and then the concentration. It also assumes that the
#' concentration is numerical (does not contain any non-numerical characters)
#' and does not contain the 'sep' symbol.
#'
#'
#' @param mp_df Dataframe output from \code{calc_mp}, containing mp-values and
#'     treatment and batch details.
#' @param sep Symbol separating the treatment and concentration in the
#'     concatenated column, as string.
#' @param tx_col Name of the column containing the concatenated treatment and
#'     concentration data.
#'
#' @importFrom assertthat assert_that
#'
#'
#' @export
clean_mp <- function(mp_df, sep, tx_col = "tx") {

  # check inputs
  assert_that(is.character(sep), length(sep) == 1,
              msg = "Check that 'sep' is a single string")

  assert_that(is.character(tx_col), length(tx_col) == 1,
              msg = "Check that 'tx_col' is a single string")

  assert_that(sum(colnames(mp_df) == tx_col) == 1,
              msg = "Check there is one 'tx_col' in 'mp_df'")


  # extract concentration
  conc <- sapply(strsplit(mp_df[[tx_col]], sep), function(x) x[length(x)])

  mp_df$conc <- as.numeric(conc)

  # extract compound
  compound <- sapply(strsplit(mp_df[[tx_col]], sep),
                     function(x) paste(x[1:(length(x) - 1)], collapse = sep))

  mp_df$compound <- as.character(compound)

  # re-order columns
  mp_ncols <- ncol(mp_df)

  mp_df <- mp_df[ ,c(1,2, mp_ncols, (mp_ncols-1), 3:(mp_ncols-2))]

  return(mp_df)

}


### plot_mp

#' Plot MP values
#'
#' Creates density plots of the MP value or Mahalanobis distance.
#'
#' @param mp_df Dataframe containing the MP values.
#' @param mp_col Name of the column containing the MP values, as string.
#' @param Mdist_col Name of the column containing the Mahalanobis distances,
#'     as string.
#'
#'
#' @importFrom assertthat assert_that
#' @import ggplot2
#'
#' @describeIn plot_mp Creates a density plot of the MP values, in the
#'     current graphics device.
#' @export
plot_mp <- function(mp_df, mp_col = "mp.value") {

  # check inputs
  assert_that(is.character(mp_col), length(mp_col) == 1,
              msg = "Check 'mp_col' is a single string")

  assert_that(sum(colnames(mp_df) == mp_col) == 1,
              msg = "Check there is one 'mp_col' column in 'mp_df'")


  ggplot(mp_df, aes_string(x = mp_col)) +
    geom_density(fill = "#7570b3", alpha = 0.5) +
    labs(title = "MP value distribution for all comparisons", y = "Density",
         x = "MP value") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

}


#' @describeIn plot_mp Creates a density plot of the Mahalanobis distances,
#'     in the current graphics device.
#' @export
plot_Mdist <- function(mp_df, Mdist_col = "Mahalanobis") {

  # check inputs
  assert_that(is.character(Mdist_col), length(Mdist_col) == 1,
              msg = "Check 'mp_col' is a single string")

  assert_that(sum(colnames(mp_df) == Mdist_col) == 1,
              msg = "Check there is one 'Mdist_col' column in 'mp_df'")


  ggplot(mp_df, aes_string(x = Mdist_col)) +
    geom_density(fill = "#7570b3", alpha = 0.5) +
    labs(title = "Mahalanobis distance distribution for all comparisons",
         y = "Density", x = "Mahalanobis distance") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

}













