#!/usr/local/bin/R

# This function (mpvalue) calculates mp-values for
# all treatments at the batch level.
# Inputs are as follows:
# dataset: 	a data frame where each row is a sample
# 		and each column is a variable.
# txlabels: the column name containing the treatment labels
# batchlabels:	the column name containing the batch labels
# datacols: the numbers of the columns containing the data to be
# 		used for calculating the mp-value
# negctrls: tx identifier(s) for the negative control treatments
# dirprefix:	directory to store results in (include / at the end).
# 			This directory must already exist.
# allbyall:	logical value; if TRUE, do all treatment-treatment
# 		comparisons; if FALSE, do only treatment-negative control
# 		comparisons
# outfile: 	filename to save all mp-values
# loadingsout:	logical value; if TRUE, output PCA loadings
# pcaout:	logical value; if TRUE, output PCA values
# gammaout:	logical value; if TRUE, output gamma distribution parameters,
# 		p-value for goodness of fit, and sample p-value according to
# 		the fit distribution
# 		NOTE: When gammaout=TRUE, warnings like these may be displayed:
# 		"In dgamma(x, shape, scale, log) : NaNs produced"
# 		These occur when the gamma distribution parameters are being
# 		derived and are generally not a problem.
# Example: -------------------------------------------------------------------------------------
# This example uses the well-known 'iris' dataset in R.
# > iris$batch <- rep(1, nrow(iris));
# > mpvalue(iris, txlabels="Species", batchlabels="batch", datacols=c(1:4), negctrls="setosa", dirprefix="irisresults/", allbyall=TRUE, outfile="iris_mpvalues.txt", loadingsout=TRUE, pcaout=TRUE, gammaout=TRUE);
#
# The results from iris_mpvalues.txt (spacing adjusted and decimals truncated):
# batch	tx		compared.to	Mahalanobis	mp.value 	rate 	shape	gamma.p.value	gamma.fit.p.value
# 1 		versicolor	setosa 	9.029		0 		12.51	3.155 9.563e-46 		0.878
# 1 		virginica	setosa 	7.359		0 		12.44	3.150 1.237e-36 		0.663
# 1 		virginica	versicolor 	3.018		0 		13.50	3.483 6.499e-15 		0.997
#
#-------------------------------------------------------------------------------------------------


library("plyr");

mpvalue <- function(dataset, txlabels, batchlabels, excludecols=c(), negctrls, dirprefix="/var/www/html/mpvalue/results/", outfile="all_mp-values.txt", allbyall=FALSE, loadingsout=FALSE, pcaout=FALSE, gammaout=FALSE) {
	# Standardizing variable names
	dataset$batch <- as.character(dataset[,names(dataset) %in% batchlabels]);
	dataset$tx <- as.character(dataset[,names(dataset) %in% txlabels]);
	excludecols <- c(excludecols, txlabels, batchlabels, "tx");
	# FUNCTION: newlabels
	# This function takes in labels and permutes them, but does
	# not allow the resulting permutation to be identical to the
	# original.
	newlabels <- function(x) {
		y <- sample(x);
		while (identical(y, x)) {
			y <- sample(x);
		}
		return(y);
	}  ## END FUNCTION newlabels

    # FUNCTION: checkdata
    # This function takes in data and checks to make sure that (1) it is a
    # matrix (not a vector) and (2) that the matrix has the minimum number
    # of columns and rows to be used for this analysis.
    checkdata <- function(x) {
            baddata <- FALSE;
			numtx <- length(table(dimnames(x)[[1]]));
            if (class(x) %in% "numeric") { baddata <- TRUE; }
            else if (nrow(x) < 3 | ncol(x) < 2  | numtx < 2) { baddata <- TRUE; }
            if (baddata) {
                    if (gammaout) {
                            y <- (matrix(c(unique(negctrls), 0, NA, NA, NA, NA, NA), 1, 7));
                    }
                    else {
                            y <- (matrix(c(unique(negctrls), 0, NA), 1, 3));
                    }
            }
            else {
                    y <- 1;
            }
            return(y);
    }

	# FUNCTION: txtomp
	# This function takes in a data subset representing replicates of a single
	# treatment and replicates of negative controls from the same batch.
	# It produces an mp-value based 
	txtomp <- function(txsubset, ncdf, negctrls) {
		# Print the status (which treatment is currently being evaluated)
		currbatch <- unique(txsubset$batch);
		currtx <- unique(txsubset$tx);
		if (!allbyall) {
			cat("running on treatment", currtx, "in batch", currbatch, fill=TRUE);
		}

		# Combine tx data with negative control data 
		newdf <- rbind(txsubset, ncdf);
		justdata <- newdf;
		
		# Remove non-numeric columns
		justdata <- justdata[,!(names(justdata) %in% excludecols)];
	 	readoutnames <- names(justdata);
		justdata <- apply(justdata, 2, as.numeric);
		dimnames(justdata)[[1]] <- as.character(newdf$tx);

		# Scale data in both dimensions and perform PCA.
		# Remove replicates that have constant values for all
		# variables
		justdata <- t(scale(t(justdata), center=TRUE, scale=TRUE));
		dimnames(justdata)[[1]] <- as.character(newdf$tx);
		dimnames(justdata)[[2]] <- readoutnames;
		
		# Remove rows with all NAs
		justdata <- justdata[which(rowSums(is.na(justdata)) < ncol(justdata)),];
		
		# Remove any analyses left with only 1 dimension of data (either only 1 row or
		# only 1 column)
		checkres <- checkdata(justdata);
        if (length(checkres) > 1) {
                return(checkres);
        }

        # Trim columns and rows with the most missing values until the
        # data frame contains no more missing values
        allnas <- sum(is.na(justdata));
        while (allnas > 0) {
                # Determine the percentage of rows OR columns that would be trimmed
                colnas <- colSums(is.na(justdata));
                rownas <- rowSums(is.na(justdata));
                colstotrim <- length(colnas[colnas %in% max(colnas)])/length(colnas);
                rowstotrim <- length(rownas[rownas %in% max(rownas)])/length(rownas);
                samplefreqs <- as.data.frame(table(dimnames(justdata)[[1]]));
                names(samplefreqs) <- c("tx", "origfreq");
                rowsatrisk <- dimnames(justdata)[[1]][which(rowSums(is.na(justdata)) == max(rowSums(is.na(justdata))))];
                riskfreqs <- as.data.frame(table(rowsatrisk));
                names(riskfreqs) <- c("tx", "riskremoval");
                risking <- merge(samplefreqs, riskfreqs, by="tx");
                risking$left <- risking$origfreq - risking$riskremoval;
                if (colstotrim < rowstotrim | max(risking$left) < 3) {
                        justdata <- justdata[,which(colSums(is.na(justdata)) < max(colSums(is.na(justdata))))];
                }
                else {
                        justdata <- justdata[which(rowSums(is.na(justdata)) < max(rowSums(is.na(justdata)))),];
                }
                allnas <- sum(is.na(justdata));
                checkres <- checkdata(justdata);
                       if (length(checkres) > 1) {
                       return(checkres);
                }
        }
        justdata <- justdata[,which(colSums(is.na(justdata)) == 0)];
        checkres <- checkdata(justdata);
        if (length(checkres) > 1) {
            return(checkres);
        }

		justdata <- justdata[order(dimnames(justdata)[[1]]),];
		txpca <- prcomp(justdata, center=TRUE, scale. = TRUE);

		# Extract PCA loadings
		loads <- txpca$rotation;

		# Determine the number of PCs explaining 90% of the
		# total variation and keep only those PCs
		cumpct <- cumsum((txpca$sdev)^2)/sum(txpca$sdev^2);
		regpct <- ((txpca$sdev)^2)/sum(txpca$sdev^2);
		numtokeep <- max(2,length(cumpct[cumpct<0.90]));
		txpca <- txpca$x[,1:numtokeep];

		# Format the loadings and weight the loadings for each
		# PC by the percentage of variation it explains.
		# Calculate the sum of variation explained by each 
		# variable across all PCs and add that as a column
		finalorder <- loads[,1:numtokeep];
                rownames(finalorder) <- colnames(justdata);

		colnames(finalorder) <- paste("PC", c(1:ncol(finalorder)), sep="");
		weighted <- sweep(finalorder, MARGIN=2, regpct[1:numtokeep], "*");
		weighted <- abs(weighted);
		weighted <- apply(weighted, 1, sum, na.rm=TRUE);
		finalorder <- cbind(finalorder, weighted);

		# Weight each PC by the percentage of variation it
		# explains.
		txpca <- sweep(txpca, MARGIN=2, regpct[1:numtokeep], "*");

		# Add treatment and batch ID annotation to the data
		txname <- dimnames(txpca)[[1]];
		ptxpca <- cbind(rep(currbatch, nrow(txpca)), rep(currtx, nrow(txpca)), txname, txpca);
		ptxpca <- as.matrix(ptxpca);

		# The following numbered steps calculate the Mahalanobis distance
		# 1. Separate treatments from controls
		controls <- as.matrix(txpca[txname %in% negctrls,]);
		treatments <- as.matrix(txpca[!(txname %in% negctrls),]);

		# 2. Calculate means for each variable in each group 
		if (ncol(controls) == 1 & nrow(controls) > 1) {
			controls <- t(controls);
		}
		if (ncol(treatments) == 1 & nrow(treatments) > 1) {
			treatments <- t(treatments);
		}
		vardiffs <- colMeans(treatments) - colMeans(controls);

		# 3. Calculate covariance matrix 
		# If there are insufficient numbers of replicates, this
		# calculation will fail, so this is checked with the first
		# if/else statement here.
		controls <- scale(controls, scale=FALSE);
		treatments <- scale(treatments, scale=FALSE);
		if (nrow(treatments) == 1 && nrow(controls) < 3) {
			mahal <- 0;
			signf <- 1;
		}
		else if (nrow(controls) == 1 && nrow(treatments) < 3) {
			mahal <- 0;
			signf <- 1;
		}
		else {
			if (nrow(treatments) > 1 && nrow(controls) > 1) {
				weightcov <- (nrow(controls)/nrow(txpca))*cov(controls) + (nrow(treatments)/nrow(txpca))*cov(treatments);
			}
			else if (nrow(controls) > 1) {
				weightcov <- cov(controls);
			}
			else {
				weightcov <- cov(treatments);
			}
			weightcov <- solve(weightcov);
			vardiffs <- as.matrix(vardiffs);
			
			# 4. Calculate the Mahalanobis distance using the group mean
			# differences and the covariance matrix.
			mahal <- sqrt(t(vardiffs) %*% (weightcov %*% vardiffs));
			mahal <- as.numeric(mahal);
			# Permute the labels and re-calculate Mahalanobis distances 1000 times.
			permscores <- as.numeric();
			length(permscores) <- 1000;
			for (i in 1:1000) {
				ptxname <- newlabels(dimnames(txpca)[[1]]);

				pcontrols <- as.matrix(txpca[ptxname %in% negctrls,]);
				ptreatments <- as.matrix(txpca[!(ptxname %in% negctrls),]);
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
					pweightcov <- (nrow(pcontrols)/nrow(txpca))*cov(pcontrols) + (nrow(ptreatments)/nrow(txpca))*cov(ptreatments);
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
		}

		# Gamma distribution significance
		if (gammaout) {
			library(MASS);
			est <- fitdistr(permscores,"gamma");
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
		if (!(unique(txsubset$tx) %in% negctrls)) {
			if (pcaout) {
				ptxpcanames <- c('batch', 'negctrl', 'tx', paste('PC', 1:(ncol(ptxpca)-3), sep=""));
			 	ptxpca <- rbind(ptxpcanames, ptxpca);	
				write.table(ptxpca, paste(c(dirprefix, "pcaout/", currtx, "_vs_", unique(negctrls), "_", currbatch, ".txt"), collapse=""), row.names=FALSE, col.names=FALSE, quote=FALSE, sep="\t");
			}
			if (loadingsout) {
				nn <- c('readout', dimnames(finalorder)[[2]]);
				finalorder <- cbind(dimnames(finalorder)[[1]], finalorder);
				finalorder <- rbind(nn, finalorder);
				write.table(finalorder, paste(c(dirprefix, "loadings/loadings_", currtx, "_vs_", unique(negctrls), "_", currbatch, ".txt"), collapse=""), row.names=FALSE, col.names=FALSE, quote=FALSE, sep="\t");
			}
			if (gammaout) {
				return(matrix(c(unique(negctrls), mahal, signf, estrate, estshape, gsignf, fitpval), 1, 7));
			}
			else {
				return(matrix(c(unique(negctrls), mahal, signf), 1, 3));
			}
		}
	} ## END FUNCTION txtomp



	# FUNCTION: batchtotx
	# This function takes in a section of the dataset 
	# corresponding to a single batch, identifies negative
	# control replicates, breaks the batch-specific dataset
	# up by treatments, then sends those to get mp-values.
	batchtotx <- function(fulldata) {
		if (allbyall) {
			alltx <- unique(fulldata$tx);
			currbatch <- unique(fulldata$batch);
			for (i in 1:(length(alltx)-1)) {
				negctrls <- alltx[i];
				numcomps <- length(alltx) - i;
				cat("running all", numcomps, "comparisons to treatment", negctrls, "in batch", currbatch, fill=TRUE);
				ncdf <- fulldata[fulldata$tx %in% negctrls,];
				tempfulldata <- fulldata[fulldata$tx %in% alltx[i+1:length(alltx)],];
				tempmpvalues <- dlply(tempfulldata, .(tx), txtomp, ncdf=ncdf, negctrls=negctrls);
				tempmpvalues <- ldply(tempmpvalues, data.frame);
 				if (i %in% "1") {
					allmpvalues <- tempmpvalues;
				}
				else {
					allmpvalues <- rbind(allmpvalues, tempmpvalues);
				}
			}
		}
		else {
			ncdf <- fulldata[fulldata$tx %in% negctrls,];
			fulldata <- fulldata[!fulldata$tx %in% negctrls,];
			allmpvalues <- dlply(fulldata, .(tx), txtomp, ncdf=ncdf, negctrls=negctrls);
			allmpvalues <- ldply(allmpvalues, data.frame);
		}
		if (gammaout) {
			names(allmpvalues) <- c("tx", "compared to", "Mahalanobis", "mp-value", "rate", "shape", "gamma p-value", "gamma fit p-value");
		}
		else {
			names(allmpvalues) <- c("tx", "compared to", "Mahalanobis", "mp-value"); 
		}
		return(allmpvalues);
	}  ## END FUNCTION batchtotx

	# Print all batches:
	cat("All batches:", fill=TRUE);
	cat(unique(dataset$batch), fill=TRUE);

	if (!allbyall) {
		# Limit the dataset to only those batches with at least 
		# 1 negative control
		goodbatches <- unique(dataset$batch[dataset$tx %in% negctrls]);
		badbatches <- unique(dataset$batch)[!unique(dataset$batch) %in% goodbatches];
		if (length(badbatches) < 1) {
			cat("All batches contain at least 1 negative control.", fill=TRUE);
		}
		else {
			cat("The following batches do not contain negative controls and will be removed:", badbatches, fill=TRUE);
		}
		dataset <- dataset[dataset$batch %in% goodbatches,];
	}

	# This breaks up the dataset by batch and sends them to be
	# further broken up by treatment
	finalmpvalues <- dlply(dataset, .(batch), batchtotx);
	finalmpvalues <- ldply(finalmpvalues, data.frame);
	cat("Writing output file\n");
	write.table(finalmpvalues, file=outfile, append=FALSE, sep="\t", row.names=FALSE, col.names=TRUE, quote=FALSE);

}  ## END FUNCTION mpvalue
