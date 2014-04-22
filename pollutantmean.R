pollutantmean <- function(directory, pollutant, id = 1:332) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files

	## 'pollutant' is a character vector of length 1 indicating
	## the name of the pollutant for which we will calculate the
	## mean; either "sulfate" or "nitrate".

	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used

	## Return the mean of the pollutant across all monitors list
	## in the 'id' vector (ignoring NA values)

	fnames <- c(paste(directory, "\\", formatC(id, width=3, flag="0"), ".csv", sep=""))

	myData <- do.call("rbind", lapply(fnames, read.csv, header=TRUE))

	if(pollutant == "sulfate") {
		sulfates <- myData$sulfate
		bad <- is.na(sulfates )
		polMean <- mean(sulfates[!bad])
	}
	else if(pollutant == "nitrate") {
		nitrates <- myData$nitrate
		bad <- is.na(nitrates)
		polMean <- mean(nitrates[!bad])
	}

	polMean
}