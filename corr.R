corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations

##	names(myFrame) <- c("id", "nobs")	

	filenames <- list.files(path = directory)	

	myVector <- vector("numeric", length=0)

	for(i in filenames) {
		fname <- c(paste(directory, "/", i, sep=""))
		myData <- read.csv(fname, header=TRUE)
		completeCases = complete.cases(myData)
##		print(completeCases)
		numComplete <- sum(completeCases)

##		print(myData[completeCases, ][1:4, ])

		if(numComplete > threshold) {
			goodData <- myData[completeCases, ][ ,1:4]

##			print("***")
##			print(goodData$sulfate)
##			print("***")
##			print(goodData$nitrate)
			correlation <- cor(goodData$sulfate, goodData$nitrate)
##			correlation <- cor(myData$sulfate, myData$nitrate)
##			print(correlation)
			myVector <- c(myVector, correlation)
		}

		
	}

	myVector
}	