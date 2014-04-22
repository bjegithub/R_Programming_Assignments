complete <- function(directory, id = 1:332) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files

	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used
        
	## Return a data frame of the form:
	## id nobs
	## 1  117
	## 2  1041
	## ...
	## where 'id' is the monitor ID number and 'nobs' is the
	## number of complete cases

	myFrame <- data.frame(id, 0)
	names(myFrame) <- c("id", "nobs")		

	for(i in 1:length(id)) {
		fname <- c(paste(directory, "\\", formatC(id[i], width=3, flag="0"), ".csv", sep=""))
		myData <- read.csv(fname, header=TRUE)
		completeCases = complete.cases(myData)
		myFrame[i,2] = sum(completeCases)
	}

	myFrame
}