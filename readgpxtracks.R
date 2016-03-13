readgpxtracks <- function(folder) {
	# read ».gpx« data from file(s) in a non-interactive manner. All files within the given directory will be read.
	wdold <- getwd()
	setwd(folder)
	files <- list.files(path = folder, pattern = "gpx")
	# check whether GPS data could be loaded
	if(length(files) > 1) {
		gpx <- lapply(files, FUN = readOGR, layer = "tracks", verbose = FALSE)
	}
	else {
		print("No ».gpx« files found. Check your source folder.")
	}
	# restore the working directory
	setwd(wdold)
	return(gpx)
}