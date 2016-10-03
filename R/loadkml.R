loadkml <- function(folder = NULL, layer = "track_points") {
	# read Marble ».kml« data from file(s) in a non-interactive manner. All files
	# within the given directory will be read.
	# kml1 <- readOGR(, layer = "")
	if (is.null(folder)) {
		folder <- readline(prompt = "Please select a folder! ")
	}
	files <- list.files(path = folder, full.names = TRUE, pattern = ".kml")
	# check whether GPS data could be loaded
	if (length(files) >= 1) {
		# get infos via ogrListLayers(object/"filename") and ogrInfo()
		kml <- lapply(files, FUN = rgdal::readOGR, layer = layer, stringsAsFactors = FALSE, verbose = FALSE)
	}
	else {
		print("No ».kml« files found. Check your source folder.")
	}
	# Clean the data
	kml <- lapply(kml, data.frame)
	keeps <- c("track_seg_id", "track_seg_point_id", "ele", "time", "coords.x1", "coords.x2")
	kml <- lapply(kml, subset, select = keeps)
	cols <- c("segment", "segment_element", "elevation", "datetime", "longitude", "latitude")
	kml <- lapply(kml, setNames, cols)
	kml <- dplyr::bind_rows(kml, .id = "track")
	# return a tidy data frame
	return(kml)
}
