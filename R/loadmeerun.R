loadmeerun <- function(folder = NULL, layer = "track_points") {
	# read MeeRun ».gpx« data from file(s) in a non-interactive manner. All files within the given directory will be read.
	if (is.null(folder)) {
		folder <- readline(prompt = "Please select a folder! ")
	}
	files <- list.files(path = folder, full.names = TRUE, pattern = ".gpx")
	# check whether GPS data could be loaded
	if (length(files) >= 1) {
		# get infos via ogrListLayers(object/"filename") and ogrInfo()
		gpx <- lapply(files, FUN = rgdal::readOGR, layer = layer, stringsAsFactors = FALSE, verbose = FALSE)
	}
	else {
		print("No ».gpx« files found. Check your source folder.")
	}
	# Clean the data
	gpx <- lapply(gpx, data.frame)
	keeps <- c("track_seg_id", "track_seg_point_id", "ele", "time", "coords.x1", "coords.x2")
	gpx <- lapply(gpx, subset, select = keeps)
	cols <- c("segment", "segment_element", "elevation", "datetime", "longitude", "latitude")
	gpx <- lapply(gpx, setNames, cols)
	gpx <- dplyr::bind_rows(gpx, .id = "track")
	# return a tidy data frame
	return(gpx)
}
