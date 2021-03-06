#' loadkml
#'
#' @description Load kml files and return a tidy data frame (tested with files generated by KDE Marble).
#' @param folder select the folder containing the kml file(s)
#' @param layer select layer to use for reading the data
#' @return kml tidy data frame containing loaded data
#' @export
#'
loadkml <- function(folder = NULL, layer = "track_points") {
	if (is.null(folder)) {
		folder <- readline(prompt = "Please select a folder! ")
	}
	files <- list.files(path = folder, full.names = TRUE, pattern = ".kml")
	# check whether GPS data could be loaded
	if (length(files) >= 1) {
		kml <- lapply(files, FUN = rgdal::readOGR, layer = layer, stringsAsFactors = FALSE, verbose = FALSE)
	}
	else {
		print("No '.kml' files found. Check your source folder.")
	}
	# Clean the data
	kml <- lapply(kml, data.frame)
	keeps <- c("track_seg_id", "track_seg_point_id", "ele", "time", "coords.x1", "coords.x2")
	kml <- lapply(kml, subset, select = keeps)
	cols <- c("segment", "segment_element", "elevation", "datetime", "longitude", "latitude")
	kml <- lapply(kml, FUN = stats::setNames, cols)
	kml <- dplyr::bind_rows(kml, .id = "track")
	# Return a tidy data frame
	return(kml)
}
