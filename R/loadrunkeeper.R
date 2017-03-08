#' loadrunkeeper
#'
#' @description Load gpx files and return a tidy data frame (function
#' adapted from FlowingData).
#' @param folder select the folder containing the kml file(s)
#' @return gpx tidy data frame containing loaded data
#' @export
#'
loadrunkeeper <- function(folder) {
	# GPX files downloaded from Runkeeper
	files <- dir(folder, pattern = "\\.gpx")

	# Consolidate routes in one drata frame
	index <- c()
	latitude <- c()
	longitude <- c()
	for (i in seq_len(files)) {
		route <- plotKML::readGPX(files[i])
		location <- route$tracks[[1]][[1]]

		index <- c(index, rep(i, dim(location)[1]))
		latitude <- c(latitude, location$lat)
		longitude <- c(longitude, location$lon)
	}
	gpx <- data.frame(cbind(index, latitude, longitude))
  return(gpx)
	# Map the routes
	# ids <- unique(index)
	# plot(routes$longitude, routes$latitude, type = "n", axes = FALSE,
	# xlab = "", ylab = "", main = "", asp = 1)
	# for (i in 1:length(ids)) {
	# 	currRoute <- subset(routes, index == ids[i])
	# 	lines(currRoute$longitude, currRoute$latitude, col = "#00000020")
	# }
}
