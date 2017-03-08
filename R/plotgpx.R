#' plotgpx
#'
#' @description Create a map with GPS tracks using map parameters extracted from used tracks
#' @param folder select the folder containing the kml file(s)
#' @param type the type of map
#' @param zoom zoom level of the map
#' @param col colour of the GPS tracks
#' @param mapborder ratio by which the height and width of the map are increased
#' @param pdftitle title of the resulting pdf file
#' @return kml tidy data frame containing loaded data
#' @export
#' @references help and infos from:
#' - http://dbsgeo.com/latlon/
#' - http://www.r-bloggers.com/plot-maps-like-a-boss/
#' - http://stackoverflow.com/questions/13762793/plotting-choropleth-maps-from-kml-data-using-ggplot2
#' - http://flowingdata.com/2011/05/11/how-to-map-connections-with-great-circles/
#' - http://rpsychologist.com/parsing-data-from-a-text-file-and-plotting-where-people-live-using-ggplot2-and-openstreetmaps

plotgpx <- function(folder, type = "osm", zoom = 14, col = "purple",
                    mapborder = 0.1, pdftitle = "Map plot") {
	# load the gpx data
	gpx <- loadmeerun(folder = folder)
	# set map boundaries
	lon <- grDevices::extendrange(c(min(gpx$longitude), max(gpx$longitude)),
	                              f = mapborder)
	lat <- grDevices::extendrange(c(min(gpx$latitude), max(gpx$latitude)),
	                              f = mapborder)

	corners <- data.frame(tl = c(lon[1], lat[2]),
											  tr = c(lon[2], lat[2]),
											  bl = c(lon[1], lat[1]),
											  br = c(lon[2], lat[1]),
												row.names = c("longitude", "latitude"))

	# increase the range by a given factor
	# corners <- grDevices::extendrange(corners, f = mapborder)

	# download map tiles
	map <- ggmap::get_map(location = c(corners$bl, corners$tr),
	                      source = "stamen", maptype = "toner",
	                      zoom = 13, scale = "auto", messaging = FALSE)
	# create a pdf file
	grDevices::pdf(file = "Map.pdf", title = pdftitle,
	               width = 19.69, height = 27.95)
	ggmap::ggmap(map, extent = "device") +
		ggplot2::geom_path(ggplot2::aes(x = longitude, y = latitude,
		                                group = factor(track)),
		colour = "purple", data = gpx, alpha = 0.3, size = 1)
	grDevices::dev.off()
}
