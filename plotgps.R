plotgpx <- function(folder, type = "osm", zoom = 14, col = "purple", mapborder = 0.1, filetitle = "Map plot") {
	#' plotgpx
	#'
	#' @description Create a map of GPS data
	#' @references help and infos from:
	#' http://dbsgeo.com/latlon/
	#' http://www.r-bloggers.com/plot-maps-like-a-boss/
	#' http://stackoverflow.com/questions/13762793/plotting-choropleth-maps-from-kml-data-using-ggplot2
	#' http://flowingdata.com/2011/05/11/how-to-map-connections-with-great-circles/
	#' http://rpsychologist.com/parsing-data-from-a-text-file-and-plotting-where-people-live-using-ggplot2-and-openstreetmaps
	#' A1 paper: 594 * 841 mm

	# load the required libraries
	source(file = "loadgpxlibs.r")
   source(file = "readgpxtracks.r")
   source(file = "plotgpxtracks.r")
	loadgpxlibs()

	# load the gpx data
	# get infos via ogrListLayers(object) or ogrListLayers("filename")
	gpx <- readgpxtracks(folder = folder)

	# set the map area; needs to be automated
	# create a data frame which holds all min and max lat/lon values
	minmax <- data.frame(xmin = vector(mode = "numeric", length = length(gpx)),
											 xmax = vector(mode = "numeric", length = length(gpx)),
											 ymin = vector(mode = "numeric", length = length(gpx)),
											 ymax = vector(mode = "numeric", length = length(gpx)))

											 # fill the data frame with the min and max values
	for (i in 1:length(gpx)) {
		# x values
		minmax[i,1:2] <- summary(gpx[[i]])[[2]][1,]
		# y values
		minmax[i,3:4] <- summary(gpx[[i]])[[2]][2,]
	}

	# get edges: two lat / lon pairs. lat == y, lon == x
	tl <- c(max(minmax[, 3:4]), min(minmax[, 1:2]))
	br <- c(min(minmax[, 3:4]), max(minmax[, 1:2]))
	# create a data frame with lat and lon data
	coords <- data.frame(cbind(tl, br), row.names = c("lat", "lon"))
	# increase the range by a given factor
	coords[1, ] <- extendrange(coords[1, ], f = mapborder)
	coords[2, ] <- extendrange(coords[2, ], f = mapborder)

	# height and width of the map
	hw <- abs(tl - br)

	# download map tiles
	map <- openproj(openmap(tl, br, zoom = zoom, type = type), projection = "+proj=longlat")

	# create a pdf file
	pdf(file = "Map.pdf", title = filetitle, width = 19.69, height = 27.95)
	plot(map)

	# plot gpx tracks
	plotgpxtracks(gpx, colour = col)

	dev.off()
}
