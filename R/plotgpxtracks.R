plotgpxtracks <- function(gpx, colour = "purple", alpha = 0.5, ...) {
	# plot ».gpx« data from a list created with readgpxtracks as lines
	# packages scales is needed for colours with alpha channel
	colour = scales::alpha(colour, alpha) # make the colour transparent
	invisible(lapply(gpx, FUN = lines, col = colour, ...))
}
