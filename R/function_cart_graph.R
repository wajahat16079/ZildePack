

# Function that create a map centered on the start point

#' Title
#'
#' @param datf
#'
#' @return
#' @export
#'
#' @examples
#'
#'
function_cart_graph <- function(datf) {

m <- leaflet(datf) %>%
  addTiles() %>%
  setView(lng = mean(as.numeric(datf[,2])), lat = mean(as.numeric(datf[,1])), zoom = 2)

# Add markers for the start, end, and additional cities
m <- m %>%
  addCircleMarkers(data = datf, label = ~datf[,3])

# Create lines between the start and end points and the additional cities
m <- m %>%
  addPolylines(lat = datf[,1], lng = datf[,2])

# Print the map
return(m)
}

