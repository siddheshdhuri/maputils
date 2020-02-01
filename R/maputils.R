# Map Utils package!
#
# This package has functions that help in manipulating data for plotting
# in leaflet map
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' function to get df to plot on map
#'
#' @param df with account details and longitude latitude colums
#' @param UNIQUE_ID_COL unique identified column for customer recrods
#' @param CUTOMER_SIZE_COL column to determine radius of circle.
#' @param LONGITUDE_COL
#' @param LATITUDE_COL
#'
#' @return data.frame with complete coordinates and radius for plotting
#'
#' @export
getMapPlotDF <- function(df, UNIQUE_ID_COL, CUTOMER_SIZE_COL,
                         LONGITUDE_COL=NULL, LATITUDE_COL=NULL,
                         radius_lower_limit=8, radius_upper_limit=60){

  #' Keep one row per customer record unique_id
  accounts <- df[ !duplicated(df[[UNIQUE_ID_COL]]) , ]

  #' set NA account_values to zero
  accounts[[CUTOMER_SIZE_COL]][is.na(accounts[[CUTOMER_SIZE_COL]])] <- 0

  #' if coordiante columns are null return null
  if(is.null(LONGITUDE_COL) | is.null(LATITUDE_COL)) return(NULL)

  has.cood.index <- complete.cases(accounts[,c(LONGITUDE_COL,LATITUDE_COL)])
  has.cood.account <- accounts[has.cood.index,]

  #' if there is no row with coordiantes return NULL
  if(nrow(has.cood.account) <= 0) return(NULL)

  #' crete new radius column and scale size column in the range 8 to 60
  has.cood.account$RADIUS <- scales::rescale(
    has.cood.account[[CUTOMER_SIZE_COL]], to = c(radius_lower_limit,radius_uper_limit))

  # generate second set of unique location IDs for second layer of selected locations
  has.cood.account$secondLocationID <- paste0(as.character(has.cood.account[[UNIQUE_ID_COL]]), "_selectedLayer")

  return(has.cood.account)

}




#' Find locations inside a polygon, square, or circle drawn with leaflet.extras drawing tools on a Shiny Leaflet map.
#'
#' @param shape Shiny input (input$MAPID_draw_new_feature), representing shape drawn on the map by the user.
#' @param location_coordinates A SpatialPointsDataFrame containing coordinates and ids for all map locations.
#' @param location_id_colname Column name from location_coordinates containing desired names or ids for set of locations returned.
#' @return A vector of location ids.
#' @examples
#' mock_input.map_feature <- list(type = "Feature"
#'                          , properties = list(`_leaflet_id`= 13477, feature_type = "rectangle")
#'                          , geometry = list(type = "Polygon"
#'                          , coordinates = list(list(list(-76.15723, 39.51252)
#'                          , list(-76.15723,  40.30467), list(-74.73999, 40.30467)
#'                          , list(-74.73999, 39.51252), list(-76.15723, 39.51252)))))
#' airports <- data.frame('locationID' = c('PHL', 'DTW')
#'                       , 'Longitude' = c(-75.2408, -83.3533)
#'                       , 'Latitude' = c(39.8722, 42.2125))
#' coords = sp::SpatialPointsDataFrame(airports[,c('Longitude', 'Latitude')], airports)
#' findLocations(shape = mock_input.map_feature
#'                      , location_coordinates = coords
#'                      , location_id_colname = "locationID")
#'
#' @export
findLocations <- function(shape, location_coordinates, location_id_colname) {

  # derive polygon coordinates and feature_type from shape input
  polygon_coordinates <- shape$geometry$coordinates
  feature_type <- shape$properties$feature_type

  if(feature_type %in% c("rectangle","polygon")) {

    # transform into a spatial polygon
    drawn_polygon <- sp::Polygon(do.call(rbind,lapply(polygon_coordinates[[1]],function(x){c(x[[1]][1],x[[2]][1])})))

    # identify selected locations
    selected_locs <- sp::over(location_coordinates, sp::SpatialPolygons(list(sp::Polygons(list(drawn_polygon),"drawn_polygon"))))

    # get location ids
    x = (location_coordinates[which(!is.na(selected_locs)), location_id_colname])

    selected_loc_id = as.character(x[[location_id_colname]])

    return(selected_loc_id)

  } else if (feature_type == "circle") {

    center_coords <- matrix(c(polygon_coordinates[[1]], polygon_coordinates[[2]])
                            , ncol = 2)

    # get distances to center of drawn circle for all locations in location_coordinates
    # distance is in kilometers
    dist_to_center <- sp::spDistsN1(location_coordinates, center_coords, longlat = TRUE)

    # get location ids
    # radius is in meters
    x <- location_coordinates[dist_to_center < shape$properties$radius/1000, location_id_colname]

    selected_loc_id = as.character(x[[location_id_colname]])

    return(selected_loc_id)
  }
}
