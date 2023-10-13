#' Calculates a market size for nearest grocery stores
#'
#' @description This function calculates a market size area given distances (in miles)
#' to nearest grocery stores in four cardinal directions.
#'
#' @param north_distance distance to northern store in miles.
#' @param east_distance distance to eastern store in miles.
#' @param south_distance distance to southern store in miles.
#' @param west_distance distance to western store in miles.
#'
#'
#' @author Harun Celik
#' @export

Quadrant_Area <- function(north_distance, east_distance, south_distance, west_distance) {

  # Calculate each of the wedges of the circle for each quadrant
  north_area <- ((pi * (north_distance/2) * (north_distance/2))/4)
  east_area <- ((pi * (east_distance/2) * (east_distance/2))/4)
  south_area <- ((pi * (south_distance/2) * (south_distance/2))/4)
  west_area <- ((pi * (west_distance/2) * (west_distance/2))/4)

  # Sum the wedges to get a total area and round to one decimal place
  market_total_area <- round(sum(north_area, east_area, south_area, west_area), digits = 1)

  # Put all calculations into a list
  areas_list <- list(north_area = round(north_area, digits = 1),
                     east_distance = round(east_area, digits = 1),
                     south_area = round(south_area, digits = 1),
                     west_area = round(west_area, digits = 1),
                     market_total_area = market_total_area)

  return(areas_list)
}
