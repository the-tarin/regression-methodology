weighted_distance_rainfall_average <- function(a_distance, b_distance, a_rainfall, b_rainfall) {
  numer <- a_rainfall/a_distance + b_rainfall/b_distance
  demon <- 1/a_distance + 1/b_distance
  weighted_average <- numer/denom
  return weighted_average
}