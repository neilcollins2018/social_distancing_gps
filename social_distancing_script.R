library(tidyverse)
library(Imap)

#### Distance Matrix functions
ReplaceLowerOrUpperTriangle <- function(m, triangle.to.replace){
  # If triangle.to.replace="lower", replaces the lower triangle of a square matrix with its upper triangle.
  # If triangle.to.replace="upper", replaces the upper triangle of a square matrix with its lower triangle.
  
  if (nrow(m) != ncol(m)) stop("Supplied matrix must be square.")
  if      (tolower(triangle.to.replace) == "lower") tri <- lower.tri(m)
  else if (tolower(triangle.to.replace) == "upper") tri <- upper.tri(m)
  else stop("triangle.to.replace must be set to 'lower' or 'upper'.")
  m[tri] <- t(m)[tri]
  return(m)
}

GeoDistanceInMetresMatrix <- function(df.geopoints){
  # Returns a matrix (M) of distances between geographic points.
  # The row and column names are given by df.geopoints$name.
  
  GeoDistanceInMetres <- function(g1, g2){
    # Returns a vector of distances. (But if g1$index > g2$index, returns zero.)
    # The 1st value in the returned vector is the distance between g1[[1]] and g2[[1]].
    # The 2nd value in the returned vector is the distance between g1[[2]] and g2[[2]]. Etc.
    DistM <- function(g1, g2){
      
      return(ifelse(g1$index > g2$index, 0, gdist(lat.1=g1$latitude, lon.1=g1$longitude, lat.2=g2$latitude, lon.2=g2$longitude, units="m")))
    }
    return(mapply(DistM, g1, g2))
  }
  
  n.geopoints <- nrow(df.geopoints)
  
  # The index column is used to ensure we only do calculations for the upper triangle of points
  df.geopoints$index <- 1:n.geopoints
  
  # Create a list of lists
  list.geopoints <- by(df.geopoints[,c("index", "latitude", "longitude")], 1:n.geopoints, function(x){return(list(x))})
  
  # Get a matrix of distances (in metres)
  mat.distances <- ReplaceLowerOrUpperTriangle(outer(list.geopoints, list.geopoints, GeoDistanceInMetres), "lower")
  # Set the row and column names
  rownames(mat.distances) <- df.geopoints$id
  colnames(mat.distances) <- df.geopoints$id
  
  return(mat.distances)
}


##### File read function
data_read <- function(fname) {
  
  data = read_csv(here::here('taxi_log',fname), 
                  col_names = F) %>%
    rename(
      'id' = 'X1',
      'time' = 'X2',
      'longitude' = 'X3',
      'latitude' = 'X4',
    )
  
  return(data)
  
}

####List files
activity_files <- list.files(here::here('taxi_log'), ".txt")

### Read files
list_df <- 
  activity_files %>%
  set_names(.) %>%
  map_dfr(data_read) %>%
  split(.$id)

### Use a single file to create a time series sequence with a set length which will then be used to align all other time-series
single_file <- list_df[[1]]
time_distinct <- single_file %>% distinct(time)
new_row <- nrow(time_distinct)
time_range <- seq(min(single_file$time), max(single_file$time), length.out = new_row)

##### Using the above time sequence, all other files will be resampled so as to align all data
new_list_df <- list()
for(i in seq_along(list_df)) {
  
  df <- list_df[[i]]
  id_name <- paste0('id_', unique(df$id))
  name <- rep(id_name, new_row)
  
  lat_new <- approx(1:length(df$latitude), df$latitude, n = new_row)$y
  long_new <- approx(1:length(df$longitude), df$longitude, n = new_row)$y
  df_ind_final <- tibble(
    'time' = time_range, 
    'id' = name,
    'latitude' = lat_new,
    'longitude' = long_new
  )
  
  new_list_df[[i]] <- df_ind_final
  
}

#### Data is then split so as to create a list of dataframes with a dataframe at each timepoint
new_list_df <- new_list_df %>% bind_rows(.) %>% split(.$time)

#### Loop through list applying distance matrix function and then place into new list
dist_matr_list <- list()
for(q in seq_along(new_list_df)){
  
  
  nam <- names(new_list_df)[[q]]
  dist_df <- new_list_df[[q]]
  dist_matr <- GeoDistanceInMetresMatrix(dist_df)
  
  #### Here any distances under 1000 are changed to 1, values above or equal to 0 are changed to 0
  #### This should be changed to 2m for social distancing investigation
  dist_matr[dist_matr <= 1000 & dist_matr != 0] <- 1
  dist_matr[dist_matr > 1000] <- 0
  
  dist_matr_list[[q]] <- dist_matr
  
}

#### Aggregate list of matices, the alter to be time series data
final_incursion_matrix <- Reduce('+', dist_matr_list)
final_time_matrix <- final_incursion_matrix/864000 #### Data sampled at 10htz. 86400 seconds in a day, 10 data points per second.
final_time_matrix <- chron::times(final_time_matrix)

write.table(final_incursion_matrix, 'incursion_tabl.csv', sep = ',', col.names = NA) 
write.table(final_time_matrix, 'incursion_time.csv', sep = ',', col.names = NA)
