
# define global map function (monthly)
map_global <- function(df, var) {

  name_var <- var
  var <- sym(var)

  df_sub <- df %>%
    filter(lat < 65)

  df_sub %>%
    ggplot() +
    geom_raster(aes(lon, lat, fill = !!var)) +
    scale_fill_viridis_c(name = name_var) +
    coord_quickmap(expand = 0) +
    labs(title = paste(name_var, "Global Surface Distribution"), subtitle = "Depth: 5m  Month: August", x = "Longitude", y = "Latitude") +
    geom_raster(data = landmask,
                aes(lon, lat), fill = "grey80")

}


# define global section function (monthly)
section_global <- function(df, var) {

  name_var <- var
  var <- sym(var)

  df_sub <- left_join(section_global_coordinates, df)

  surface <- df_sub %>%
    ggplot(aes(dist, depth, z = !!var)) +
    geom_contour_filled() +
    geom_vline(data = section_global_coordinates %>% filter(lat == 0.5),
               aes(xintercept = dist), col = "white") +
    scale_fill_viridis_d(name = name_var) +
    coord_cartesian(expand = 0,
                    ylim = c(500, 0)) +
    scale_y_reverse() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "top") +
    labs(y = "Depth (m)",
         title = "Global U-shape Section",
         subtitle = "Month: August")

  deep <- df_sub %>%
    ggplot(aes(dist, depth, z = !!var)) +
    geom_contour_filled() +
    geom_vline(data = section_global_coordinates %>% filter(lat == 0.5),
               aes(xintercept = dist), col = "white") +
    scale_fill_viridis_d(name = name_var) +
    scale_y_reverse() +
    coord_cartesian(expand = 0, ylim = c(5400, 500)) +
    labs(x = "Distance (Mm)", y = "Depth (m)") +
    theme(legend.position = "none")

  surface / deep

}


# define zonal mean section function (monthly)
section_zonal <- function(df, i_basin_AIP, var) {

  name_var <- var
  var <- sym(var)

  df_sub <- df %>%
    filter(basin_AIP == i_basin_AIP) %>%
    group_by(lat, depth) %>%
    summarise(zonal_mean = mean(!!var))

  lat_max <- max(df_sub$lat)
  lat_min <- min(df_sub$lat)

  surface <- df_sub %>%
    ggplot(aes(lat, depth, z = zonal_mean)) +
    geom_contour_filled(breaks = breaks) +
    scale_fill_viridis_d(name = name_var) +
    coord_cartesian(expand = 0,
                    ylim = c(500, 0),
                    xlim = c(lat_min, lat_max)) +
    scale_y_reverse() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
    ) +
    labs(y = "Depth (m)",
         title = "Zonal Average Section",
         subtitle = paste("Basin:", i_basin_AIP, "| Month: August"))

  deep <- df_sub %>%
    ggplot(aes(lat, depth, z = zonal_mean)) +
    geom_contour_filled(breaks = breaks) +
    scale_fill_viridis_d(name = name_var) +
    scale_y_reverse() +
    coord_cartesian(expand = 0,
                    ylim = c(5400, 500),
                    xlim = c(lat_min, lat_max)) +
    labs(x = "latitude (°N)", y = "Depth (m)")

  surface / deep +
    plot_layout(guides = "collect")

}


# define global map function (annual)
map_global_aa <- function(df, var) {

  name_var <- var
  var <- sym(var)

  df_sub <- df %>%
    filter(lat < 65)

  df_sub %>%
    ggplot() +
    geom_raster(aes(lon, lat, fill = !!var)) +
    scale_fill_viridis_c(name = name_var) +
    coord_quickmap(expand = 0) +
    labs(title = paste(name_var, "Global Surface Annual Average Distribution"), subtitle = "Depth: 5m", x = "Longitude", y = "Latitude") +
    geom_raster(data = landmask,
                aes(lon, lat), fill = "grey80")

}


# define global section function (annual)
section_global_aa <- function(df, var) {

  name_var <- var
  var <- sym(var)

  df_sub <- left_join(section_global_coordinates, df)

  surface <- df_sub %>%
    ggplot(aes(dist, depth, z = !!var)) +
    geom_contour_filled() +
    geom_vline(data = section_global_coordinates %>% filter(lat == 0.5),
               aes(xintercept = dist), col = "white") +
    scale_fill_viridis_d(name = name_var) +
    coord_cartesian(expand = 0,
                    ylim = c(500, 0)) +
    scale_y_reverse() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "top") +
    labs(y = "Depth (m)",
         title = "Global U-shape Section",
         subtitle = "Annual average")

  deep <- df_sub %>%
    ggplot(aes(dist, depth, z = !!var)) +
    geom_contour_filled() +
    geom_vline(data = section_global_coordinates %>% filter(lat == 0.5),
               aes(xintercept = dist), col = "white") +
    scale_fill_viridis_d(name = name_var) +
    scale_y_reverse() +
    coord_cartesian(expand = 0, ylim = c(5400, 500)) +
    labs(x = "Distance (Mm)", y = "Depth (m)") +
    theme(legend.position = "none")

  surface / deep

}


# define zonal mean section function (annual)
section_zonal_aa <- function(df, i_basin_AIP, var) {

  name_var <- var
  var <- sym(var)

  df_sub <- df %>%
    filter(basin_AIP == i_basin_AIP) %>%
    group_by(lat, depth) %>%
    summarise(zonal_mean = mean(!!var))

  lat_max <- max(df_sub$lat)
  lat_min <- min(df_sub$lat)

  surface <- df_sub %>%
    ggplot(aes(lat, depth, z = zonal_mean)) +
    geom_contour_filled(breaks = breaks) +
    scale_fill_viridis_d(name = name_var) +
    coord_cartesian(expand = 0,
                    ylim = c(500, 0),
                    xlim = c(lat_min, lat_max)) +
    scale_y_reverse() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
    ) +
    labs(y = "Depth (m)",
         title = "Zonal Annual Average Section",
         subtitle = paste("Basin:", i_basin_AIP))

  deep <- df_sub %>%
    ggplot(aes(lat, depth, z = zonal_mean)) +
    geom_contour_filled(breaks = breaks) +
    scale_fill_viridis_d(name = name_var) +
    scale_y_reverse() +
    coord_cartesian(expand = 0,
                    ylim = c(5400, 500),
                    xlim = c(lat_min, lat_max)) +
    labs(x = "latitude (°N)", y = "Depth (m)")

  surface / deep +
    plot_layout(guides = "collect")

}
