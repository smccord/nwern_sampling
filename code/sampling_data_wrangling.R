library(httr)
library(jsonlite)
library(terradactyl)

# LPI
# read and organize data
dima <- function(endpoint,values=NULL) {
  if(is.null(values)){
    url <- paste0("https://dima.landscapedatacommons.org/api/",endpoint)
    get_url <- GET(url) 
    flat_get <- content(get_url, "text")
    jsonize <- fromJSON(flat_get, flatten = TRUE)
    df <- as.data.frame(jsonize)
    return(df)
  } else if(!is.null(values)){
    
    url <- paste0("https://dima.landscapedatacommons.org/api/",endpoint,"?",values)
    get_url <- GET(url) 
    flat_get <- content(get_url, "text")
    jsonize <- fromJSON(flat_get, flatten = TRUE)
    df <- as.data.frame(jsonize)
    return(df)
  }
}

lpi_detail<-dima("tblLPIDetail") # needs function to create unlimited request parameters and ellipsis to insert
lpi_header <-dima("tblLPIHeader") # needs function to create unlimited request parameters and ellipsis to insert
tblPlots <- dima("tblPlots")

lpi_tall <- gather_lpi_terradat(detail = lpi_detail, 
                                header = lpi_header)

# create subset groups
sample_lpi <- function(pin_drop, transect_length, lpi_tall = lpi_tall){
  lpi_subset <- lpi_tall %>% subset(PointLoc %in% seq(pin_drop,transect_length, pin_drop)) %>% 
    dplyr::mutate(pin_drop = pin_drop,
                  transect_length = transect_length)
  
}

subsample_lpi <- lapply(c(0.25, 0.5, 1), 
               function(d) lapply(X = c(25, 50,100), 
                                  function(X) sample_lpi(lpi_tall = lpi_tall, 
                                                         pin_drop = d, 
                                                         transect_length = X)) 
)


subsample_df <- do.call("rbind", test) %>% do.call("rbind",.)

# join to Plot ID
subsample_df <- dplyr::left_join(subsample_df, tblPlots %>% dplyr::select(PrimaryKey, PlotID))    

# Add sampling info to PrimaryKey to create unique instance
subsample_df <- subsample_df %>% dplyr::mutate(PrimaryKey = paste(PrimaryKey, 
                                                                  pin_drop, 
                                                                  transect_length, 
                                                                  sep = "_"))

# Calculate Indicators ####
# line level summaries
bare_soil_lines <- terradactyl::pct_cover_bare_soil(lpi_tall = subsample_df,
                                                    by_line = TRUE, tall = TRUE)
total_foliar_lines <- terradactyl::pct_cover_total_foliar(lpi_tall = subsample_df,
                                                          by_line = TRUE, tall = TRUE) 
litter_lines <- terradactyl::pct_cover_litter(lpi_tall = subsample_df,
                                              by_line = TRUE, tall = TRUE)
lines_calcs <- dplyr::bind_rows(
  total_foliar_lines,
  bare_soil_lines,
  litter_lines)

lines_summary <- lines_calcs %>% dplyr::group_by(PrimaryKey, indicator) %>% 
  dplyr::summarise(within_plot_mean = mean(percent), within_plot_sd = sd(percent))%>%
  dplyr::mutate(within_plot_mean = dplyr::if_else(is.na(within_plot_mean), 0, within_plot_mean),
                within_plot_sd = dplyr::if_else(is.na(within_plot_sd), 0, within_plot_sd))


# plot level summaries
bare_soil_plots <- terradactyl::pct_cover_bare_soil(lpi_tall = subsample_df,
                                                    tall = TRUE)
total_foliar_plots <- terradactyl::pct_cover_total_foliar(lpi_tall = subsample_df,
                                                           tall = TRUE) 
litter_plots <- terradactyl::pct_cover_litter(lpi_tall = subsample_df,
                                              tall = TRUE)
plots_calcs <- dplyr::bind_rows(
  total_foliar_plots,
  bare_soil_plots,
  litter_plots)

plots_calcs <- dplyr::left_join(plots_calcs, subsample_df %>% dplyr::select(PrimaryKey, PlotID))

plots_summary <- plots_calcs %>% dplyr::group_by(PrimaryKey, PlotID) %>% 
  dplyr::summarise(between_plot_mean = mean(percent), between_plot_sd = sd(percent)) %>%
  dplyr::mutate(between_plot_mean = dplyr::if_else(is.na(between_plot_mean), 0, between_plot_mean),
                between_plot_sd = dplyr::if_else(is.na(between_plot_sd), 0, between_plot_sd))

# Power to detect change ####
mdc = function(chi, alpha, beta, n){
  
}
