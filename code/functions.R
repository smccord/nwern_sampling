
# build lpi function that iterates over different line lengths, pin drop intervals, and number of transects
lpi_iteration <- function(lpi_tall, 
                       line_length = 100,
                       interval = 25, 
                       n_lines = 1, 
                       FUN){
  
  # select PointNbrs to use
  point_nbr <- seq(1,line_length*4, interval/25)
  
  # convert pin_n to PointNbr
  pin_n = length(point_nbr)*n_lines
  
  # subset needed pin drops
  lpi_subset <- lpi_tall %>% subset(PointNbr %in% point_nbr)
  
  # randomly subset number of lines
  lines_summary <- lpi_subset %>% dplyr::select(PrimaryKey, LineKey) %>%
    dplyr::distinct() %>% dplyr::group_by(PrimaryKey) %>% dplyr::slice_sample(n = n_lines)
  
  lpi_subset <- dplyr::left_join(lines_summary, lpi_subset)
  
  cover <- FUN(lpi_subset, tall = TRUE) %>% dplyr::mutate(line_length = line_length, 
                                                          interval = interval, 
                                                          n_lines, 
                                                          pin_n)
  
}


# build functions to iterate ttest by indicator and group
group_ttest <- function(data){
  lapply(X= unique(data$group)[unique(data$group)!="NWERN Base"],
         function(X){
           data_group <-data %>% subset(group ==X)
           test <- t.test(data_group$percent, data_group$nwern_percent, paired = T) %>% 
             broom::tidy()
           test$group <- X
           test
         }) %>% do.call("rbind",.) %>%
    # note if significant or not
     dplyr::mutate(significant = ifelse(p.value<0.05, "no", "yes"))

}

iteration_ttest <- function(data){
  lapply(X = unique(data$indicator),
         function(X){
           data_indicator <- data %>% subset(indicator == X)
           test <- group_ttest(data = data_indicator) %>% 
             dplyr::mutate(indicator = X)
         }
         ) %>% do.call("rbind",.)
}

# function to read latest file
latest_file = function(fpattern, fpath) {
  f = list.files(pattern=fpattern, path=fpath, full.names=TRUE)
  f = file.info(f)
  rownames(f)[which.max(f$mtime)] 
}

myfile = latest_file(fpattern="myfile.*csv", fpath="~/downloads")
d = readr::read_csv(myfile)


# functions to iterate prop.test by indicator, group, PrimaryKey

