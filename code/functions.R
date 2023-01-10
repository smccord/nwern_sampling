
# LPI iteration ####
# build lpi function that iterates over different line lengths, pin drop intervals, and number of transects
lpi_iteration <- function(lpi_tall, 
                       line_length = 50,
                       interval = 25, 
                       n_lines = 1, 
                       FUN){
  
 print(paste("line length:", line_length, "interval:", interval, "n_lines:", n_lines ))
   # select PointNbrs to use
  point_nbr <- seq(1,line_length*4, interval/25)
  
  # convert pin_n to PointNbr
  pin_n = length(point_nbr)*n_lines
  
  # reset transect layout to radiating spoke design if transects <= 50 m
  if(line_length <=50){
    lpi_tall <- lapply(X = unique(lpi_tall$PrimaryKey), 
                       FUN = function(x) {
                         print(x)
                         pk_data <- lpi_tall %>% subset(PrimaryKey %in% x)
                         line_key_data <- lapply(X = unique(pk_data$LineKey), 
                                FUN = function(x){
                                  line_data <- pk_data %>% subset(LineKey %in% x)
                                  line_missing_pt <- setdiff(1:400, line_data$PointNbr) 
                                  # it appears occasionally the pad is not removed from data
                                  if(length(line_missing_pt)==0){
                                    line_missing_pt <- 201
                                  }
                                  # line a will run from the middle of the transect out instead of out in
                                  line_a <- line_data %>% 
                                    subset(PointNbr %in% 1:(min(line_missing_pt)-1)) %>% 
                                    dplyr::left_join(
                                      tibble(PointNbr = 1:(min(line_missing_pt)-1), 
                                             PointNbr_new = (min(line_missing_pt)-1):1)) %>% 
                                    dplyr::mutate(
                                      PointNbr_old = PointNbr,
                                      PointNbr = PointNbr_new, 
                                      LineKey = paste0(LineKey, "a")) 
                                  # line b will run from out in but we reset numbers to run from 1 to n
                                  if(length(line_missing_pt)==0){
                                    line_missing_pt <- 200
                                  }
                                  line_b <- line_data %>% 
                                    subset(PointNbr %in% max(line_missing_pt):400) %>% 
                                    dplyr::left_join(tibble(PointNbr = (max(line_missing_pt)+1):400, 
                                                            PointNbr_new = 1:(400-max(line_missing_pt)))) %>%
                                    dplyr::mutate(
                                      PointNbr_old = PointNbr,
                                      PointNbr = PointNbr_new, 
                                      LineKey = paste0(LineKey, "b")) 
                                  
                                  new_line_data <- dplyr::bind_rows(line_a, line_b)
                                  
                                  
                                } ) %>% do.call(bind_rows, .) 
                         
                         
                       })  %>% do.call(bind_rows, .) 

    
  }
  
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


# Helper functions ####
# function to read latest file
latest_file = function(fpattern, fpath) {
  f = list.files(pattern=fpattern, path=fpath, full.names=TRUE)
  f = file.info(f)
  rownames(f)[which.max(f$mtime)] 
}

# statistical functions ####
# functions to perform Fisher's exact test and output results as data frame
fisher_test <- function(data = iteration_calcs, 
                        PK_sample, group_sample, 
                        compare_sample = "", 
                        compare_group = "" ) {
  print(paste("PrimaryKey:", PK_sample, "Group:", group_sample))
  
  subsample <- data %>% subset(PrimaryKey %in% c(PK_sample, compare_sample) & group %in% c(group_sample, compare_group ))
  
  if(nrow(subsample)==2){
    test <-  subsample %>%
      mutate(n_present = round(percent*pin_n/100),
             n_absent = pin_n - n_present) %>% 
      select(n_present, n_absent) %>% fisher.test()
    
    # output data frame
    df <- data.frame(PrimaryKey = PK_sample, 
                     group = group_sample, 
                     compare = compare_sample,
                     p.value = test$p.value, 
                     odds.ratio = test$estimate,
                     lower.conf.interval = test$conf.int[1], 
                     upper.conf.interval = test$conf.int[2]) %>% `rownames<-`( NULL )
  }else{
    warning(paste("PrimaryKey:", PK_sample, "Group:", group_sample, "not run"))
    df <- NULL
  }
  
}

fisher_test_gap <- function(data = gap_calcs, 
                          PK_sample = "15050113465465692015-05-05", 
                          group_sample = "L_25_N_1",
                          compare_sample = "", 
                          compare_group = "NWERN Base",
                          gap_indicator = "201-Inf"){
  print(paste("PrimaryKey:", PK_sample, "Group:", group_sample))
  
  subsample <- data %>% subset(PrimaryKey %in% c(PK_sample, compare_sample) & group %in% c(group_sample, compare_group ) & gap_class %in% gap_indicator)
  
  if(nrow(subsample)==2){
    test <- subsample %>% 
      mutate(gap = length,
             no_gap = total_line_length - length ) %>% 
      select(gap, no_gap) %>% fisher.test()
    
    # output data frame
    df <- data.frame(PrimaryKey = PK_sample, 
                     compare_group = compare_group,
                     group = group_sample, 
                     compare = compare_sample,
                     gap_class = gap_indicator,
                     p.value = test$p.value, 
                     odds.ratio = test$estimate,
                     lower.conf.interval = test$conf.int[1], 
                     upper.conf.interval = test$conf.int[2]) %>% `rownames<-`( NULL )
  }else{
    warning(paste("PrimaryKey:", PK_sample, "Group:", group_sample, "not run"))
    df <- NULL
  }
  
        
        
}

# gap iteration ####
gap_iteration <- function(
    gap, 
    line_length = 25,
    n_lines = 1
    ){
 
  line_length_cm <- line_length*100
  # subset line
  if(line_length <=50) {
    # reorient transects to start at the center (50 m mark)
    # first deal with instances of NoCanopyGap
    if(any(gap$GapStart == 0 & gap$GapEnd == 0)) {
      gap <- bind_rows(
        gap %>% subset(GapStart == 0 & GapEnd == 0) %>%
          mutate(GapStart = 5000, 
                 GapEnd = 5000), 
        gap %>% subset(GapStart == 0 & GapEnd == 0) %>%
          mutate(GapStart = 5001, 
                 GapEnd = 5001), 
        gap %>% subset(!(GapStart == 0 & GapEnd == 0)) 
      ) %>% mutate(Gap = abs(GapStart-GapEnd) )
      
    } 
    # split gaps that cross the 50 m mark
    if (any(gap$GapStart <= 5000 & gap$GapEnd >5000)) {
      gap_50 <- bind_rows (
        gap %>% subset(GapStart <= 5000 & GapEnd >5000) %>% 
        mutate(GapEnd = 5000), 
        gap %>% subset(GapStart <= 5000 & GapEnd >5000) %>% 
          mutate(GapStart = 5001)) %>% mutate(Gap = abs(GapStart-GapEnd))
      
      # join back to main data
      gap <- gap %>% subset(!(GapStart <= 5000 & GapEnd >5000)) %>% 
        bind_rows(gap_50)
    }
    
    # create new LineKeys
    gap <- gap %>% mutate(LineKey_sub = case_when (GapStart <= 5000 ~ "a",
                                               GapStart > 5000 ~ "b")
    )
    
    # truncate lines to line length
    if(line_length <50){
      # reserve all PrimaryKey/LineKey combinations for reference later
      unique_lines <- gap %>% 
        dplyr::select(PrimaryKey, LineKey, LineKey_sub) %>% dplyr::distinct()
      
      # The A line runs from 5000 cm to 0, truncate to line length
      gap_a <- gap %>% subset(LineKey_sub == "a" & GapStart > (5000-line_length_cm))
      gap_a_truncate <- gap %>% 
        subset(LineKey_sub == "a" & GapStart < (5000-line_length_cm) & GapEnd > (5000-line_length_cm))  %>%
        mutate(GapStart = 2500)
      
      gap_a <- bind_rows(gap_a, 
                         gap_a_truncate)
      
   
      # The b line runs from 5000 cm to 10000 cm, truncate to line length
      gap_b <- gap %>% subset(LineKey_sub == "b" & GapEnd < (5000 + line_length_cm))
      gap_b_truncate <- gap %>% 
        subset(LineKey_sub == "b" & GapStart < (5000+line_length_cm) & GapEnd > (5000+line_length_cm))  %>%
        mutate(GapEnd = 7500)
      
      gap_b <- bind_rows(gap_b, 
                         gap_b_truncate)
      
      gap_new <- bind_rows(gap_a, gap_b)
      
      # find instances where the truncation would yield no canopy gaps
     gap_missing <- gap_new %>% 
        dplyr::select(PrimaryKey, LineKey, LineKey_sub) %>% dplyr::distinct()  %>% anti_join(unique_lines, 
                                                                                             .) %>%
       left_join(gap) %>%
        mutate(GapStart = case_when(LineKey_sub == "a" ~ 5000,
                                    LineKey_sub == "b" ~ 5001),
               GapEnd = case_when(LineKey_sub == "a" ~ 5000,
                                  LineKey_sub == "b" ~ 5001))
      
      
    # bind it all back together
     gap <- bind_rows(gap_new, 
                      gap_missing) %>%
        # recalculate gaps
        mutate(Gap = abs(GapStart-GapEnd))
               
      
    }
    # create a and b LineKeys
    gap <- gap %>% mutate(
      LineKey = paste(LineKey, LineKey_sub, sep = "_")
    )
                                    
  }
  

  # randomly subset number of lines
  lines_summary <-  gap %>% 
    dplyr::select(PrimaryKey, LineKey) %>% dplyr::distinct() %>%
    dplyr::group_by(PrimaryKey) %>% dplyr::slice_sample(n = n_lines)
  
  gap_subset <- dplyr::left_join(lines_summary, gap) %>% 
    mutate(LineKey = as.character(LineKey))
  
  gap_subset$LineLengthAmount <- line_length
  
  gap_subset <- gap_subset %>% dplyr::mutate(
                                group = paste("L", line_length, "N", n_lines, sep = "_"), 
                                line_length = line_length, 
                                n_lines = n_lines
                                )
  
}


