instant_pkgs <- function(pkgs) { 
  pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
  if (length(pkgs_miss) > 0) {
    install.packages(pkgs_miss)
  }
  
  if (length(pkgs_miss) == 0) {
    message("\n ...Packages were already installed!\n")
  }
  
  # install packages not already loaded:
  pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
  if (length(pkgs_miss) > 0) {
    install.packages(pkgs_miss)
  }
  
  # load packages not already loaded:
  attached <- search()
  attached_pkgs <- attached[grepl("package", attached)]
  need_to_attach <- pkgs[which(!pkgs %in% gsub("package:", "", attached_pkgs))]
  
  if (length(need_to_attach) > 0) {
    for (i in 1:length(need_to_attach)) require(need_to_attach[i], character.only = TRUE)
  }
  
  if (length(need_to_attach) == 0) {
    message("\n ...Packages were already loaded!\n")
  }
}

depart <- function(bird_id, last_mins = 1/60) {
  dir_res <- 60
  dir_brk <- seq(-dir_res/2, 360, by = dir_res)  
  upper <- dir_brk[dir_brk > 0 & dir_brk < 360]
  lower <- c(360 - dir_res/2, upper[-1 * length(upper)])
  dir_lab <- paste0(lower, "-", upper)
  
  if (last_mins < 1) {
    time_frame <- paste(round(last_mins * 60, 1), "sec")
  } else if (last_mins < 60) {
    time_frame <- paste(last_mins, "min")
  } else {
    time_frame <- paste(round(last_mins / 60, 2), "h")
  }
  
  polar_dat <- filter(mywa_filt, id == bird_id,
                      difftime(max(ts), ts, units = "mins") <= last_mins) %>%
    mutate(dir_bin = cut(dir, breaks = dir_brk, 
                         ordered_result = TRUE,
                         labels = dir_lab),
           last_det = max(ts),
           stopover = max(round(since_rel, 1))) %>%
    group_by(site, last_det, stopover, dir_bin) %>% 
    summarise(count = n(),
              last_dir_det = max(ts)) %>%
    as.data.frame() %>%
    mutate(min_til_last_det = round(as.numeric(difftime(last_det, 
                                                        last_dir_det, 
                                                        units = "mins")), 3)) %>%
    arrange(min_til_last_det) %>%
    dplyr::select(-last_dir_det)
  
  p <- ggplot(polar_dat, aes(x = dir_bin, y = count)) +
    geom_bar(stat = "identity", color = "black", fill = "red", alpha = 0.5) +
    scale_x_discrete(drop = FALSE, labels = waiver()) +
    coord_polar(start = -((dir_res/2)/360) * 2 * pi) + 
    geom_text(aes(y = count/2, label = count)) +
    facet_grid(site ~ .) + 
    scale_y_continuous("", breaks = NULL) + 
    ggtitle(paste0("Tag ID: ", bird_id, 
                   "\nlast ", time_frame, " of detections",
                   "\nFinal detection: ", format(unique(polar_dat$last_det), 
                                                 format = "%d %b @ %H:%M"),
                   "\nStopover length: ", unique(polar_dat$stopover), " days")) +
    theme_bw() + 
    theme(axis.title.x = element_blank())
  
  print(p)
  
  return(polar_dat)
  
}