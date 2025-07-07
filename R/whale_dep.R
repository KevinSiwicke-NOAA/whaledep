#' wrapper function to produce plots
#'
#' @param channel where the at-sea Access database is
#' @param station station number
#'
#' @return plots for viewing
#' @export whale_dep
#'
#' @examples
#' \dontrun{
#' whale_dep(channel, station)
#' }
whale_dep <- function(channel, station) {

  dat <- whaledep:::get_data(channel = channel, sta_num = station)

  plot_dat <- whaledep:::make_plot_data(depth = dat[[1]], cat = dat[[2]], names = dat[[3]], strat = dat[[4]])

  cpue_sab_strat <- plot_dat[[7]]
  write.csv(cpue_sab_strat[, 1:5], paste0(station, '_cpue.csv'))

  plt <- whaledep:::plot_data(depth_strat = plot_dat[[1]],
                              spp_sum_strat = plot_dat[[2]],
                              dep = plot_dat[[3]],
                              catch = plot_dat[[4]],
                              spp_sum = plot_dat[[5]],
                              cpue = plot_dat[[6]],
                              cpue_sab_strat = cpue_sab_strat,
                              roll = plot_dat[[8]])

  prev_cpue_csv <- list.files(path = paste0(getwd(), "/cpue"), pattern = "*.csv", full.names = TRUE)
  prev_cpue <- prev_cpue_csv %>% purrr::map_dfr(~readr::read_csv(., show_col_types = FALSE))
  stn_area = dat[[5]]

  all_cpue <- dplyr::bind_rows(prev_cpue, cpue_sab_strat) %>%
    dplyr::left_join(stn_area) %>%
    dplyr::filter(!is.na(depth_stratum)) %>%
    dplyr::mutate(depth_stratum = factor(depth_stratum), levels = c("0-100 m", "101-200 m", "201-300 m", "301-400 m", "401-600 m", "601-800 m", "801-1000 m", "1001-1200 m"))

  stn_area = dat[[5]]

  cpue_plt <- ggplot2::ggplot(all_cpue, ggplot2::aes(depth_stratum, ds_mean, size = num_skates)) +
    ggplot2::geom_text(ggplot2::aes(label = station), position = 'jitter') +
    ggplot2::labs(x = "Depth strata", y = "Station CPUE", size = "# Skates") +
    ggplot2::facet_wrap(~area_id)

  ggplot2::ggsave(plot = cpue_plt, filename = "cpue_compare.png", height = 8, width = 8)
}
