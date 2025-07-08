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
  write.csv(cpue_sab_strat[, 1:5], paste0('cpue/', station, '_cpue.csv'), row.names = FALSE)

  plt <- whaledep:::plot_data(depth_strat = plot_dat[[1]],
                              spp_sum_strat = plot_dat[[2]],
                              dep = plot_dat[[3]],
                              catch = plot_dat[[4]],
                              spp_sum = plot_dat[[5]],
                              cpue = plot_dat[[6]],
                              cpue_sab_strat = cpue_sab_strat,
                              roll = plot_dat[[8]])

  stn <- station
  cpue_csv <- list.files(path = paste0(getwd(), "/cpue"), pattern = "*.csv", full.names = TRUE)
  cpue_dat <- cpue_csv %>% purrr::map_dfr(~readr::read_csv(., show_col_types = FALSE)) %>%
    dplyr::mutate(type = ifelse(station == stn, "cur", "prev"))
  stn_area = dat[[5]]
  strat = dat[[4]]

  all_cpue <- cpue_dat %>%
    dplyr::left_join(stn_area) %>%
    dplyr::filter(!depth_stratum %in% c("0-100 m", "1201-32000 m")) %>%
    dplyr::mutate(depth_stratum = factor(depth_stratum, levels = c("101-200 m", "201-300 m", "301-400 m", "401-600 m", "601-800 m", "801-1000 m"))) %>%
    dplyr::left_join(strat) %>%
    dplyr::mutate(strat = factor(startdepth))

  cpue_plt <- ggplot2::ggplot() +
    ggplot2::geom_text(data = all_cpue %>% dplyr::filter(type == 'prev'), ggplot2::aes(strat, ds_mean, label = station), position = ggplot2::position_jitter(width = 0.15, height = 0), size = 8) +
    ggplot2::geom_text(data = all_cpue %>% dplyr::filter(type == 'cur'), ggplot2::aes(strat, ds_mean, label = station), col = 'firebrick', size = 10) +
    ggplot2::labs(x = "Depth stratum", y = "Station CPUE") +
    ggplot2::facet_wrap(~area_id) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0), lty = 2) +
    ggplot2::scale_y_continuous(expand = c(0, 0.01), limits = c(0, max(all_cpue$ds_mean + 0.02)), breaks = seq(0, max(all_cpue$ds_mean + 0.02), by = 0.1)) +
    ggplot2::theme_bw()

  ggplot2::ggsave(plot = cpue_plt, filename = "cpue_compare.png", height = 16, width = 16)
}
