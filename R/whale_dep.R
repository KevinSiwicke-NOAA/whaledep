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
}
