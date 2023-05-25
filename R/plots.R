#' make and save plots for use in assessing whale depredation
#'
#' @param depth_strat depth strata distinctions
#' @param spp_sum_strat summary of catch by species and depth strata
#' @param dep depredation evidence
#' @param catch catch data by skate
#'
#' @return
#' @export plot_data
#'
#' @examples
#' \dontrun{
#' plot_data(depth_strat = depth_strat, spp_sum_strat = spp_sum_strat, dep = dep, catch = catch
#' }
plot_data <- function(depth_strat = depth_strat, spp_sum_strat = spp_sum_strat, dep = dep, catch = catch) {
  nameColors <- c(RColorBrewer::brewer.pal(9, "Set1"), "black")
  names(nameColors) <- levels(spp_sum_strat$common_name)
  name_colScale <- ggplot2::scale_fill_manual(name = "Hook Accounting", values = nameColors)

  depColors <- c(RColorBrewer::brewer.pal(8, "Set2"), "black")
  names(depColors) <- levels(depth_strat$depth_stratum)
  dep_colScale <- ggplot2::scale_colour_manual(name = "Depth Stratum", values = depColors)

  plot1 <- ggplot2::ggplot(depth_strat, ggplot2::aes(hachi, intrpdep, color = depth_stratum)) +
    ggplot2::geom_point() +
    ggplot2::labs(col = "Depth Stratum") +
    ggplot2::xlab("Skate #") +
    ggplot2::ylab("Depth (m)") +
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_y_reverse() +
    dep_colScale

  depth_strat$effective <- 45 - depth_strat$ineffective

  cpue <- dplyr::left_join(depth_strat, spp_sum, by = c("station", "haul", "hachi"))

  cpue$cat_spp <- ifelse(is.na(cpue$cat_spp), 0, cpue$cat_spp)

  cpue$spp_cpue = cpue$cat_spp / cpue$effective

  cpue_sab <-  cpue %>%
    dplyr::filter(common_name == "Sablefish") %>%
    group_by(station, depth_stratum) %>%
    summarize(ds_mean = mean(spp_cpue), min_x = min(hachi), max_x = max(hachi))

  cpue_spp <-  cpue %>%
    dplyr::filter(common_name == "Sablefish") %>%
    dplyr::group_by(station, depth_stratum) %>%
    dplyr::summarize(ds_mean = mean(spp_cpue), min_x = min(hachi), max_x = max(hachi))

  roll <- cpue %>%
    dplyr::filter(common_name == "Sablefish") %>%
    dplyr::mutate(ma3 = zoo::rollmean(spp_cpue, 3, fill = NA))

  plot2 <- ggplot2::ggplot(cpue %>% filter(common_name == "Sablefish"), ggplot2::aes(hachi, spp_cpue, col = depth_stratum)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line(data = roll, ggplot2::aes(x = hachi, y = ma3), col = "blue") +
    ggplot2::scale_x_continuous(expand = c(0, 0))  +
    ggplot2::ylab("Sablefish CPUE") +
    ggplot2::xlab("Skate #") +
    dep_colScale +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_segment(data = cpue_sab, ggplot2::aes(x = min_x, xend = max_x, y = ds_mean, yend = ds_mean), linewidth = 0.5, lty = 2)

  plot3 <- ggplot2::ggplot(spp_sum_strat) +
    ggplot2::geom_col(ggplot2::aes(hachi, cat_spp, fill = common_name), col = "black") +
    ggplot2::geom_point(data = dep, ggplot2::aes(hachi, tot_dep), shape = 23, size = 6, fill = "white") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 5), lty = 2, col = "white") +
    ggplot2::labs(fill = "Hook Accounting") +
    ggplot2::xlab("Skate #") +
    ggplot2::ylab("Cumulative Catch") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2)) +
    name_colScale

  plot <- cowplot::plot_grid(nrow = 3, plot1, plot2, plot3, rel_heights = c(1, 1, 2))
  st = unique(catch$station)
  file <- paste0(getwd(), "/Station_", st, ".png")

  cowplot::save_plot(file, plot, base_height = 12)
  return(plot)
}


