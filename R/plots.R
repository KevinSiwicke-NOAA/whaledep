#' make and save plots for use in assessing whale depredation
#'
#' @param depth_strat depth strata distinctions
#' @param spp_sum_strat summary of catch by species and depth strata
#' @param dep depredation evidence
#' @param catch catch data by skate
#'
#' @return ggplot/cowplot graphic
#'
#' @examples
#' \dontrun{
#' plot_data(depth_strat, spp_sum_strat, dep, catch, spp_sum, cpue, cpue_sab_strat, roll)
#' }
plot_data <- function(depth_strat, spp_sum_strat, dep, catch, spp_sum, cpue, cpue_sab_strat, roll) {
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
    ggplot2::scale_x_continuous(expand = c(0,0), limits = c(0, max(depth_strat$hachi) + 1), breaks = seq(1, max(depth_strat$hachi), by = 5)) +
    ggplot2::theme(legend.position = "bottom",
                   panel.background = ggplot2::element_rect(fill = "white", colour = NA),
                   panel.border = ggplot2::element_rect(fill = NA, colour="grey50"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90", size = 0.2),
                   panel.grid.minor = ggplot2::element_line(colour = "grey98", size = 0.5),
                   panel.margin = ggplot2::unit(0.25, "lines")) +
    ggplot2::guides(col = ggplot2::guide_legend(nrow = 1)) +
    ggplot2::scale_y_reverse() +
    dep_colScale

  plot2 <- ggplot2::ggplot(cpue, ggplot2::aes(hachi, spp_cpue, col = depth_stratum)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line(data = roll, ggplot2::aes(x = hachi, y = ma3), col = "blue") +
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0, max(depth_strat$hachi) + 1), breaks = seq(1, max(depth_strat$hachi), by = 5)) +
    ggplot2::ylab("Sablefish CPUE") +
    ggplot2::xlab("Skate #") +
    dep_colScale +
    ggplot2::theme(legend.position = "none",
                   panel.background = ggplot2::element_rect(fill = "white", colour = NA),
                   panel.border = ggplot2::element_rect(fill = NA, colour="grey50"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90", size = 0.2),
                   panel.grid.minor = ggplot2::element_line(colour = "grey98", size = 0.5),
                   panel.margin = ggplot2::unit(0.25, "lines")) +
    ggplot2::geom_segment(data = cpue_sab_strat, ggplot2::aes(x = min_x, xend = max_x, y = ds_mean, yend = ds_mean), linewidth = 0.5, lty = 2)

  plot3 <- ggplot2::ggplot(spp_sum_strat) +
    ggplot2::geom_col(ggplot2::aes(hachi, cat_spp, fill = common_name), col = "black") +
    ggplot2::geom_point(data = dep, ggplot2::aes(hachi, tot_dep), shape = 23, size = 6, fill = "white") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 5), lty = 2, col = "white") +
    ggplot2::labs(fill = "Hook Accounting") +
    ggplot2::xlab("Skate #") +
    ggplot2::ylab("Cumulative Catch") +
    ggplot2::theme(legend.position = "bottom",
                   panel.background = ggplot2::element_rect(fill = "white", colour = NA),
                   panel.border = ggplot2::element_rect(fill = NA, colour="grey50"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90", size = 0.2),
                   panel.grid.minor = ggplot2::element_line(colour = "grey98", size = 0.5),
                   panel.margin = ggplot2::unit(0.25, "lines")) +
    ggplot2::scale_x_continuous(expand = c(0,0), limits = c(0, max(depth_strat$hachi) + 1), breaks = seq(1, max(depth_strat$hachi), by = 5)) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2)) +
    name_colScale

  plt <- cowplot::plot_grid(nrow = 3, plot1, plot2, plot3, rel_heights = c(1, 1, 2))
  st = unique(catch$station)
  file <- paste0(getwd(), "/Station_", st, ".png")

  cowplot::save_plot(file, plt, base_height = 12)
  return(plt)
}
