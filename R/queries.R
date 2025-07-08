#' query the at-sea Access database for catch data
#'
#' @param channel channel to the at-sea database
#' @param sta_num station number
#'
#' @return catch and depth data by skate
#'
#' @examples
#' \dontrun{
#' get_data(channel, sta_num = 70)
#' }
get_data <- function(channel, sta_num ) {
  depth <- RODBC::sqlQuery(channel, base::paste0("select * from Depth where station = ", sta_num)) %>%
           dplyr::rename_all(tolower)

  cat <- RODBC::sqlQuery(channel, base::paste0("select * from Catch where station = ", sta_num)) %>%
         dplyr::rename_all(tolower)

  names <- RODBC::sqlQuery(channel, base::paste0("select * from \"Sppcode Catch\"")) %>%
    dplyr::rename_all(tolower)

  strat <- RODBC::sqlQuery(channel, base::paste0("select * from DepthStratum")) %>%
    dplyr::rename_all(tolower)

  stn_area <- RODBC::sqlQuery(channel, base::paste0("select * from Stations")) %>%
    dplyr::rename_all(tolower) |>
    dplyr::rename(station = station_number)

  dat_list <- list(depth, cat, names, strat, stn_area)
}

#' prepare data for plots
#'
#' @param depth depth by skate
#' @param cat catch by skate
#' @param strat depth strata
#' @param names names of the species from codes
#'
#' @return list of data used for making plots
#'
#' @examples
#' \dontrun{
#' make_plot_data(depth, cat, strat, names)
#' }
make_plot_data <- function(depth, cat, strat, names) {
  strat$depth_stratum <- base::paste0(strat$startdepth, "-", strat$enddepth, " m")

  catch <- dplyr::left_join(cat, names, by = c("species_code" = "species code")) %>%
    dplyr::rename(common_name = "common name")

  # Make rarer species into an "Other" category
  catch$common_name = base::ifelse(catch$species_code %in% c(20510, 21720, 21230, 10120, 10111, 30051, 30020), catch$common_name, "Other")

  # Summarize and combine catch/depth
  spp_sum <- catch %>%
    dplyr::group_by(station, haul, hachi, common_name) %>%
    dplyr::summarize(cat_spp = sum(catch_freq))

  bait_sum <- depth %>%
    dplyr::filter(baited > 0) %>%
    dplyr::group_by(station, haul, hachi) %>%
    dplyr::summarize(common_name = "Bait", cat_spp = sum(baited))

  inef_sum <- depth %>%
    dplyr::filter(ineffective > 0) %>%
    dplyr::group_by(station, haul, hachi) %>%
    dplyr::summarize(common_name = "Ineffective", cat_spp = sum(ineffective))

  all_sum <- dplyr::bind_rows(spp_sum, bait_sum, inef_sum)
  spp_sum_strat <- dplyr::left_join(all_sum, depth, by=c("station", "haul", "hachi")) %>%
    dplyr::mutate(common_name = factor(common_name, levels=c("Sablefish", "Pacific cod",
                                                             "Pacific halibut", "Atheresthes sp.", "Rougheye, shortraker rockfish",
                                                             "Shortspine thornyhead", "Giant grenadier", "Other", "Bait", "Ineffective")))

  depth_strat <- dplyr::left_join(depth, strat, by = "stratum") %>%
    dplyr::mutate(depth_stratum = factor(depth_stratum, levels=c("0-100 m", "101-200 m",
                                                                 "201-300 m",  "301-400 m",  "401-600 m",  "601-800 m",  "801-1000 m",
                                                                 "1001-1200 m", "1201-32000 m")))

  dep <- catch %>%
    dplyr::filter(depredated_freq > 0) %>%
    dplyr::group_by(station, haul, hachi) %>%
    dplyr::summarize(tot_dep = sum(depredated_freq))

  depth_strat$effective <- 45 - depth_strat$ineffective

  cpue <- dplyr::left_join(depth_strat, spp_sum %>% dplyr::filter(common_name == "Sablefish")) %>%
    dplyr::mutate(common_name = "Sablefish",
                  skate_eff = ifelse(ineffective > 5, 'ineff', 'eff'),
                  cat_spp = ifelse(is.na(cat_spp), 0, cat_spp),
                  spp_cpue = ifelse(cat_spp == 0, 0, cat_spp / effective))

  cpue_sab_strat <-  cpue %>%
    dplyr::filter(skate_eff == 'eff') |>
    dplyr::group_by(station, depth_stratum) %>%
    dplyr::summarize(ds_mean = mean(spp_cpue), ds_var = var(spp_cpue), num_skates = dplyr::n(), min_x = min(hachi), max_x = max(hachi))

  roll <- cpue %>%
    dplyr::arrange(hachi) %>%
    dplyr::mutate(ma3 = zoo::rollmean(spp_cpue, 3, fill = NA))

  plot_data <- list(depth_strat, spp_sum_strat, dep, catch, spp_sum, cpue, cpue_sab_strat, roll)
}
