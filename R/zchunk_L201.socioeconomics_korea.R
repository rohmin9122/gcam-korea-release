#' module_gcam.korea_L201.socioeconomics
#'
#' Interest rate, population, and GDP for gcam-korea.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L201.InterestRate_GCAM_korea}, \code{L201.Pop_GCAM_korea}, \code{L201.BaseGDP_GCAM_korea}, \code{L201.LaborForceFillout_GCAM_korea}, \code{L201.LaborProductivity_GCAM_korea}. The corresponding file in the
#' original data system was \code{L201.socioeconomics_USA.R} (gcam-usa level2).
#' @details Interest rate, population, and GDP for gcam-korea.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author M. Roh
module_gcam.korea_L201.socioeconomics <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-korea/states_subregions",
             "L100.Pop_thous_korea",
             "L100.GDP_mil90usd_korea",
             "L100.pcGDP_thous90usd_korea",
             "L102.pcgdp_thous90USD_GCAM3_ctry_Y"
             ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L201.InterestRate_GCAM_korea",
             "L201.Pop_GCAM_korea",
             "L201.BaseGDP_GCAM_korea",
             "L201.LaborForceFillout_GCAM_korea",
             "L201.LaborProductivity_GCAM_korea"))
  } else if(command == driver.MAKE) {

    # silence package checks
    year <- value <- state <- totalPop <- baseGDP <- iso <- growth <- timestep <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-korea/states_subregions")
    L100.Pop_thous_state <- get_data(all_data, "L100.Pop_thous_korea")
    L100.GDP_mil90usd_state <- get_data(all_data, "L100.GDP_mil90usd_korea")
    L100.pcGDP_thous90usd_korea <- get_data(all_data, "L100.pcGDP_thous90usd_korea")
    L102.pcgdp_thous90USD_GCAM3_ctry_Y<- get_data(all_data, "L102.pcgdp_thous90USD_GCAM3_ctry_Y")
    # ===================================================
    # NOTE: Socioeconomics for grid regions are dealt with in module_gcam.usa_L223.electricity_USA

    # L201.InterestRate: Interest rates by region
    L201.InterestRate <- tibble(region = states_subregions$state, interest.rate = socioeconomics.DEFAULT_INTEREST_RATE)

    # L201.Pop_GCAMUSA: Population by region from the GCAM 3.0 core scenario
    L201.Pop_GCAM_korea <- L100.Pop_thous_state %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(totalPop = value,
             region = state) %>%
      mutate(totalPop = round(totalPop, socioeconomics.POP_DIGITS))

    # L201.BaseGDP_GCAMUSA: Base GDP for GCAM-USA scenario
    L201.BaseGDP_GCAM_korea <- L100.GDP_mil90usd_state %>%
      filter(year == min(MODEL_YEARS)) %>%
      rename(baseGDP = value, region = state) %>%
      mutate(baseGDP = round(baseGDP, socioeconomics.GDP_DIGITS)) %>%
      # Ulsan was promoted to metropolitan city in 1997. So, the base gdp (1975) is zero
      # 0.000001 is a very small number, that will prevent model error
      mutate(baseGDP=if_else(baseGDP==0, 0.000001, baseGDP)) %>%
      select(-year)

    # L201.LaborForceFillout: Labor force participation and productivity for all scenarios
    # NOTE: No model of labor force used; labor force participation set to a constant
    L201.LaborForceFillout <- tibble(region = states_subregions$state,
                                     year.fillout = min(MODEL_YEARS),
                                     laborforce = socioeconomics.DEFAULT_LABORFORCE)


    popShare_korea <- L100.Pop_thous_state %>%
      filter(year %in% MODEL_YEARS) %>%
      group_by(year) %>%
      mutate(popShare=value/sum(value)) %>%
      select(state, year, popShare, -value) %>%
      ungroup

    pcGDP_korea <- popShare_korea %>%
      left_join_error_no_match(L102.pcgdp_thous90USD_GCAM3_ctry_Y %>%
                                 filter(tolower(iso) == "kor", year %in% MODEL_YEARS), by = "year") %>%
      mutate(pcGDP=popShare*value) %>%
      select(state, year, pcGDP)


    # LABOR PRODUCTIVITY GROWTH RATE CALCULATION
    # Labor productivity growth is calculated from the change in per-capita GDP ratio in each time period
    # Calculate the growth rate in per-capita GDP
    L201.LaborProductivity_GCAM_korea <- pcGDP_korea %>%
      # In order to calculate growth rate we need to know how much GDP grew and number of years between periods
      mutate(growth = pcGDP / lag(pcGDP),
             timestep = year - lag(year),
             laborproductivity = round(growth ^ (1 / timestep) - 1, socioeconomics.LABOR_PRODUCTIVITY_DIGITS)) %>%
      # Remove the first model year, since it has no previous period to calculate growth rate
      filter(year != min(MODEL_YEARS)) %>%
      replace_na(list(laborproductivity = 0)) %>%
      ungroup %>%
      # Renaming to region because region column needed for write_to_all_states
      rename(region=state)%>%
      select(region, year, laborproductivity)

    #L201.LaborProductivity_GCAM_korea[is.infinite(L201.LaborProductivity_GCAM_korea$laborproductivity),"laborproductivity"] <- 0

    # ===================================================

    # Produce outputs
    L201.InterestRate %>%
      add_title("Interest rates by state") %>%
      add_units("Unitless") %>%
      add_comments("Constant assumed for all states") %>%
      add_legacy_name("L201.InterestRate_GCAM_korea") %>%
      add_precursors("gcam-korea/states_subregions") ->
      L201.InterestRate_GCAM_korea

    L201.Pop_GCAM_korea %>%
      add_title("Population by state") %>%
      add_units("thousand persons") %>%
      add_comments("Data from L100.Pop_thous_korea") %>%
      add_legacy_name("L201.Pop_GCAM_korea") %>%
      add_precursors("L100.Pop_thous_korea") ->
      L201.Pop_GCAM_korea

    L201.BaseGDP_GCAM_korea %>%
      add_title("Base year GDP by state") %>%
      add_units("million 1990 USD") %>%
      add_comments("Data from L100.GDP_mil90usd_korea") %>%
      add_legacy_name("L201.BaseGDP_GCAM_korea") %>%
      add_precursors("L100.GDP_mil90usd_korea") ->
      L201.BaseGDP_GCAM_korea

    L201.LaborForceFillout %>%
      add_title("Labor force participation and productivity for all scenarios") %>%
      add_units("Unitless") %>%
      add_comments("Constant value assumed") %>%
      add_legacy_name("L201.LaborForceFillout") %>%
      add_precursors("gcam-korea/states_subregions") ->
      L201.LaborForceFillout_GCAM_korea

    L201.LaborProductivity_GCAM_korea %>%
      add_title("Labor force productivity growth rate for GCAM-Korea") %>%
      add_units("Unitless (annual rate of growth)") %>%
      add_comments("Values from L100.pcGDP_thous90usd_korea used to calculate annual growth") %>%
      add_comments("Korea value written to all states") %>%
      add_legacy_name("L201.LaborProductivity_GCAM_korea") %>%
      add_precursors("L100.pcGDP_thous90usd_korea",
                     "L102.pcgdp_thous90USD_GCAM3_ctry_Y",
                     "L100.Pop_thous_korea") ->
      L201.LaborProductivity_GCAM_korea


    return_data(L201.InterestRate_GCAM_korea, L201.Pop_GCAM_korea, L201.BaseGDP_GCAM_korea, L201.LaborForceFillout_GCAM_korea, L201.LaborProductivity_GCAM_korea)
  } else {
    stop("Unknown command")
  }
}
