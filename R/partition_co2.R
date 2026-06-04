
### IN PROGRESS
# Remember to add @export to all of these when complete


## CO2 flux partitioning: NEE → GPP + Reco
#
## Load packages
#library(tidyverse)
#library(data.table)
#
## Load slopes
#df <- read_csv("https://raw.githubusercontent.com/abbylewis/Chapada_flux_data/refs/heads/master/processed_data/L0.csv", show_col_types = F) %>%
#  rename(Chamber = Fluxing_Chamber)
#met <- read_csv("https://raw.githubusercontent.com/abbylewis/Chapada_flux_data/refs/heads/master/processed_data/met_2025_dashboard.csv")
#
## Format
#df$DateTime <- as.POSIXct(df$TIMESTAMP, tz = "EST")
#met$DateTime <- as.POSIXct(met$TIMESTAMP, tz = "EST")
#
## Convert to data.table
#setDT(df) 
#setDT(met)
#
## Set keys: DateTime is what will be used to join fluxes with met
#setkey(df, DateTime)
#setkey(met, DateTime)
#
##Join and format
#merged <- met[df, roll = "nearest"] %>% # Rolling join: nearest met to each flux
#  rename(Ta = AirT_C_Avg,
#         PAR = SlrFD_W_Avg) 
#
#chamber_height = 156 # cm
#chamber_radius = 25 # cm
#
#
#

#' CO2 flux partitioning: NEE → GPP + Reco
#'
#' @param merged Dataframe of fluxes merged with drivers
#' @param chamber_height_cm Chamber height (cm)
#' @param chamber_radius_cm Chamber radius (cm)
#' @param group_cols Optional grouping columns
#' @param par_night_thresh Threshold of PAR that constitutes night/no photosynthesis
#' @param window_days Moving window days for respiration calcs
#' @param step_days Timestemp of respiration calcs
#'
#' @returns Data frame with partitioned CO2
#'
#' @examples
partition_co2 <- function(merged,
                          chamber_height_cm,
                          chamber_radius_cm,
                          group_cols = NULL,
                          par_night_thresh = 5,
                          window_days = 30,
                          step_days = 1) {
  
  chamber_area_m2 = pi*(chamber_radius_cm/100)^2 # m2
  chamber_volume_L = chamber_height_cm/100 * # m
    chamber_area * 1000 #L
  
  setDT(merged)
  
  group_vars <- c("Chamber", group_cols) |> purrr::discard(is.null)
  
  # --- Flux calculations ---
  merged[, `:=`(
    NEE = CO2_slope_ppm_per_day *
      chamber_volume_L / 
      (0.08206 * (Ta + 273.15)) / 
      (60*60*24) /
      chamber_area_m2,
    
    CH4 = CH4_slope_ppm_per_day *
      chamber_volume_L / 
      (0.08206 * (Ta + 273.15)) / 
      (60*60*24) /
      chamber_area_m2,
    
    is_night = PAR < par_night_thresh
  )]
  
  # --- Moving window parameter estimation ---
  params_dt <- merged[, {
    
    dt_ch <- .SD
    
    start_time <- min(dt_ch$DateTime, na.rm = TRUE)
    end_time   <- max(dt_ch$DateTime, na.rm = TRUE)
    
    centers <- seq(start_time, end_time, by = paste0(step_days, " days"))
    
    res <- rbindlist(lapply(centers, function(center) {
      
      half_window <- window_days / 2
      
      wnd <- dt_ch[
        DateTime >= center - as.difftime(half_window, units = "days") &
        DateTime <= center + as.difftime(half_window, units = "days")
      ]
      
      wnd_night <- wnd[
        is_night == TRUE &
        is.finite(NEE) & NEE > 0 &
        is.finite(Ta)
      ]
      
      fit <- fit_q10_lm(wnd_night)
      
      if (!is.null(fit)) {
        data.table(
          center = center,
          Rref = fit$Rref,
          Q10 = fit$Q10,
          n_night = fit$n
        )
      } else {
        data.table(
          center = center,
          Rref = NA_real_,
          Q10 = NA_real_,
          n_night = nrow(wnd_night)
        )
      }
    }))
    
    res
    
  }, by = group_vars]
  
  # Limit cols
  params_dt <- params_dt[, c(group_vars, "center", "Rref", "Q10"), with = FALSE]
  
  # --- Linear interpolation ---
  merged[, c("Rref_t", "Q10_t") := {
    
    pch <- params_dt[.BY, on = group_vars]
    pch <- pch[!is.na(Rref) & !is.na(Q10)]
    
    if (nrow(pch) < 2) {
      list(rep(NA_real_, .N), rep(NA_real_, .N))
    } else {
      pch <- unique(pch, by = "center")
      
      x  <- as.numeric(pch$center)
      xt <- as.numeric(DateTime)
      
      list(
        approx(x, pch$Rref, xt, rule = 2, ties = "ordered")$y,
        approx(x, pch$Q10,  xt, rule = 2, ties = "ordered")$y
      )
    }
    
  }, by = group_vars]
  
  # --- Reco + GPP ---
  Tref <- 10
  
  merged[, Reco := Rref_t * (Q10_t ^ ((Ta - Tref)/10))]
  
  merged[, `:=`(
    GPP = fifelse(is_night, NA_real_, pmax(Reco - NEE, 0))
  )]
  
  return(merged)
}

#' Fit Q10
#'
#' @param dt_night 
#' @param Tref 
#' @param min_night 
#'
#' @returns List of fitted parameters
#'
fit_q10_lm <- function(dt_night, Tref = 10, min_night = 5) {
  # dt_night: data.table with columns NEE, Ta; NEE must be > 0
  if (nrow(dt_night) < min_night) return(NULL)
  dt_night <- dt_night[NEE > 0 & is.finite(Ta)]
  if (nrow(dt_night) < min_night) return(NULL)
  X <- dt_night[, Ta - Tref]
  Y <- log(dt_night$NEE)
  fit <- try(lm(Y ~ X), silent = TRUE)
  if (inherits(fit, "try-error")) return(NULL)
  coef <- coefficients(fit)
  a <- coef[1]; b <- coef[2]
  Rref <- exp(a)
  Q10 <- exp(b * 10)
  return(list(Rref = as.numeric(Rref), Q10 = as.numeric(Q10), n = nrow(dt_night), fit = fit))
}

#' Moving-window parameter estimation per chamber
#'
#' @param dt_ch 
#' @param window_days 
#' @param step_days 
#' @param par_night_thresh 
#' @param Tref 
#'
#' @returns Dataframe with estimated parameters
#'
estimate_params_moving_window <- function(
    dt_ch, window_days = 30, step_days = 1, 
    par_night_thresh = 5, Tref = 10) {
  # dt_ch: data.table for one chamber
  if (nrow(dt_ch) == 0) return(NULL)
  start_time <- min(dt_ch$DateTime, na.rm = TRUE)
  end_time   <- max(dt_ch$DateTime, na.rm = TRUE)
  centers <- seq(from = start_time, to = end_time, by = paste0(step_days, " days"))
  res_list <- vector("list", length(centers))
  for (i in seq_along(centers)) {
    center <- centers[i]
    wstart <- center - as.difftime(window_days/2, units = "days")
    wend   <- center + as.difftime(window_days/2, units = "days")
    wnd <- dt_ch[DateTime >= wstart & DateTime <= wend]
    # nighttime points (PAR-based)
    wnd_night <- wnd[PAR < par_night_thresh & is.finite(NEE) & NEE > 0 & is.finite(Ta)]
    fit <- fit_q10_lm(wnd_night, Tref = Tref)
    if (!is.null(fit)) {
      res_list[[i]] <- data.table(Chamber = dt_ch$Chamber[1],
                                  center = center,
                                  Rref = fit$Rref,
                                  Q10 = fit$Q10,
                                  n_night = fit$n)
    } else {
      res_list[[i]] <- data.table(Chamber = dt_ch$Chamber[1],
                                  center = center,
                                  Rref = NA_real_,
                                  Q10 = NA_real_,
                                  n_night = ifelse(is.null(wnd_night), 0, nrow(wnd_night)))
    }
  }
  res_dt <- rbindlist(res_list)
  # drop centers with NA Rref & Q10? Keep for interpolation (will be NA)
  return(res_dt)
}

#out <- partition_co2(merged, chamber_volume_L, chamber_area_m2, group_cols = "location")
