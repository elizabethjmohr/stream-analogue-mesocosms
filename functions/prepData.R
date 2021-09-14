prepData <- function(data, Q, V){
  
  num_dates <- length(table(as.Date(data$dateTime)))
  
  data_list = c(
    list(
      GPP_daily_mu = 20,
      GPP_daily_sigma,
      ER_daily_mu,
      ER_daily_sigma,
      K600_daily_meanlog,
      K600_daily_sdlog,
      d = num_dates, 
      Q = Q,
      V = V,
      err_obs_iid_sigma_scale,
      err_proc_iid_sigma_scale,
      d, 
      timestep = 1/(6*24),
      n24 = n24, 
      n = num_daily_obs,
      DO_obs_1, 
      DO_obs,
      DO_sat,
      light_mult_GPP,
      KO2_conv
    ),
    list(
      DO_obs_1 = array(time_by_date_matrix(data$DO.obs)[1,], dim=num_dates)), # duplication of effort below should be small compared to MCMC time
    
    # Every timestep
    switch(
      features$GPP_fun,
      linlight = list(
        # X_mult_Y syntax: X = process reflected by multiplier, Y = quantity
        # modified by multiplier
        light_mult_GPP = {
          mat_light <- time_by_date_matrix(data$light)
          # normalize light by the sum of light in the first 24 hours of the time window
          in_solar_day <- apply(obs_times, MARGIN=2, FUN=function(timevec) {timevec - timevec[1] <= 1} )
          daily_totals <- colSums(mat_light*in_solar_day)
          if(any(daily_totals <= 0)) {
            stop('daily light total is <= 0 on ', paste(names(date_table)[which(daily_totals <= 0)], collapse=', '))
          }
          sweep(mat_light, MARGIN=2, STATS=daily_totals, FUN=`/`) / timestep_days
        }),
      satlight = list(
        light = time_by_date_matrix(data$light)
      )
    ),
    
    list(
      # X_mult_Y syntax: X = process reflected by multiplier, Y = quantity
      # modified by multiplier
      const_mult_ER  = time_by_date_matrix(1),
      KO2_conv = {
        KO2_conv_vec <- suppressWarnings(convert_k600_to_kGAS(k600=1, temperature=data$temp.water, gas="O2"))
        if(any(is.nan(KO2_conv_vec))) {
          bad_rows <- which(is.nan(KO2_conv_vec))
          show_rows <- bad_rows[seq_len(min(length(bad_rows), 3))]
          more_rows <- if(length(bad_rows) > 3) length(bad_rows) - 3 else NA
          bad_times <- data$solar.time[show_rows]
          bad_temps <- data$temp.water[show_rows]
          stop(sprintf(
            'NaNs in KO2-K600 conversion at %s%s',
            paste0(sprintf('%s (temp.water=%0.2f)', format(bad_times, '%Y-%m-%d %H:%M:%S'), bad_temps), collapse=', '),
            if(!is.na(more_rows)) sprintf(', and %d more rows', more_rows) else ''
          ))
        }
        time_by_date_matrix(KO2_conv_vec)
      },
      depth    = time_by_date_matrix(data$depth),
      DO_sat   = time_by_date_matrix(data$DO.sat),
      DO_obs   = time_by_date_matrix(data$DO.obs)
    ),
    
    specs[specs$params_in]
  )
  if(features$pool_K600_type == 'binned') {
    data_list$K600_lnQ_nodes_meanlog <- array(data_list$K600_lnQ_nodes_meanlog, dim=data_list$b)
    data_list$K600_lnQ_nodes_sdlog <- array(data_list$K600_lnQ_nodes_sdlog, dim=data_list$b)
  }
  
  # check that the params_out are unique (non-unique messes up our parsing of
  # the stanfit output)
  if(length(specs$params_out) != length(unique(specs$params_out))) {
    stop('params_out must all be unique')
  }
  
  data_list
}

