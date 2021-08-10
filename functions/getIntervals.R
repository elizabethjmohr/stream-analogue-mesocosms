# Get prediction intervals for by calculating the quantiles for each time
getPI <- function(ensemble, varName, times){
  pi <- apply(ensemble[[varName]],2,quantile,probs = c(0.025,0.5,0.975), na.rm = TRUE) %>%
    t() %>%
    as_tibble() %>%
    mutate(times = times/60) %>%
    rename("Lower" = "2.5%", "Median" = "50%", "Upper" = "97.5%") %>% 
    mutate(delta = ensemble$deltaNO30, NO3Pre = ensemble$NO3Pre, k2 = ensemble$k2)
  return(pi)
}

# Plot prediction intervals
plotPI <- function(PI, xFacet = "NO3Pre", yFacet = "delta"){
  plot <- ggplot(data = PI, aes(x = times)) +
    geom_ribbon(aes(x = times, ymin = Lower, ymax = Upper), fill = "grey80") + 
    geom_line(aes(y = Median)) +
    xlab("Time (hours)") +
    theme_minimal()
  return(plot)
}