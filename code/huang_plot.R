################################################################################

## Load relevant packages ##
library(tidyverse)
library(ggthemes)
library(extrafont)

extrafont::loadfonts()

################################################################################

#' @title Generate relative probabilities
#' 
#' @description
#' `rel_probs` generates binned relative probabilities by transforming 
#' waiting times using the sigmoid function. 
#' 
#' @param sim_output [lst] output from running `simulate_nav`
#' @param resource [str] resource (e.g. angio_inr, ct, etc.)
#' 
#' @return data frame of relative probabilities and the resource

rel_probs <- function(sim_output, resource){
  
  # Prepare data #
  data <- sim_output[[1]] # extract arrivals data
  data$wait_time[which(data$wait_time < 0)] <- 0 # -ve set to 0
  data <- data %>% mutate(wait = wait_time > 0) # wait ? (bool)

  # Wait time > 0 for resource #
  x1 <- data$wait_time[which(data$resource == resource & 
                               data$wait == TRUE)]
  # Wait time = 0 for resource # 
  x2 <- data$wait_time[which(data$resource == resource &
                             data$wait == FALSE)]
  
  # Seq. inputs #
  from <- 0
  to <- max(x1)
  by <- to/500
  
  # Bins and midpoints #
  x <- cut(x1, seq(from,to,by), include.lowest=TRUE) # cut x1 into bins
  vec <- seq(from+by/2, to-by/2, by) # midpoints
  names(vec) <- levels(x) # matching names to levels
  cut_vec <- unname(vec[x]) # map names of bins to midpoints
  
  # Transform to relative probabilities #
  y <- table(cut_vec)/length(x2) # take ratios
  df2 <- as.data.frame( (2/(1+exp(-y)) - 1) ) # sigmoid transform
  df2[,1] <- as.numeric(as.character(df2[,1])) # change to num.
  df2 <- rbind(c(0,1), df2, c(200,1e-10)) # add value for wait_time = 0 & 200
  df2 <- cbind(df2, rep(resource, times = nrow(df2))) # add resource 
  colnames(df2)[3] <- 'resource'

  df2
}

################################################################################

#' @title Reproducing figure 2 from Huang et al., 2019
#' 
#' @description 
#' `plot_func` returns a plot of similar form to figure 2 from
#' the Huang et al., 2019 paper. Note: requires the use of 
#' `rel_probs`.
#' 
#' @param sim_lst [lst] list of `simulate_nav` outputs
#' @param scenarios [vec] vector of scenarios in the order of `sim_lst`
#' 
#' @return plot of the relative wait time probabilities

plot_func <- function(sim_lst, scenarios){
  
  n <- length(sim_lst) # length of list
  plot_df <- data.frame() # empty df for storage
  
  resources <- c('angio_inr', 'angio_staff', 'ct', 
                    'ed_staff', 'inr', 'stroke_doctor')
  
  for (i in 1:n) { # for each df in df_lst
    
    sim_output <- sim_lst[[i]] # reassign name
     
    rel_prob_df <- data.frame() # empty df for storage
    
    # Obtain relative probabilities 
    for (j in resources) { # for each resource
      rel_prob_df <- rbind(rel_prob_df, rel_probs(sim_output,j))
    }
    
    # Assign scenario names to data 
    rel_prob_df <- cbind(rel_prob_df, 
                         rep(scenarios[i], nrow(rel_prob_df)))
    colnames(rel_prob_df)[ncol(rel_prob_df)] <- 'scenario'
    plot_df <- rbind(plot_df, rel_prob_df)
  }
  
  # Produce plot
  plt <- ggplot(plot_df) + 
    geom_line(aes(x = cut_vec, y = Freq, col = resource),
              linewidth = 0.75, alpha = 0.9) +
    facet_wrap(vars(factor(scenario, c(scenarios)))) +
    xlim(0,200) +
    coord_trans(x = 'sqrt', y = 'sqrt') + 
    xlab('Patient wait time (min)') + 
    ylab('Standardized density of patients in queue') + 
    guides(colour = guide_legend(nrow = 1)) +
    theme_minimal() +
    theme(text = element_text(family = 'Centaur'),
          axis.title.x = element_text(colour = 'black'), 
          axis.title.y = element_text(colour = 'black'), 
          strip.text = element_text(colour = 'black', 
                                    face = 'bold'),
          legend.position = 'bottom',
          legend.direction = 'horizontal', 
          legend.title = element_blank())
  
  plt
  
}

# Execute to reproduce figure 2
sim_lst <- list(baseline, exclusive, double)
scenarios <- c('Baseline', 'Exclusive-use', 'Two angio INRs') 
plot_func(sim_lst, scenarios)

# Execute to reproduce the supplementary figure 
sim_lst <- list(baseline, baseline_double, baseline_triple)
scenarios <- c('Baseline', 'Doubling ECR Patients', 'Tripling ECR Patients') 
plot_func(sim_lst, scenarios)

################################################################################

#' @title Reproducing figure 3 from Huang et al., 2019
#' 
#' @description
#' `plot_func_hours` returns a plot of similar form to figure 3 from
#' the Huang et al., 2019 paper. Note: requires the use of 
#' `rel_probs`.
#' 
#' @param lst_5pm [lst] list of output from `simulat_nav` for the 5pm scenarios
#' @param lst_6pm [lst] list of output from `simulat_nav` for the 6pm scenarios
#' @param lst_7pm [lst] list of output from `simulat_nav` for the 7pm scenarios
#' 
#' @return plot of the relative wait time probabilities for extra hours

plot_func_hours <- function(lst_5pm, lst_6pm, lst_7pm, scenarios){
  
  res <- 'angio_inr' # just need the angio_inr resource for fig. 3
  n <- length(scenarios) # no. of scenarios
  plot_df <- data.frame() # empty df for storage 
  
  for (i in 1:n) { # for each scenario
    
    # we're obtaining the relative probabilities here
    
    rel_df1 <- rel_probs(lst_5pm[[i]], res)
    rel_df1 <- cbind(rel_df1, rep('5pm', nrow(rel_df1)))
    colnames(rel_df1)[ncol(rel_df1)] <- 'shift_end'
    
    rel_df2 <- rel_probs(lst_6pm[[i]], res)
    rel_df2 <- cbind(rel_df2, rep('6pm', nrow(rel_df2)))
    colnames(rel_df2)[ncol(rel_df2)] <- 'shift_end'
    
    rel_df3 <- rel_probs(lst_7pm[[i]], res)
    rel_df3 <- cbind(rel_df3, rep('7pm', nrow(rel_df3)))
    colnames(rel_df3)[ncol(rel_df3)] <- 'shift_end'
    
    df <- rbind(rel_df1, rel_df2, rel_df3)
    df <- cbind(df, rep(scenarios[i], nrow(df)))
    colnames(df)[ncol(df)] <- 'scenario'
    plot_df <- rbind(plot_df, df)
    
  }
  
  # Produce plot 
  plt <- ggplot(plot_df) + 
    geom_line(aes(x = cut_vec, y = Freq, col = shift_end), 
              linewidth = 0.75, alpha = 0.9) +
    facet_wrap(vars(factor(scenario, scenarios))) +
    xlim(0,200) + 
    coord_trans(x = 'sqrt', y = 'sqrt') + 
    xlab('Patient wait time (min)') + 
    ylab('Standardized density of patients in queue') + 
    guides(colour = guide_legend(nrow = 1)) +
    theme_minimal() +
    theme(text = element_text(family = 'Centaur'),
          strip.text = element_text(colour = 'black', 
                                    face = 'bold'),
          legend.position = 'bottom',
          legend.direction = 'horizontal', 
          legend.title = element_blank())
  
  plt
  
}

# Execute to reproduce figure 3
lst_5pm <- list(baseline, exclusive, double)
lst_6pm <- list(baseline_hr, exclusive_hr, double_hr)
lst_7pm <- list(baseline_2hr, exclusive_2hr, double_2hr)
scenarios <- c('Baseline', 'Exclusive-use', 'Two angio INRs') 
plot_func_hours(lst_5pm, lst_6pm, lst_7pm, scenarios)

################################################################################

#' @title Reproducing figure 4 from Huang et al., 2019
#' 
#' @description
#' `plot_wait` returns a plot of similar form to figure 4 from
#' the Huang et al., 2019 paper.
#' 
#' @param df_base [df] baseline scenario arrivals data frame
#' @param df_lst [lst] list of arrivals data frames for the other scenarios
#' @param scenarios [vec] vector of scenarios in the order of `df_lst`
#' 
#' @return bar chart of the mean number of days of disability-free life added
#' when compared to baseline

plot_wait <- function(df_base, df_lst, scenarios){
  
  # Average wait time for baseline 
  x0 <- mean(
    df_base$wait_time[which(df_base$category == 'ed' &
                            df_base$resource == 'angio_inr')]
    ) 
  
  # Average wait time for all other scenarios
  n <- length(df_lst)
  x <- numeric(n) # empty vector for storing
  for (i in 1:n){
    x[i] <- mean(
      df_lst[[i]]$wait_time[which(df_lst[[i]]$category == 'ed' &
                              df_lst[[i]]$resource == 'angio_inr')]
    ) 
  }
  
  # Store results in a df
  df <- as.data.frame(cbind((x0-x) * 4.2, scenarios))
  colnames(df) <- c('added_time', 'scenario')
  df$added_time <- as.numeric(df$added_time)
  df
  
  # Create plot 
  ggplot(df) +
    geom_col(aes(x = factor(scenario, scenarios),
                 y = added_time)) +
    xlab('Scenarios') +
    ylab('Mean disability-free life added (days)') + 
    theme(axis.title = element_text(size=14, face='bold'))
  
}

# Execute to reproduce figure 4
df_base <- baseline[[1]]
df_lst <- list(exclusive[[1]], double[[1]], exclusive_hr[[1]])
scenarios <- c('Exclusive-use', 'Two angio INRs', 'Exclusive-use and +1hr work')
plot_wait(df_base, df_lst, scenarios)

################################################################################

#' @title Reproducing figure 5 from Huang et al., 2019
#' 
#' @description
#' `plot_util` returns a plot of similar form to figure 5 from
#' the Huang et al., 2019 paper.
#' 
#' @param df_lst [lst] list of resources data frames 
#' @param scenarios [vec] vector of scenarios in the order of `df_lst`
#' @param res [str] resource to isolate
#' 
#' @return bar chart of the utilisation percentages for each scenario

plot_util <- function(df_lst, scenarios, res){
  
  n <- length(df_lst) # no. of scenarios
  df <- data.frame() # empty data frame for storing
  
  for (i in 1:n) { # for each scenario
    
    # Extract only the required resource  
    df_lst[[i]] <- df_lst[[i]][
      which(df_lst[[i]]$resource == res),]
    
    # Code modified from simmer source code to obtain utilisation values 
    df_lst[[i]] <- df_lst[[i]] %>%
      group_by(.data$resource, .data$replication) %>%
      mutate(dt = lead(.data$time) - .data$time) %>%
      mutate(capacity = ifelse(.data$capacity < .data$server, 
                               .data$server, .data$capacity)) %>%
      mutate(dt = ifelse(.data$capacity > 0, .data$dt, 0)) %>%
      mutate(in_use = .data$dt * .data$server / .data$capacity) %>%
      summarise(utilization = sum(.data$in_use, na.rm = TRUE) / 
                  sum(.data$dt, na.rm=TRUE)) %>%
      summarise(Q25 = stats::quantile(.data$utilization, .25),
                Q50 = stats::quantile(.data$utilization, .5),
                Q75 = stats::quantile(.data$utilization, .75))
    df <- rbind(df, c(df_lst[[i]]$Q25,
                      df_lst[[i]]$Q50,
                      df_lst[[i]]$Q75, 
                      scenarios[i])
                )
  }
  
  # Create plot
  colnames(df) <- c('Q25', 'Q50', 'Q75', 'scenario')
  df[1:3] <- sapply(df[,1:3], as.numeric)
  q50_vals <- paste(round(100*df$Q50), "%", sep='')
  ggplot(df) +
    aes(x = scenario, y = Q50, ymin = Q25, ymax = Q75) +
    geom_col() +
    geom_errorbar(width = .25, color = "black") +
    geom_text(aes(label = q50_vals), vjust = -1) +
    scale_y_continuous(labels = scales::percent, 
                       limits = c(0, 0.4), 
                       breaks = seq(0, 2, .2)) +
    xlab('Scenario') +
    ylab('Utilization') + 
    theme(axis.title = element_text(size=14, face='bold'))
  
}

# Execute to reproduce figure 5
df_lst <- list(baseline[[2]], exclusive[[2]], double[[2]])
scenarios <- c('Baseline', 'Exclusive-use', 'Two angio INRs')
res <- 'angio_inr'
plot_util(df_lst, scenarios, res)

################################################################################