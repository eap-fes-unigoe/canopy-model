# setup input and fluxes for 2016 (because in 2018 lw is NA)
library(tidyverse)
library(progress)
dt <- 3600
source("setup_parameters.R")
source("setup_sitedata.R")

source("fun_calc_radiative_transfer.R")

state <- tibble(t_leaf = input$tair, t_soil = fluxes$tsoil)

# Utility funcs

#' Full radiative transfer model with potentially new paramameters that overwrite the defaults
rad_transf_new_p <- function (new_p=list()){
  pars <- merge_lists(pars, new_p)
  rad_transf(pars = pars)
}

#' Full radiative transfer model

d_input <- input
d_state <- state
d_pars <-  pars
d_dt <- dt
rad_transf <- function(input = d_input, state = d_state, pars = d_pars, dt = d_dt){
  out <- data.frame()
  pb <- setup_pb()
  for (i in seq_along(input$datetime)){
    rad<- fun_calc_radiative_transfer(input[i,], state[i,], pars, dt)
    out[i, names(rad)] <- rad
    pb$tick()
  }
  return(out)
}

setup_pb <- function(){
  progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed: :elapsedfull | ETA: :eta]",
                       total = length(input$datetime),
                       clear = FALSE)
}

#' merges two list, in case of conflicts uses the second list value
merge_lists <- function (list1, list2){
  l <- list1
  for (name in names(list2)){
    l[name] <- list2[name]
  }
  return(l)
}

#' transpose an aggreagated sensisivity dataframe
invert_sens <- function(df){
    vars <- df$var
    df <- select(df, -var)
    pars <- names(df)
    df <- as_tibble(t(df))
    df <- cbind(pars, df)
    names(df) <- c("var", vars)
    return(df)
}

filter_sens <- function(sens_model, vars = NULL, pars = NULL){
  sens_model <- select(sens_model, -x)
  if (!is.null(vars)) {sens_model <- filter(sens_model, var %in% vars)}
  if (!is.null(pars)) {sens_model <- select(sens_model, all_of(c("var", pars)))}
  return(sens_model)
}

detailed_sens <- function(sens_model, func){
  sens_model %>%
        group_by(var) %>%
        summarize_all(func) %>%
        invert_sens
}