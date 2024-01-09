
# 0. Set Up --------------------------------------------------------------------
# Install the correct version of the SEM package, if the SEM on your 
# local machine is up to date then you should be able to run 
#library(SEM) 
# otherwise install the dev branch from github 
# remotes::install_github("kdorheim/SEM@dev")
devtools::load_all("/Users/dorh012/Documents/2023/FoRTEII/SEM")

library(dplyr)
library(ggplot2)
library(here)
library(lubridate)


# 1. Run SEM -------------------------------------------------------------------
# Load the UMBS meteorological inputs 
met_inputs <- read_SEM_met(here("data", "met-input", "AMF_UMBS_2007-2021.csv"))


# Read in the UMBS parameters and inital pool values 
# some of these values might need to be changed
params_df <- read.csv(here("data", "UMBS-params.csv"))
#params_df <- SEM::params_df # not using the UMBS paramters to see if this is the problem or not 
#pools_df <- read.csv(here("data", "UMBS-pools.csv"))
#pools <- pools_df$value
#names(pools) <- pools_df$pool
pools <- SEM::pools

pests <- c("phloem" = 0, "xylem" = 0, "leaf" = 0, "root" = 0, "stem" = 0)
pest.time <- NULL

run_SEM(pest = pests, 
        pest.time = pest.time,
        inputs = met_inputs, 
        X = pools, 
        param_df = params_df, 
        DBH = 8, quiet = FALSE) %>% 
  mutate(time = ymd_hm(time)) -> 
  out

write.csv(out, file = "with_UMBS_params.csv", row.names = FALSE)
out %>% 
  filter(variable == "GPP") %>% 
  ggplot() + 
  geom_line(aes(time, value))
