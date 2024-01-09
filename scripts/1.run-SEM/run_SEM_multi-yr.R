
# 0. Set Up --------------------------------------------------------------------
# Install the correct version of the SEM package, if the SEM on your 
# local machine is up to date then you should be able to run 
# otherwise install the dev branch from github 
# remotes::install_github("kdorheim/SEM@dev")
# library(SEM)
devtools::load_all("/Users/dorh012/Documents/2023/FoRTEII/SEM")

# The packages that are used in this script... 
library(dplyr)
library(ggplot2)
library(here)
library(lubridate)


# 1. Run SEM -------------------------------------------------------------------
# Load the UMBS meteorological inputs 
met_inputs <- read_SEM_met(here("data", "met-input", "AMF_UMBS_2007-2021.csv")) %>%  
  # For right now while we are trying to get everything set up and running 
  # lets limit the run to the first five years. 
 # dplyr::filter(year(ymd_hm(time)) <= 2010) %>% 
  mutate(PAR = if_else(PAR < 0.1, 1e-20, PAR))
  
# Read in the UMBS parameters and initial pool values 
# some of these values might need to be changed
params_df <- read.csv(here("data", "UMBS-params.csv"))
pools_df <- read.csv(here("data", "UMBS-pools.csv"))

MD <- as.data.frame(SEM::params_df)
params_df$value - unlist(MD$value)

params_df <- SEM::params_df
pools_df <- SEM::pools


params_df
pests <- c("phloem" = 0, "xylem" = 0, "leaf" = 0, "root" = 0, "stem" = 0)
pest.time <- NULL

# Running it until the point it will fail, right beforehand 
index <- which(met_inputs$time == 201001150000)

run_SEM(pest = pests, 
        pest.time = pest.time,
        inputs = met_inputs, 
        X = pools, 
        param_df = params_df, 
        DBH = 8, 
        quiet = FALSE) %>% 
  mutate(time = ymd_hm(time)) -> 
  out

out %>% 
  ggplot() + 
  geom_line(aes(time, value)) + 
  facet_wrap("variable", scales = "free")



# okay so there is something funky going on here of course, it looks like at least 
# one of the paramters or a combo of parameters is leading DEATH... so what do we need 
# to do?? is probably take a look parameter by paramter it figure out which one is 
# going to cause of failure. 
