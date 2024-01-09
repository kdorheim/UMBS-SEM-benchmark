# Objective: get Lisa up and running with SEM as we are still trying to sort out 
# why the UMBS parmaterized SEM fails early 

# 0. Set Up --------------------------------------------------------------------
# Install the correct version of the SEM package, using the remotes_install 
# call will make sure that you are working with my most recent pushes to SEM. 
remotes::install_github("kdorheim/SEM@dev")
library(SEM)


# Additional packages used in this script. 
library(dplyr)
library(ggplot2)
library(here)
library(lubridate)


# 1. Basic SEM Run -------------------------------------------------------------------
# Load the UMBS meteorological inputs 
met_inputs <- read_SEM_met(here("data", "met-input", "AMF_UMBS_2007-2021.csv")) %>%  
  # For right now while we are trying to get everything set up and running 
  # lets limit the run to the first few years. The filter call is limiting 
  # the met data to obs from certain years. 
  dplyr::filter(year(ymd_hm(time)) <= 2010) %>% 
  # It looks like the ameriflux par sensors are pretty sensitive, they are recording
  # PAR in the middle of the night (leads to some midnight GPP) which does not 
  # seem right. For PAR values less than 0.1 write them as 0. 
  # TODO is there a better way to handel this?
  mutate(PAR = if_else(PAR < 0.1, 1e-20, PAR))

# Read in the UMBS parameters and initial pool values 
# some of these values might need to be changed
# # Comment these out to make sure you are using the default params from the 
# # D&M publication. 
# params_df <- read.csv(here("data", "UMBS-params.csv"))
# pools_df <- read.csv(here("data", "UMBS-pools.csv"))

# Load the default SEM parameters that were used in the M&D publication, to use 
# the UMBS settings comment these lines out and un-comment the lines 35 & 36. 
params_df <- SEM::params_df
pools_df <- SEM::pools


# Set up a non disturbance run 
pests <- c("phloem" = 0, "xylem" = 0, "leaf" = 0, "root" = 0, "stem" = 0)
pest.time <- NULL

# Run the model! 
run_SEM(pest = pests, 
        pest.time = pest.time,
        inputs = met_inputs, 
        X = pools, 
        param_df = params_df, 
        DBH = 8, 
        quiet = FALSE) %>% 
  # I personally like formatting the time as year month date hour minute so 
  # that it makes it easy to filter and plot. 
  mutate(time = ymd_hm(time)) -> 
  out

# Take a quick look at the output! 
out %>% 
  ggplot() + 
  geom_line(aes(time, value)) + 
  facet_wrap("variable", scales = "free")


# 2. With a disturbances -------------------------------------------------------
# Set up a non disturbance run 
pests_1 <- c("phloem" = 0.95, "xylem" = 0, "leaf" = 0, "root" = 0, "stem" = 0)
pest.time_1 <- 200807120000 # In the second year of the run only during 1 time step! 

# Run the model with the short disturbance.  
run_SEM(pest = pests_1, 
        pest.time = pest.time_1,
        inputs = met_inputs, 
        X = pools, 
        param_df = params_df, 
        DBH = 8, 
        quiet = FALSE) %>% 
  mutate(time = ymd_hm(time)) -> 
  out_dist1

out$run <- "control"
out_dist1$run <- "30 min phloem"

to_plot <- rbind(out, out_dist1)

# Quickly compare the results from the control and disturbance runs, for the 30 
# min disturbance only see the difference between the runs very briefly 
to_plot %>% 
  filter(year(time) == 2008 & month(time) == 07 & day(time) == 12) %>% 
  ggplot() + 
  geom_line(aes(time, value, color = run)) + 
  facet_wrap("variable", scales = "free")

# Now apply the disturbance for the entire day 
met_inputs %>% 
  filter(grepl(pattern = "20080712", x = time)) %>% 
  pull(time) -> 
  pest.time_2

# Run the model with the short disturbance.  
run_SEM(pest = pests_1, 
        pest.time = pest.time_2,
        inputs = met_inputs, 
        X = pools, 
        param_df = params_df, 
        DBH = 8, 
        quiet = FALSE) %>% 
  mutate(time = ymd_hm(time)) -> 
  out_dist2

out_dist2$run <- "full day phloem"

to_plot <- rbind(out, out_dist1, out_dist2)

# Quickly compare the results from the control and disturbance runs, looking at the 
# disturbance day. 
to_plot %>% 
  filter(year(time) == 2008 & month(time) == 07 & day(time) == 12) %>% 
  ggplot() + 
  geom_line(aes(time, value, color = run)) + 
  facet_wrap("variable", scales = "free")

to_plot %>% 
  filter(year(time) == 2008 & month(time) == 07) %>% 
  ggplot() + 
  geom_line(aes(time, value, color = run)) + 
  facet_wrap("variable", scales = "free")


# 3. Global Sensitivity Run -------------------------------------------------------
# Define the set up fo a no pest run, right now this example only considered 
# two parameters, no pests (control run), and uses the default pools/params. 
# Also only runs 10 times, but depending on how many years of met data is 
# being fed in can take longer.
pests <- c("phloem" = 0, "xylem" = 0, "leaf" = 0, "root" = 0, "stem" = 0)
pest_time <- NULL

# I recommend using a shorter time series cause it will take a while 
met_inputs <- filter(met_inputs, year(ymd_hm(time)) == 2007)

# Set up the parameters to vary 
pars <- c("Vcmax" =  18, "Jmax" = 30.06)

# The ranges to explore per parameter. 
prange <- data.frame(min = pars - pars * 0.15,
                     max = pars + pars * 0.15)

global_out <- SEM_sensrange(pars = pars,
                            parRange = prange, 
                            param_df = params_df, 
                            inputs = met_inputs, 
                            X = pools, 
                            DBH = 10,
                            pest = pests,
                            pest.time = NULL,
                            n = 5) # n is the number of SEM runs to limit run time set this value to 10, increasing n will increase the run time.


# Use the format_sensout function to transform the object returned by the 
# SEM_sensrange function into 
global_out_to_plot <- format_sensout(global_out) %>% 
  mutate(time = ymd_hm(time))

# Select a few variables to look at
vars_to_plot <- c("GPP", "Rleaf", "Bleaf", "Rh")

global_out_to_plot %>% 
  filter(variable %in% vars_to_plot) %>% 
  # Select only a handful of dates to look at.  
  filter(date(time) %in% c("2007-03-21", "2007-03-22", "2007-03-23")) %>% 
  ggplot() + 
  geom_line(aes(time, Mean)) +
  geom_ribbon(aes(time, ymin = Min, ymax = Max), alpha = 0.5) + 
  facet_wrap("variable", scales = "free") + 
  labs(x = NULL, y = NULL)


# 4. Local Sensitivity -------------------------------------------------------

local_out <- SEM_sensfunc(pars = pars,
                          param_df = params_df, 
                          inputs = met_inputs, 
                          X = pools, 
                          DBH = 10) 

summary(local_out)
pairs(local_out)

# Format the results for easy plotting. 
local_out %>% 
  format_sensout() %>% 
  mutate(time = ymd_hm(time)) -> 
  local_out_plotting


# Select a few variables to look at
vars_to_plot <- c("GPP", "Rleaf", "Bleaf", "Rh")

local_out_plotting %>% 
  filter(variable %in% vars_to_plot) %>% 
  # Select only a handful of dates to visualize otherwise it gets to be a lot. 
  ggplot() + 
  geom_line(aes(time, value, color = parameter)) +
  facet_wrap("variable", scales = "free") + 
  labs(x = NULL, y = NULL)
