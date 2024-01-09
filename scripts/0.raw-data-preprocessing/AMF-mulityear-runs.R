# Read in the half hourly Ameriflux data at the UMBS site, convert the variables 
# to the appropriate units and naming patterns for an SEM run. The meterolgical 
# input returned here will be for a multiyear run with different data for each 
# year (this is different than the idealized runs where the same weather from a 
# single year is held constant over the course of the run). 

# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(here)
library(lubridate)

# The directory to write the output file to. 
WRITE_TO <- here("data", "met-input")


# Set up option to write some quality check figures out 
MAKE_FIGS <- TRUE
FIGS_DIR <- file.path(WRITE_TO, "FIGS-AMF_UMBS_2007-2021")
dir.create(FIGS_DIR)
theme_set(theme_bw())

# 1. Format --------------------------------------------------------------------
# Read in the data 
AMF_file <- here("data", "raw-data", "AMF_US-UMd_FLUXNET_SUBSET_HH_2007-2021_3-5.csv")

# Select the variables and format into a data frame. 
read.csv(AMF_file) %>%  
  select(time = TIMESTAMP_START, 
         temp = TA_F, 
         precip = P_F, 
         VPD = VPD_F, 
         PAR = PPFD_IN) %>% 
  mutate(VPD = VPD/10) %>% # the VPD had to be converted from hPa to kPa
  mutate(PAR = ifelse(PAR < 0, 0, PAR)) ->  # the no data PAR code was -9999 replaces with 0 
  formatted_data 


# It looks like there might have been some issues with the PAR sensors, there are 
# some stretches - days to months where PAR is 0 the entire time, which is simply 
# not true. What we are going to want to do here is over write the days where is 
# 0 all the time. 

# Subset the PAR data so that it has only information about midday (12:00)
# this is when we would expect the daily par to be at its max. 
formatted_data %>%  
  select(time, PAR) %>% 
  mutate(hour = hour(ymd_hm(time))) %>% 
  filter(hour == 12) %>% 
  filter(PAR <= 1e-3) %>% 
  mutate(year_mon_day = date(ymd_hm(time))) %>% 
  pull(year_mon_day) -> 
  PAR_dates_to_replace

# Quick take a look at how many days are going to have to be replaced... 
# it looks like only about 11% of the days are missing the PAR values
length(PAR_dates_to_replace) / length(unique(date(ymd_hm(formatted_data$time))))

formatted_data %>%  
  mutate(year_mon_day = date(ymd_hm(time))) %>% 
  mutate(good = if_else(!year_mon_day %in% PAR_dates_to_replace, 1, 0)) %>% 
  ggplot(aes(year_mon_day, good, color = good)) + 
  geom_point() + 
  labs(title = "Distirbtuion of data to replace", x = "Day", y = "")
  
# Subset the PAR data into the "good data" i.e. there is PAR values over the 
# course of the entire day. 
formatted_data %>%  
  select(time, PAR) %>% 
  mutate(year_mon_day = date(ymd_hm(time))) %>% 
  filter(!year_mon_day %in% PAR_dates_to_replace) %>% 
  select(-year_mon_day) %>% 
  mutate(mon_day_time = substr(time, 5, 12)) %>% 
  group_by(mon_day_time) %>% 
  summarise(new_PAR = median(PAR)) -> 
  new_par_df

# Saving the original data so that ight want to compare the new values being filled in with this plot.... to 
# show just how different some of the values that are being overwritten are 
formatted_data %>%  
  mutate(year_mon_day = date(ymd_hm(time))) %>% 
  filter(year_mon_day %in% PAR_dates_to_replace) %>% 
  mutate(time = ymd_hm(time)) -> 
  og_data 

# All of the other variables appear to be correct and should be left alone. 
formatted_data %>%  
  mutate(year_mon_day = date(ymd_hm(time))) %>% 
  filter(year_mon_day %in% PAR_dates_to_replace)  %>% 
  select(-year_mon_day) -> 
  PAR_data_needed

PAR_data_needed %>% 
  mutate(mon_day_time = substr(time, 5, 12)) %>% 
  left_join(new_par_df, by = "mon_day_time") %>% 
  select(-mon_day_time) -> 
  new_n_old_PAR

# Quick comparison plots for the original/old PAR data and the 
# new PAR values. 
ggplot(new_n_old_PAR) + 
  geom_point(aes(PAR, new_PAR)) + 
  geom_abline(slope = 1, intercept = 0) +
  labs(x = "old PAR values", y = "New PAR values")

new_n_old_PAR  %>% 
  mutate(time = ymd_hm(time)) %>%
  ggplot() + 
  geom_point(aes(time, PAR, color = "old PAR")) + 
  geom_point(aes(time, new_PAR, color = "new PAR"), alpha = 0.5)

new_n_old_PAR %>%  
  select(-PAR) %>% 
  select(time, temp, precip, VPD, PAR = new_PAR) -> 
  data_to_add

# Subset the original formatted data so that it only includes the values 
# for the days we are not having to replace the PAR. 
formatted_data %>%  
  mutate(year_mon_day = date(ymd_hm(time))) %>% 
  filter(!year_mon_day %in% PAR_dates_to_replace) %>% 
  select(-year_mon_day) -> 
  data_to_keep 

# Combine the met data frames (days we are not having to replace the PAR and the 
# data frame with the PAR data filled in) into a single data frame. 
data_to_keep %>% 
  rbind(data_to_add) %>%  
  arrange(time) -> 
  formatted_data

# 2. Quality Checks --------------------------------------------------------------------
# Take a look at the average temperature... 
# Based on Gough et al., 2013 we would expect something around 5C 
mean(formatted_data$temp)


# Check to make sure that there are no repeat times.. .
stopifnot(length(unique(formatted_data$time)) == nrow(formatted_data))

# Check to see if the length is what we would expect, it is important to note that 
# there are 4 leap years that would occur in this time frame.  
expected <- 15 * 365 * 24 * 2
days_diff_in_time <- (nrow(formatted_data) - expected) / 48
expected_leap_years <- 4
stopifnot(diff_in_time == expected_leap_years)


if(MAKE_FIGS){
  # Format the data for plotting
  formatted_data %>% 
    mutate(time = ymd_hm(time)) %>%  
    mutate(yr = year(time), 
           mon = month(time)) -> 
    to_plot
  
  # Plot each of the years/months of half hourly meterological data
  split(to_plot, interaction(to_plot$yr, to_plot$mon)) %>% 
    lapply(function(d){
      
      yr <- unique(d$yr)
      mon <- unique(d$mon)
      
      title <- paste(yr, month.name[mon], sep = " ")
      
      d %>% 
        tidyr::pivot_longer(cols = c(temp, precip, VPD, PAR),
                            names_to = "variable", values_to = "value") %>% 
        ggplot(aes(time, value)) + 
        geom_line() + 
        facet_wrap("variable", scales = "free") + 
        theme_bw() + 
        labs(y = NULL, x = NULL, title = title) -> 
        fig 
      
      ggsave(plot = fig, filename = file.path(FIGS_DIR, paste0(title, ".png")))

    })
}

# 3. Save------------------ ----------------------------------------------------------------------
write.csv(formatted_data, file = file.path(WRITE_TO, "AMF_UMBS_2007-2021.csv"), row.names = FALSE)







