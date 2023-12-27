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
MAKE_FIGS <- FALSE
FIGS_DIR <- file.path(WRITE_TO, "FIGS-AMF_UMBS_2007-2021")
dir.create(FIGS_DIR)

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


# 3. Additional Formatting ----------------------------------------------------------------------

# 3. Save------------------ ----------------------------------------------------------------------
write.csv(formatted_data, file = file.path(WRITE_TO, "AMF_UMBS_2007-2021.csv"), row.names = FALSE)







