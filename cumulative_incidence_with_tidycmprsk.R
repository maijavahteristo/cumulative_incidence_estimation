#loading the needed libraries
library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggsurvfit)
library(tidycmprsk)
library(lubridate)

#loading the data
data <- fread("data.csv")

#preparing the follow-up time
data[,overall_end := as.Date("2021-12-31")] #the last day of the follow-up 

#in the data table dg_date_first is the date of the first HSIL diagnosis of an individual
#date_of_death is the date of individuals possible death
#end_of_scr is the date of exit from screening
#in the next row the first of these dates is set as the end of the follow-up date
data[,cens_pvm := pmin(overall_end, dg_date_first, date_of_death, end_of_scr, na.rm = T)]

#in the next three rows the status at the end_of_follow-up is set up for each individual 
#if the date 
data[cens_pvm == overall_end | cens_pvm == end_of_scr, status_cens := 0]
#if the censoring date is the date of the HSIL diagnosis, censoring status is 1, which is the event of interest
data[!is.na(HSIL_dg) & dg_date_first == cens_pvm, status_cens := 1]
#if the censoring date is the date of death, censoring status is 2, since death is considered a competing event in this analysis
data[cens_pvm == date_of_death, status_cens := 2]

#calculating the follow-up time in years 
data[, follow_up_time :=  time_length(difftime(as.Date(cens_pvm), as.Date(date_of_entry_test)), "years")]


#performing the cumulative incidence calculation with the tidycmprsk package
cumulative_incidence_fig <- tidycmprsk::cuminc(Surv(follow_up_time, as.factor(status_cens)) ~ genotype, data) %>%
  ggcuminc() + #plotting the cumulative incidences 
  facet_wrap(~strata, nrow = 4, axes = "all", axis.labels = "all") +
  xlab("Time (years)") +
  add_confidence_interval() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "CIN2+ cumulative incidence by HPV genotype") +
  scale_color_lancet() +
  scale_fill_lancet() +
  coord_cartesian(xlim = c(0,18)) + 
  theme(legend.position="none") 

#saving the figure to the working directory
ggsave("cumulative_incidence_by_genotype.jpg", dpi = 700, width = 8, height = 6)

