source("R/functions.R")

# NINDSS linelist 
# read in the full linelist
linelist_full <- load_linelist(use_vic = FALSE,
                               use_nsw = TRUE,
                               date = as_date("2023-02-16"))

#read in the last 6 months only updates
linelist <- load_linelist(use_vic = FALSE, use_nsw = FALSE) 
#cutoff date

#NNDSS has somehow been giving us incomplete days worth of cases at the start of the weekly (six month updates) linelists
#calculate number of cases on each day in both linelists and find the oldest day where both linelists match to use as the cutoff date
#because of NSW being loaded from different data there may not be many matching days. If this is the case, exclude NSW from the summaries
min_ll_date <- linelist %>% pull(date_confirmation) %>% min()

ll_full_summary <- linelist_full %>% filter(linelist_full$date_confirmation >= min_ll_date) %>% count(date_confirmation)
ll_summary <- linelist %>% count(date_confirmation)

ll_matches <- semi_join(ll_summary, ll_full_summary)
cutoff_date <- min(ll_matches$date_confirmation)

#filter six month update by cutoff date
linelist <- linelist %>% filter(linelist$date_confirmation >= cutoff_date)

#join updated linelist with older one
linelist <- linelist %>% 
  filter(state != "NSW") %>% 
  bind_rows(linelist_full %>% 
              filter(date_confirmation < cutoff_date | state == "NSW"))

# remove dubious SA onset dates that are dated to same as confirmation dates
linelist$date_onset[(linelist$state == "SA" & linelist$date_onset >= as_date("2022-02-27"))] <- NA

#cut off impossible early RATs that could cause date misalignment
linelist <- linelist %>% 
  filter(
    !(test_type == "RAT" & 
        date_confirmation < as_date("2021-11-01"))
  )
#check min & max dates
min_date <- min(linelist$date_confirmation)
min_date
(max_date <- max(linelist$date_confirmation))

#remove dubious confirmation dates (shouldn't be any here)
linelist <- linelist %>% filter(date_confirmation >= "2020-01-23")

#fix future dates problem
linelist <- linelist %>% filter(date_confirmation <= max(date_linelist))

#visual check
plot_linelist_by_confirmation_date(linelist = linelist, date_cutoff = cutoff_date - months(1))

# load in Victorian summary data
summary_data <- get_summary_data(states = "VIC")

#visually check for issues
summary_data %>% filter(date>=(max(summary_data$date)-days(30))) %>% 
  ggplot(aes(x = date, y = cases, fill = test_type)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~state,scales = "free")

#replace linelist bit with summary data
linelist <- replace_linelist_bits_with_summary(linelist,
                                               summary_data,
                                               states_select = c("VIC"),
                                               start = as_date("2022-01-06"),
                                               end = NULL)

#save linelist
saveRDS(
  linelist,
  sprintf(
    "outputs/linelist_%s.RDS",
    linelist$date_linelist[1] %>%
      format.Date(format = "%Y%m%d")
  )
)
