source("R/functions.R")

#parse all contact rate data, automatically remove duplicates, and visually
# check for duplicate ages [~60s]


parse_all_doh_surveys() %>%
  filter(wave > (max(wave) - 4)) %>%
  plot_age_duplication()

ggsave(
  "outputs/figures/age_deduplication_check.png",
  width = 9,
  height = 10)

# will light up cells with any overly large counts. If this happens check raw data