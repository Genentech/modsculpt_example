
library(dplyr)
library(lubridate)

# . Preparation -----------------------
if (!dir.exists(here::here("data"))) dir.create(here::here("data"))
if (!dir.exists(here::here("data", "raw"))) dir.create(here::here("data", "raw"))
if (!dir.exists(here::here("data", "processed"))) dir.create(here::here("data", "processed"))


# . Compas (Politico) -----------------


# .. Download data --------------------

download.file(
  "https://raw.githubusercontent.com/propublica/compas-analysis/master/compas-scores-two-years.csv",
  here::here("data", "raw", "compas-scores-two-years.csv")
)


# .. Preprocessing --------------------
# according to: https://github.com/corels/corels/blob/3a57a7e7537f9416e5e05c0e0ae9a1202b3ae95c/processing/compas.py#L11

df <- read.csv(here::here("data", "raw", "compas-scores-two-years.csv")) %>%
  mutate(
    new_age = floor(time_length(as_date(compas_screening_date) - as_date(dob), unit="years")) + 1,
    juvenile_crimes = juv_fel_count + juv_misd_count + juv_other_count,
    current_charge_degree = ifelse(c_charge_degree == "M", "Misdemeanor", "Felony")
  ) %>% 
  select(
    sex, age = new_age, 
    juvenile_felonies = juv_fel_count, juvenile_misdemeanors = juv_misd_count, 
    juvenile_crimes, priors = priors_count, current_charge_degree,
    recidivate_within_two_years = two_year_recid
  )


# .. Holdout split --------------------

set.seed(5246)
idx <- sample(nrow(df), size = 721)
write.csv(df[idx,], file = here::here("data", "processed", "compas_holdout.csv"), row.names = FALSE)
write.csv(df[-idx,], file = here::here("data", "processed", "compas_train.csv"), row.names = FALSE)



# . Bike sharing ----------------------

# .. Download data --------------------
download.file(
  "https://archive.ics.uci.edu/static/public/275/bike+sharing+dataset.zip",
  here::here("data", "raw", "bike+sharing+dataset.zip")
)

# unzip

unzip(here::here("data", "raw", "bike+sharing+dataset.zip"),
      exdir = here::here("data", "raw", "bike_sharing_dataset"))


# .. Preprocessing --------------------

bikes <- read.csv(here::here("data", "raw", "bike_sharing_dataset", "day.csv"))


# .. Holdout split --------------------
set.seed(548975)
sp_bikes <- rsample::initial_split(bikes, prop = 2/3)

write.csv(rsample::training(sp_bikes), file = here::here("data", "processed", "bike_train.csv"), row.names = FALSE)
write.csv(rsample::testing(sp_bikes), file = here::here("data", "processed", "bike_holdout.csv"), row.names = FALSE)




