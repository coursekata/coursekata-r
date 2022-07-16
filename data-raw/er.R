library(dplyr)
library(lubridate)
library(readr)
library(readxl)
library(rlang)
library(stringr)
library(tidyr)
library(usethis)

# Citation:
# Kline JA, Fisher MA, Pettit KL, Linville CT, Beck AM (2019) Controlled
# clinical trial of canine therapy versus usual care to reduce patient anxiety
# in the emergency department. PLoS ONE 14(1): e0209232.
# https://doi.org/10.1371/journal.pone.0209232

# Data site:
# https://doi.org/10.5061/dryad.9pv5625

# Data source: https://datadryad.org/stash/downloads/file_stream/38877
if (!file.exists("data-raw/canine_data.xls")) {
  download.file(
    "https://datadryad.org/stash/downloads/file_stream/38877",
    "data-raw/canine_data.xls"
  )
}

raw <- readxl::read_excel(
  "data-raw/canine_data.xls",
  skip = 1,
  col_names = c(
    "id", "event_name", "age", "gender", "race", "race_other", "veteran", "disabled",
    paste0("mh_", c(
      "none", "asthma", "copd", "smoker", "cad", "diabetes", "hypertension",
      "stroke", "chronic_kidney", "hyperlipidemia", "hiv", "other"
    )),
    paste0("ph_", c(
      "adhd", "anxiety", "bipolar", "borderline", "depression",
      "schizophrenia", "ptsd", "none", "other"
    )),
    "time_of_consent", "provider_male", "provider",
    "temperature", "heart_rate", "resp_rate", "sp_o2", "bp_syst", "bp_diast", "med_given",
    "timepoint", "t1_occurred", "t2_occurred", "condition",
    "time_dog_entered", "dog_name", "provider_reevaluate", "time_dog_exited",
    "time_surveyed", "pain", "depression", "anxiety", "anxiety_baseline_delta",
    "time_provider_surveyed", paste0("provider_", c("pain", "depression", "anxiety")),
    "touched_dog", "talked_to_handler", "handler_interaction_rating"
  ),
  col_types = c(
    rep("text", 2), "numeric", rep("text", 5),
    rep("text", 11), "skip", "text", # skip "other" checkbox, but keep next other text
    rep("text", 7), "skip", rep("text", 2), # skip "other" checkbox, but keep next other text
    "skip", # skip complete? is true for all
    "date", "numeric", "text", rep("numeric", 6),
    "text", rep("skip", 28), # skip med administration details
    rep("text", 4), "date", rep("text", 2), "date",
    "date", rep("numeric", 3), "numeric",
    "date", rep("numeric", 3),
    rep("text", 2), "numeric", "skip"
  ),
  na = c("", "-", "nd", "unk", "Unk", "Unknown")
)

cleaned <- raw %>%
  set_names(str_to_lower((names(.)))) %>%
  separate(id, c("id", "withdrew"), fill = "right") %>%
  mutate(
    withdrew = !is.na(withdrew),
    provider_male = !is.na(provider_male),
    # format the time point labels similarly
    timepoint = ifelse(str_starts(timepoint, "Baseline"), "T0", timepoint),
    # one of the time points is mislabeled
    timepoint = ifelse(id == 55 & event_name == "T1 (Arm 2: No Dog)", "T1", timepoint),
    # convert check boxes to logical
    across(
      c(starts_with("mh_"), starts_with("ph_"), -mh_other, -ph_other),
      ~ .x == "Checked"
    ),
    # convert yes/no to logical
    across(
      c(
        veteran, disabled, t1_occurred, t2_occurred, provider_reevaluate,
        touched_dog, talked_to_handler, med_given
      ),
      ~ .x == "Yes"
    )
  )


# Survey data at baseline ---------------------------------------------------------------------

er_full_survey <- cleaned %>%
  filter(timepoint == "T0") %>%
  select(id:med_given, -event_name)

write_csv(er_full_survey, "data-raw/er_full_survey.csv")
# use_data(er_full_survey, overwrite = TRUE, compress = "xz")


# Trial information across time points --------------------------------------------------------

er_full_trials <- cleaned %>%
  select(id, timepoint:handler_interaction_rating)

write_csv(er_full_trials, "data-raw/er_full_trials.csv")
# use_data(er_full_trials, overwrite = TRUE, compress = "xz")


# Wide data, by participant -------------------------------------------------------------------

# the times are all reported on different lines and dates are not given, so the values are confusing
# to make this easier to understand, just convert everything to how many minutes between each event

mins_between <- function(t1, t2) {
  interval(t1, t2) %>%
    as.duration() %>%
    as.numeric("minutes")
}

patient_times <- er_full_trials %>%
  select(id, timepoint, time_surveyed) %>%
  pivot_wider(
    names_from = timepoint,
    values_from = starts_with("time"),
    names_glue = "{timepoint}_{.value}"
  )

dog_times <-
  full_join(
    er_full_trials %>% select(id, time_dog_entered) %>% na.omit(),
    er_full_trials %>% select(id, time_dog_exited) %>% na.omit(),
    by = "id"
  )

durations <-
  full_join(patient_times, dog_times, by = "id") %>%
  transmute(
    id = id,
    mins_T0_T1 = mins_between(T0_time_surveyed, T1_time_surveyed),
    mins_T0_dog = mins_between(T0_time_surveyed, time_dog_entered),
    mins_dog_present = mins_between(time_dog_entered, time_dog_exited),
    mins_dog_T1 = mins_between(time_dog_exited, T1_time_surveyed),
    mins_T1_T2 = mins_between(T1_time_surveyed, T2_time_surveyed)
  )

# these are only reported at T1, so they don't pivot
handler_ratings <- er_full_trials %>%
  filter(timepoint == "T1") %>%
  select(id, touched_dog, talked_to_handler, handler_interaction_rating)

conditions <- er_full_trials %>%
  filter(timepoint == "T0") %>%
  select(id, condition, dog_name)

er_full <- er_full_trials %>%
  select(id, timepoint, pain, depression, anxiety, starts_with("provider_")) %>%
  left_join(durations, by = "id") %>%
  left_join(handler_ratings, by = "id") %>%
  pivot_wider(
    names_from = timepoint,
    values_from = c(pain, depression, anxiety, starts_with("provider")),
    names_glue = "{timepoint}_{.value}"
  ) %>%
  left_join(
    er_full_survey %>% select(-time_of_consent),
    by = "id"
  ) %>%
  left_join(conditions, by = "id")

write_csv(er_full, "data-raw/er.csv")
# use_data(er_full, overwrite = TRUE, compress = "xz")


# Wide data,  abbreviated ---------------------------------------------------------------------

er <- er_full %>%
  mutate(
    condition = ifelse(condition == "Dog", "Dog", "Control"),
    base_total = T0_pain + T0_depression + T0_anxiety,
    later_total = T1_pain + T1_depression + T1_anxiety,
    last_total = T2_pain + T2_depression + T2_anxiety,
    change_pain = T1_pain - T0_pain,
    change_depression = T1_depression - T0_depression,
    change_anxiety = T1_anxiety - T0_anxiety,
    change_total = change_pain + change_depression + change_anxiety
  ) %>%
  select(
    id, condition, age, gender, race, veteran, disabled, dog_name,
    base_pain = T0_pain, base_depression = T0_depression, base_anxiety = T0_anxiety, base_total,
    later_pain = T1_pain, later_depression = T1_depression, later_anxiety = T1_anxiety, later_total,
    last_pain = T2_pain, last_depression = T2_depression, last_anxiety = T2_anxiety, last_total,
    change_pain, change_depression, change_anxiety, change_total,
    provider_male, provider, heart_rate, resp_rate, sp_o2, bp_syst, bp_diast, med_given,
    starts_with("mh_"), starts_with("ph_")
  ) %>%
  filter(!is.na(base_anxiety) & !is.na(later_anxiety))

write_csv(er, "data-raw/er.csv")
use_data(er, overwrite = TRUE, compress = "xz")
