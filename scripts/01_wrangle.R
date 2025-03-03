# ----------------------------------------------------------------
# SOLO VERSUS JOINT ACTION IN A COSTLY COOPERATION TASK
#   -- Lalli, Al Afif, Tang, Hasan, Dissanayake, Sussman, Lokesh,
#      Rathwell, Cashaback, Carter
#
# Data wrangling
#
# Author(s):
#   - Mikayla Lalli
#   - Mike Carter
# ----------------------------------------------------------------


#-- [ SCRIPT SETUP ] ----
source(here::here("scripts/00_libraries-and-functions.R"))

#-- [[ Load data files ]]
motor_data <- readr::read_csv(
  here("data/behaviour-data.csv"),
  col_select = c(sub_id = study_id, everything())
)

qaire_data <- readr::read_csv(
  here("data/questionnaire-data.csv")
)

#-- [[ Create workable tibbles and make variables factors ]]
motor_tib <- motor_data |> 
  dplyr::mutate(
    sub_id = forcats::as_factor(sub_id)
  )

qaire_tib <- qaire_data |>
  dplyr::mutate(
    question = as_factor(question),
    theme = as_factor(theme),
    sub_theme = as_factor(sub_theme),
    initial_theme = as_factor(initial_theme),
    code = as_factor(code)) |>
  dplyr::group_by(theme, sub_theme, initial_theme, code) |>
  dplyr::arrange(question, .by_group = TRUE)


#-- [ CHOICE PROPORTIONS IN CHOICE TRIALS ] ----

#-- [[ Create needed tibbles and get proportions at the participant (p)
#--    level then add choice value of 0 for participants that never chose 
#--    the together option on choice trials and append to tibble ]]
choice_tib <- motor_tib |>
  #-- Trial mode 3 == choice-alone and trial mode 4 == choice-together
  dplyr::filter(trial_mode == 3 | trial_mode == 4)

choices_p <- choice_tib |>
  dplyr::group_by(sub_id) |>
  dplyr::count(trial_mode) |>
  dplyr::rename("choices" = "n") |>
  dplyr::mutate(proportion = choices/90)

together_s107 <- data.frame(
  sub_id = "s107",
  trial_mode = 4,
  choices = 0,
  proportion = 0)

together_s212 <- data.frame(
  sub_id = "s212",
  trial_mode = 4,
  choices = 0,
  proportion = 0)

choices_p <- rbind(
  choices_p,
  together_s107,
  together_s212
) |>
  dplyr::arrange(sub_id) |> 
  dplyr::mutate(trial_mode = forcats::as_factor(trial_mode))


#-- [[ Create a tibble with proportion of together choices at the box number
#--    (b) level and grouped by participant. Add rows with choice values of 0
#--    for those who never chose together under certain box number conditions
#--    then append to the main tibble ]]
together_choices_b <- choice_tib |>
  dplyr::filter(trial_mode == 4) |>
  dplyr::select(sub_id,
                box_number) |>
  dplyr::group_by(sub_id) |>
  dplyr::count(box_number) |>
  dplyr::mutate(proportion = n/30) |> #-- 30 choice trials for each box number
  dplyr::rename("together_choices" = "n")

s107 <- data.frame(
  "sub_id" = c("s107", "s107", "s107"),
  "box_number" = c(4, 8, 12),
  "together_choices" = c(0, 0, 0),
  "proportion" = c(0, 0, 0))

s207 <- data.frame(
  "sub_id" = c("s207", "s207"),
  "box_number" = c(4, 8),
  "together_choices" = c(0, 0),
  "proportion" = c(0, 0))

s212 <- data.frame(
  "sub_id" = c("s212", "s212", "s212"),
  "box_number" = c(4, 8, 12),
  "together_choices" = c(0, 0, 0),
  "proportion" = c(0, 0, 0))

s305 <- data.frame(
  "sub_id" = c("s305"),
  "box_number" = c(12),
  "together_choices" = c(0),
  "proportion" = c(0))

s309 <- data.frame(
  "sub_id" = c("s309"),
  "box_number" = c(12),
  "together_choices" = c(0),
  "proportion" = c(0))

s313 <- data.frame(
  "sub_id" = c("s313", "s313"),
  "box_number" = c(8, 12),
  "together_choices" = c(0, 0),
  "proportion" = c(0, 0))

together_choices_b <- rbind(
  together_choices_b,
  s107,
  s207,
  s212,
  s305,
  s309,
  s313
) |>
  dplyr::arrange(sub_id) |>
  dplyr::group_by(sub_id) |> 
  dplyr::mutate(box_number = forcats::as_factor(box_number))

#-- [[ Create a wide format tibble of together_choices_b ]]
together_choices_b_wide <- tidyr::pivot_wider(
  together_choices_b,
  id_cols = sub_id,
  names_from = box_number,
  values_from = proportion
)

#-- [[ Create a tibble with mean proportion of choices across all 
#--    participants for all trials. Then create a column with mean proportion
#--    of choices for each trial in each trial mode. Next create a tibble 
#--    with mean proportion of together choices across all participants and
#--    change trial numbers to be 1 to 90 ]]
mean_choice_tib <- choice_tib |>
  dplyr::group_by(trial_number) |>
  dplyr::count(trial_mode) |>
  dplyr::rename("choices" = "n") |>
  dplyr::mutate(proportion = choices/50)

mean_choice_tib <- mean_choice_tib |>
  dplyr::group_by(trial_number, trial_mode) |>
  dplyr::summarise(mean_proportion = mean(proportion))

mean_together_choice_tib <- mean_choice_tib |>
  dplyr::filter(trial_mode == 4)

mean_together_choice_tib$trial_number[1:90] <- c(1:90)


#-- [ TRIAL TIME IN THE FORCED ALONE AND TOGETHER TRIALS ] ----

#-- [[ Create tibble for trial time outcome variable then create a tibble
#--    with mean trial times at the participant (p) level and grouped by
#--    trial mode (tm). Also create a tibble with mean times at the box
#--    number (b) level grouped by participant and trial mode ]]
time_tib <- motor_tib |>
  #-- Trial mode 1 == forced alone and trial mode 2 == forced together
  dplyr::filter(trial_mode == 1 | trial_mode == 2)

mean_time_p_tm <- time_tib |>
  dplyr::group_by(sub_id, trial_mode) |>
  dplyr::summarise(mean_trial_time = mean(trial_time), .groups = "keep") |>
  #-- Convert mean time from ms to s
  dplyr::mutate(mean_trial_time = mean_trial_time/1000)

mean_time_b_p_tm <- time_tib |>
  dplyr::group_by(sub_id, trial_mode, box_number) |>
  dplyr::summarise(mean_trial_time = mean(trial_time), .groups = "keep") |>
  #-- Convert trial time from ms to s
  dplyr::mutate(mean_trial_time = mean_trial_time/1000)

#-- [[ Create a wide format tibble of mean trial times and rename columns ]]
mean_time_b_p_tm_wide <- tidyr::pivot_wider(
  mean_time_b_p_tm,
  id_cols = sub_id,
  names_from = c(trial_mode, box_number),
  values_from = mean_trial_time
)

mean_time_b_p_tm_wide <- mean_time_b_p_tm_wide |>
  dplyr::rename(
    alone_12 = "1_12",
    alone_4 = "1_4",
    alone_8 = "1_8",
    together_4 = "2_4",
    together_8 = "2_8",
    together_12 = "2_12"
  ) |>
  dplyr::select(1, 3, 4, 2, everything())

#-- [[ Create a tibble with mean times at the box number (b) level grouped
#--    by participant and also make one in wide format ]]
mean_time_p_b <- time_tib |>
  dplyr::group_by(sub_id, box_number) |>
  dplyr::summarise(mean_trial_time = mean(trial_time), .groups = "keep") |>
  #-- Convert trial time from ms to s
  dplyr::mutate(mean_trial_time = mean_trial_time/1000)

mean_time_p_b_wide <- tidyr::pivot_wider(
  mean_time_p_b,
  id_cols = sub_id,
  names_from = c(box_number),
  values_from = mean_trial_time
)


#-- [ DISTANCE TRAVELED IN FORCED ALONE AND TOGETHER TRIALS ]

#-- [[ Create needed tibbles for distance travelled outcome variable:
#--    i) tibble with mean distance traveled (in metres) at the participant (p)
#--    level grouped by trial mode (tm)
#--    ii) tibble with mean distance traveled at the participant level (p) and
#--    grouped by box number (b)
#--    iii) tibble with mean distance traveled at the participant (p) level and
#--     grouped by box number (b) in wide format
#--    iv) tibble with mean distance traveled at the box number (b) level and
#--    grouped by participant (p) and trial mode (tm); also make a wide one ]]
distance_tib <- motor_tib |>
  #-- Trial mode 1 == forced alone and trial mode 2 == forced together
  dplyr::filter(trial_mode == 1 | trial_mode == 2)

mean_distance_p_tm <- distance_tib |>
  dplyr::group_by(
    sub_id,
    trial_mode
  ) |>
  dplyr::summarise(
    mean_distance = mean(distance_traveled),
    groups = "keep"
  ) |>
  dplyr::ungroup()

mean_distance_p_b <- distance_tib |>
  dplyr::group_by(
    sub_id, 
    box_number
  ) |>
  dplyr::summarise(
    mean_distance = mean(distance_traveled),
    .groups = "keep"
  ) |>
  dplyr::ungroup()

mean_distance_p_b_wide <- tidyr::pivot_wider(
  mean_distance_p_b,
  id_cols = sub_id,
  names_from = (box_number),
  values_from = mean_distance)

mean_distance_b_p_tm <- distance_tib |>
  dplyr::group_by(
    sub_id,
    trial_mode, 
    box_number
  ) |>
  dplyr::summarise(
    mean_distance = mean(distance_traveled), 
    .groups = "keep"
  )

mean_distance_b_p_tm_wide <- tidyr::pivot_wider(
  mean_distance_b_p_tm,
  id_cols = sub_id,
  names_from = c(trial_mode, box_number),
  values_from = mean_distance
)

mean_distance_b_p_tm_wide <- mean_distance_b_p_tm_wide |>
  dplyr::rename(
    alone_12 = "1_12",
    alone_4 = "1_4",
    alone_8 = "1_8",
    together_4 = "2_4",
    together_8 = "2_8",
    together_12 = "2_12"
  )
