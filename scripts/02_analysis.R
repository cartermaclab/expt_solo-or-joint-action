# ----------------------------------------------------------------
# SOLO VERSUS JOINT ACTION IN A COSTLY COOPERATION TASK
#   -- Lalli, Al Afif, Tang, Hasan, Dissanayake, Sussman, Lokesh,
#      Rathwell, Cashaback, Carter
#
# Statistical analyses
#
# Author(s):
#   - Mikayla Lalli
#   - Mike Carter
# ----------------------------------------------------------------


#-- [ SCRIPT SETUP ] ----
source(here::here("scripts/01_wrangle.R"))


#-- [ BEHAVIOURAL DATA ]

#-- [[ Proportion of together choices ]]
total_choices_count <- choice_tib %>%
  dplyr::count(trial_mode)

choice_descriptives <- choices_p |> 
  dplyr::group_by(trial_mode) |> 
  dplyr::summarize(
    n = n(),
    mean_choice = mean(proportion, na.rm = TRUE),
    sd_choice = sd(proportion, na.rm = TRUE),
  ) |> 
  dplyr::ungroup()

#-- [[ Test proportion of together choices against 50% chance level and compute effect size ]]
wilcox.test(
  choices_p$proportion[choices_p$trial_mode == "4"],
  mu = 0.5,
  conf.int = TRUE
)
##-- V = 472.5, p = 0.112

rcompanion::wilcoxonOneSampleR(
  choices_p$proportion[choices_p$trial_mode == "4"],
  mu = 0.5,
  ci = TRUE)
##-- r = -0.225, [95 CI: -0.478, 0.068]


#-- [ PROPORTION OF TOGETHER CHOICES BY BOX NUMBER]

#-- [[ Omnibus test ]]
prop_together_aov <- afex::aov_ez(
  data = together_choices_b,
  id = "sub_id",
  dv = "proportion",
  within = "box_number"
)
prop_together_aov
##-- F(1.51, 74.06) = 16.43, p < .001, ges = .057

#-- [[ Post hoc tests and compute Cohen's d with Hedge's correction ]]
emmeans::emmeans(
  prop_together_aov,
  specs = "box_number"
) |> 
  pairs(adjust = "holm")
##--  contrast estimate     SE df t.ratio p.value
##--  X4 - X8    0.1047 0.0287 49   3.651  0.0013
##--  X4 - X12   0.1453 0.0305 49   4.762  0.0001
##--  X8 - X12   0.0407 0.0173 49   2.346  0.0231

effectsize::rm_d(
  together_choices_b_wide$`4`, together_choices_b_wide$`8`, adjust = FALSE
)
effectsize::rm_d(
  together_choices_b_wide$`4`, together_choices_b_wide$`12`, adjust = FALSE
)
effectsize::rm_d(
  together_choices_b_wide$`8`, together_choices_b_wide$`12`, adjust = FALSE
)
##-- 4 vs 8: d(rm) = 0.42, 95 CI [0.19, 0.66]
##-- 4 vs 12: d(rm) = 0.57, 95 CI [0.32, 0.83]
##-- 8 vs 12: d(rm) = 0.16, 95 CI [0.03, 0.30]


#-- [ TRIAL TIME IN FORCED ALONE AND TOGETHER TRIALS ]

#-- [[ Create tibbles of descriptives for trial time:
#--    - trial mode x box number
#--    - trial mode only
#--    - box number only ]]
trial_time_descriptives <- mean_time_b_p_tm |> 
  dplyr::group_by(trial_mode, box_number) |> 
  dplyr::summarize(
    n = n(),
    mean_time = mean(mean_trial_time, na.rm = TRUE),
    sd_time = sd(mean_trial_time, na.rm = TRUE)
  ) |> 
  dplyr::ungroup()

forced_time_descriptives <- mean_time_p_tm |> 
  dplyr::group_by(trial_mode) |> 
  dplyr::summarize(
    n = n(),
    mean_time = mean(mean_trial_time, na.rm = TRUE),
    sd_time = sd(mean_trial_time, na.rm = TRUE)
  ) |> 
  dplyr::ungroup()

box_time_descriptives <- mean_time_p_b |> 
  dplyr::group_by(box_number) |> 
  dplyr::summarize(
    n = n(),
    mean_time = mean(mean_trial_time, na.rm = TRUE),
    sd_time = sd(mean_trial_time, na.rm = TRUE)
  ) |> 
  dplyr::ungroup()

#-- [[ Omnibus test ]]
trial_time_aov <- afex::aov_ez(
  data = mean_time_b_p_tm,
  id = "sub_id",
  dv = "mean_trial_time",
  within = c("trial_mode", "box_number")
)
trial_time_aov
##-- Main effect trial mode: F(1, 49) = 74.33, p < .001, ges = .323
##-- Main effect box number: F(1.57, 76.85) = 266.51, p < .001, ges = .422
##-- Interaction: F(1.52, 74.57) = 56.57, p < .001, ges = .130


#-- [[ Post hoc interaction and compute Cohen's d with Hedge's correction ]]
emmeans::emmeans(
  trial_time_aov,
  specs = c("trial_mode", "box_number")
) |> 
  pairs(adjust = "holm")
##-- X1 == alone; X2 == together
##-- X4, X8, X12 == 4, 8, and 12 boxes, respectively
##-- contrast        estimate     SE df t.ratio p.value
##-- X1 X4 - X2 X4     -0.848 0.2140 49  -3.965  0.0005  [*]
##-- X1 X4 - X1 X8     -1.072 0.0365 49 -29.393  <.0001  [*]
##-- X1 X4 - X2 X8     -3.103 0.2050 49 -15.114  <.0001
##-- X1 X4 - X1 X12    -1.897 0.0606 49 -31.280  <.0001  [*]
##-- X1 X4 - X2 X12    -5.830 0.4590 49 -12.696  <.0001
##-- X2 X4 - X1 X8     -0.224 0.2140 49  -1.047  0.3003
##-- X2 X4 - X2 X8     -2.255 0.1980 49 -11.418  <.0001  [*]
##-- X2 X4 - X1 X12    -1.049 0.2160 49  -4.863  <.0001
##-- X2 X4 - X2 X12    -4.982 0.3350 49 -14.893  <.0001  [*]
##-- X1 X8 - X2 X8     -2.031 0.1990 49 -10.200  <.0001  [*]
##-- X1 X8 - X1 X12    -0.825 0.0339 49 -24.324  <.0001  [*]
##-- X1 X8 - X2 X12    -4.758 0.4550 49 -10.469  <.0001
##-- X2 X8 - X1 X12     1.207 0.2010 49   6.006  <.0001
##-- X2 X8 - X2 X12    -2.727 0.3230 49  -8.430  <.0001  [*]
##-- X1 X12 - X2 X12   -3.934 0.4570 49  -8.617  <.0001  [*]
##-- P value adjustment: holm method for 15 tests

effectsize::rm_d(
  mean_time_b_p_tm_wide$alone_4, mean_time_b_p_tm_wide$alone_8, adjust = FALSE
)
effectsize::rm_d(
  mean_time_b_p_tm_wide$alone_4, mean_time_b_p_tm_wide$alone_12, adjust = FALSE
)
effectsize::rm_d(
  mean_time_b_p_tm_wide$alone_8, mean_time_b_p_tm_wide$alone_12, adjust = FALSE
)

effectsize::rm_d(
  mean_time_b_p_tm_wide$together_4, mean_time_b_p_tm_wide$together_8, adjust = FALSE
)
effectsize::rm_d(
  mean_time_b_p_tm_wide$together_4, mean_time_b_p_tm_wide$together_12, adjust = FALSE
)
effectsize::rm_d(
  mean_time_b_p_tm_wide$together_8, mean_time_b_p_tm_wide$together_12, adjust = FALSE
)

effectsize::rm_d(
  mean_time_b_p_tm_wide$alone_4, mean_time_b_p_tm_wide$together_4, adjust = FALSE
)
effectsize::rm_d(
  mean_time_b_p_tm_wide$alone_8, mean_time_b_p_tm_wide$together_8, adjust = FALSE
)
effectsize::rm_d(
  mean_time_b_p_tm_wide$alone_12, mean_time_b_p_tm_wide$together_12, adjust = FALSE
)
##-- A4 vs A8: d(rm) = 1.47, 95CI [1.33, 1.61]
##-- A4 vs A12: d(rm) = 1.50, 95CI [1.36, 1.64]
##-- A8 vs A12: d(rm) = 0.85, 95CI [0.77, 0.93]
##--
##-- T4 vs T8: d(rm) = 1.44, 95CI [1.09, 1.80]
##-- T4 vs T12: d(rm) = 1.52, 95CI [1.23, 1.81]
##-- T8 vs T12: d(rm) = 0.79, 95CI [0.58, 1.01]
##--
##-- A4 vs T4: d(rm) = 0.68, 95CI [0.31, 1.05]
##-- A8 vs T8: d(rm) = 1.54, 95CI [1.10, 1.98] 
##-- A12 vs T12: d(rm) = 1.56, 95CI [1.03, 2.09]


#-- [ DISTANCE TRAVELED IN FORCED ALONE AND TOGETHER TRIALS ]
#--
#-- [[ Create tibbles of descriptives for trial time:
#--    - trial mode x box number
#--    - trial mode only
#--    - box number only ]]
distance_descriptives <- mean_distance_b_p_tm |> 
  dplyr::group_by(trial_mode, box_number) |> 
  dplyr::summarize(
    n = n(),
    mean_time = mean(mean_distance, na.rm = TRUE),
    sd_time = sd(mean_distance, na.rm = TRUE)
  ) |> 
  dplyr::ungroup()

forced_distance_descriptives <- mean_distance_p_tm |> 
  dplyr::group_by(trial_mode) |> 
  dplyr::summarize(
    n = n(),
    mean_time = mean(mean_distance, na.rm = TRUE),
    sd_time = sd(mean_distance, na.rm = TRUE)
  ) |> 
  dplyr::ungroup()

box_distance_descriptives <- mean_distance_p_b |> 
  dplyr::group_by(box_number) |> 
  dplyr::summarize(
    n = n(),
    mean_time = mean(mean_distance, na.rm = TRUE),
    sd_time = sd(mean_distance, na.rm = TRUE)
  ) |> 
  dplyr::ungroup()


#-- [[ Omnibus test ]]
distance_aov <- afex::aov_ez(
  data = mean_distance_b_p_tm,
  id = "sub_id",
  dv = "mean_distance",
  within = c("trial_mode", "box_number")
)
distance_aov
##-- Main effect trial mode: F(1, 49) = 58.13, p < .001, ges = .255
##-- Main effect box number: F(1.31, 64.22) = 370.88, p < .001, ges = .577
##-- Interaction: F(1.30, 63.69) = 62.23, p < .001, ges = .173

#-- [[ Post hoc interaction and compute effect size ]]
emmeans::emmeans(
  distance_aov,
  specs = c("trial_mode", "box_number")
) |> 
  pairs(adjust = "holm")
##-- X1 == alone; X2 == together
##-- X4, X8, X12 == 4, 8, and 12 boxes, respectively
##-- contrast        estimate     SE df t.ratio p.value
##-- X1 X4 - X2 X4    -0.0189 0.01610 49  -1.173  0.2466  [*]
##-- X1 X4 - X1 X8    -0.2388 0.00625 49 -38.232  <.0001  [*]
##-- X1 X4 - X2 X8    -0.5066 0.03360 49 -15.088  <.0001
##-- X1 X4 - X1 X12   -0.4183 0.00782 49 -53.480  <.0001  [*]
##-- X1 X4 - X2 X12   -0.9763 0.07300 49 -13.381  <.0001
##-- X2 X4 - X1 X8    -0.2199 0.01690 49 -13.027  <.0001
##-- X2 X4 - X2 X8    -0.4877 0.02750 49 -17.756  <.0001  [*]
##-- X2 X4 - X1 X12   -0.3994 0.01700 49 -23.472  <.0001
##-- X2 X4 - X2 X12   -0.9573 0.06150 49 -15.566  <.0001  [*]
##-- X1 X8 - X2 X8    -0.2677 0.03250 49  -8.226  <.0001  [*]
##-- X1 X8 - X1 X12   -0.1795 0.00589 49 -30.476  <.0001  [*]
##-- X1 X8 - X2 X12   -0.7374 0.07180 49 -10.269  <.0001
##-- X2 X8 - X1 X12    0.0883 0.03160 49   2.790  0.0150
##-- X2 X8 - X2 X12   -0.4697 0.05170 49  -9.093  <.0001  [*]
##-- X1 X12 - X2 X12  -0.5579 0.07150 49  -7.805  <.0001  [*]
##-- P value adjustment: holm method for 15 tests

effectsize::rm_d(
  mean_distance_b_p_tm_wide$alone_4, mean_distance_b_p_tm_wide$alone_8, adjust = FALSE
)
effectsize::rm_d(
  mean_distance_b_p_tm_wide$alone_4, mean_distance_b_p_tm_wide$alone_12, adjust = FALSE
)
effectsize::rm_d(
  mean_distance_b_p_tm_wide$alone_8, mean_distance_b_p_tm_wide$alone_12, adjust = FALSE
)

effectsize::rm_d(
  mean_distance_b_p_tm_wide$together_4, mean_distance_b_p_tm_wide$together_8, adjust = FALSE
)
effectsize::rm_d(
  mean_distance_b_p_tm_wide$together_4, mean_distance_b_p_tm_wide$together_12, adjust = FALSE
)
effectsize::rm_d(
  mean_distance_b_p_tm_wide$together_8, mean_distance_b_p_tm_wide$together_12, adjust = FALSE
)

effectsize::rm_d(
  mean_distance_b_p_tm_wide$alone_4, mean_distance_b_p_tm_wide$together_4, adjust = FALSE
)
effectsize::rm_d(
  mean_distance_b_p_tm_wide$alone_8, mean_distance_b_p_tm_wide$together_8, adjust = FALSE
)
effectsize::rm_d(
  mean_distance_b_p_tm_wide$alone_12, mean_distance_b_p_tm_wide$together_12, adjust = FALSE
)
##-- A4 vs A8: d(rm) = 4.67, 95CI [3.84, 5.49]
##-- A4 vs A12: d(rm) = 5.27, 95CI [4.52, 6.01]
##-- A8 vs A12: d(rm) = 2.20, 95CI [1.94, 2.47]
##--
##-- T4 vs T8: d(rm) = 2.14, 95CI [1.71, 2.56]
##-- T4 vs T12: d(rm) = 1.56, 95CI [1.27, 1.85]
##-- T8 vs T12: d(rm) = 0.88, 95CI [0.66, 1.11]
##--
##-- A4 vs T4: d(rm) = 0.19, 95CI [0.13, 0.52]
##-- A8 vs T8: d(rm) = 1.26, 95CI [0.86, 1.67] 
##-- A12 vs T12: d(rm) = 1.38, 95CI [0.89, 1.86]
