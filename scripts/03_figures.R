# ----------------------------------------------------------------
# SOLO VERSUS JOINT ACTION IN A COSTLY COOPERATION TASK
#   -- Lalli, Al Afif, Tang, Hasan, Dissanayake, Sussman, Lokesh,
#      Rathwell, Cashaback, Carter
#
# Data visualization
#
# Author(s):
#   - Mikayla Lalli
#   - Mike Carter
# ----------------------------------------------------------------


#-- [ SCRIPT SETUP ] ----
source(here::here("scripts/02_analysis.R"))

#-- [[ Color and theme setup ]]
colour_theme1 <- c("#00b6eb", "#fb61d7", "#a58aff")
colour_theme2 <- c("#e69f00", "#56b4e9", "#009e73", "#cc79a7", "#f0e442")

ggplot2::theme_set(
  theme_classic() +
    theme(
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12, face = "bold"),
    )
)


#-- [ FIGURE 1: METHODS FIGURE ]
#-- Please note this figure was created using inkscape (https://inkscape.org).
#-- The .svg file is available in the fig directory of the project repository.


#-- [ FIGURE 2: CHOICE DATA ]
#--
#-- [[ Create the individual plots for multi-panel Fig 2:
#--    A) trial-by-trial together choices on choice trials
#--    B) together choices as a function of box number
#--    C) waffle plot of themes based on self-reported data ]]
fig2a <- ggplot(
  mean_together_choice_tib,
  aes(x = trial_number,
      y = mean_proportion)
  ) +
  geom_hline(
    aes(yintercept = 0.44),
    linetype = "dashed",
    linewidth = 0.45
    ) +
  geom_hline(
    aes(yintercept = 0.50),
    linetype = "solid",
    linewidth = 0.5
    ) +
  geom_line(
    linewidth = 0.5
    ) +
  geom_point() +
  labs(
    x = "Choice trial number",
    y = "Together choices") +
  scale_x_continuous(
    breaks = c(1, 15, 30, 45, 60, 75, 90),
    labels = c(1, 15, 30, 45, 60, 75, 90)
    ) +
  ylim(0, 1.0)
fig2a

fig2b <- ggplot(
  together_choices_b,
  aes(x = as.factor(box_number),
      y = proportion)
  ) +
  geom_point(
    aes(group = sub_id),
    colour = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_line(
    aes(group = sub_id),
    colour = "#d8dee9",
    position = position_dodge(0.3)
    ) +
  geom_boxplot(
    lwd = 1,
    fatten = 1,
    alpha = 0,
    color = colour_theme1
  ) +
  stat_summary(
    fun = "mean",
    geom = "segment",
    aes(xend = after_stat(x) + 0.25,
        yend = after_stat(y)),
    position = position_nudge(-0.125),
    linewidth = 1
  ) +
  scale_x_discrete(
    labels = c("4" = "Four",
               "8" = "Eight",
               "12" = "Twelve")
  ) +
  labs(
    x = "Number of boxes",
    y = "Together choices"
    ) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
    )
fig2b

fig2c <- qaire_tib |>
  group_by(theme) |>
  select(c(1, 2, 6)) |>
  summarise(frequency = sum(frequency)) |> 
  waffle(
    rows = 9,
    size = 0.5,
    title = ""
  ) +
  scale_fill_manual(
    values = colour_theme2,
    labels = c("utility" = "Chose actions with greater instrumental utility",
               "social value" = "Chose actions with greater social value",
               "flexibility" = "Flexibility in decision making",
               "movement path" = "Planned movement path",
               "reliability" = "Made reliable decisions"),
    name = NULL
    ) +
  theme(
    legend.position = "bottom",
    legend.key.spacing.y = unit(0.3, "lines"),
    legend.key.spacing.x = unit(1.5, "lines"),
    legend.text = element_text(size = 14, face = "bold")
    ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))
fig2c

fig2_design <- "
    12
    12
    33
    33
"

fig2 <- fig2a + fig2b + fig2c + plot_layout(design = fig2_design)
(fig2 + plot_annotation(
  tag_levels = 'A') & 
  theme(
    plot.tag = element_text(
      size = 22,
      face = "bold"))
)

ggsave(
  "fig2.pdf",
  device = "pdf",
  path = "figs/",
  width = 10,
  height = 8,
  units = "in",
  dpi = 300,
  #bg = "white"
)


#-- [ FIGURE 3: BEHAVIOUR DATA ]
#--
#-- [[ Create the individual plots for multi-panel Fig 2:
#--    A) mean trial time (2 trial mode x 3 box number)
#--    B) representative participant (x2) trajectory data
#--         - this fig was created in Inkscape
#--    C) mean distance traveled (2 trial mode x 3 box number) ]]

fig3_labels <- c("Forced Alone", "Forced Together")
names(fig3_labels) <- c(1, 2)

fig3a <- ggplot(
  mean_time_b_p_tm,
  aes(x = as.factor(box_number),
      y = mean_trial_time,
      color = as.factor(box_number))
    ) +
  geom_point(
    aes(group = interaction(sub_id, box_number, trial_mode)),
    colour = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_line(
    aes(group = sub_id),
    colour = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_boxplot(
    aes(fill = as.factor(trial_mode)),
    lwd = 1,
    fatten = 1,
    alpha = 0,
  ) +
  stat_summary(
    fun = "mean",
    geom = "segment",
    aes(xend = after_stat(x) + 0.25,
        yend = after_stat(y)),
    position = position_nudge(-0.125),
    linewidth = 1,
    color = "black"
  ) +
  scale_color_manual(
    values = colour_theme1
  ) +
  scale_x_discrete(
    name = NULL,
    labels = c("4" = "Four",
               "8" = "Eight",
               "12" = "Twelve")
  ) +
  scale_y_continuous(
    name = "Trial time (s)",
    breaks = seq(0, 20, 5),
    limits = c(0, 20)
  ) +
  facet_wrap(~trial_mode,
    strip.position = "bottom",
    labeller = labeller(trial_mode = fig3_labels)
  ) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.text.x = element_text(size = 14, face = "bold"),
    strip.background = element_blank(),
    strip.placement = "outside",
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14, face = "bold")
  )
fig3a


fig3b <- ggplot(
  mean_distance_b_p_tm,
  aes(x = as.factor(box_number),
      y = mean_distance,
      color = as.factor(box_number))
    ) +
  geom_point(
    aes(group = interaction(sub_id, box_number, trial_mode)),
    colour = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_line(
    aes(group = sub_id),
    colour = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_boxplot(
    aes(fill = as.factor(trial_mode)),
    lwd = 1,
    fatten = 1,
    alpha = 0,
  ) +
  stat_summary(
  fun = "mean",
  geom = "segment",
  aes(xend = after_stat(x) + 0.25,
      yend = after_stat(y)),
  position = position_nudge(-0.125),
  linewidth = 1,
  color = "black"
  ) +
  scale_color_manual(
    values = colour_theme1
  ) +
  scale_x_discrete(
    name = NULL,
    labels = c("4" = "Four",
               "8" = "Eight",
               "12" = "Twelve")
  ) +
  scale_y_continuous(
    name = "Distance traveled (m)",
    limits = c(0, 3)
  ) +
  facet_wrap(~trial_mode,
    strip.position = "bottom",
    labeller = labeller(trial_mode = fig3_labels)
  ) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.text.x = element_text(size = 14, face = "bold"),
    strip.background = element_blank(),
    strip.placement = "outside",
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14, face = "bold")
  )
fig3b


fig3_left <- fig3a / fig3b

(fig3_left + plot_annotation(
  tag_levels = "A") &
  theme(
    plot.tag = element_text(
      size = 22,
      face = "bold"
    )
  )
)

ggsave(
  "fig3_left.pdf",
  device = "pdf",
  path = "figs/",
  width = 8.5,
  height = 11,
  units = "in",
  dpi = 300,
  #bg = "white"
)




#test <- magick::image_read("figs/fig3c.png")
#
#test <- ggplot() +
#  ggpubr::background_image(test) + coord_fixed()
#test

#facet_labels_alt <- c("4 boxes", "8 boxes", "12 boxes")
#names(facet_labels_alt) <- c(4.00, 8.00, 12.00)
#
#fig3a_alt <- ggplot(
#  mean_time_b_p_tm,
#  aes(x = as.factor(trial_mode),
#      y = mean_trial_time,
#      color = trial_mode)
#    ) +
#  geom_point(
#    aes(group = interaction(sub_id, box_number, trial_mode)),
#    colour = "#d8dee9",
#    position = position_dodge(0.3)
#  ) +
#  geom_line(
#    aes(group = sub_id),
#    colour = "#d8dee9",
#    position = position_dodge(0.3)
#  ) +
#  geom_boxplot(
#    aes(fill = as.factor(box_number)),
#    lwd = 1,
#    fatten = 1,
#    alpha = 0
#  ) +
#  labs(
#    x = NULL,
#    y = "Trial time (s)"
#    ) +
#  facet_wrap(~box_number,
#    strip.position = "bottom",
#    labeller = labeller(box_number = facet_labels_alt)
#  ) +
#  theme(
#    legend.position = "none",
#    panel.grid.major = element_blank(),
#    panel.grid.minor = element_blank(),
#    panel.background = element_blank(),
#    axis.line = element_line(colour = "black"),
#    axis.text.x = element_blank(),
#    axis.title.y = element_text(size = 14, face = "bold"),
#    axis.text.y = element_text(size = 12, face = "bold"),
#    axis.ticks.x = element_blank(),
#    strip.text.x = element_text(size = 14, face = "bold"),
#    strip.background = element_blank(),
#    strip.placement = "outside"
#  )
#fig3a_alt
