# 1 BMI
model_BMI <- bam(
  ped_status ~ s(tend, by = transition) + BMI * transition +
    s(Age) + ApacheIIScore + 
    inMV2_4 + Propofol2_4 + EN2_4 + PN2_4 + OralIntake2_4 +
    Year + AdmCatID + DiagID2 + Gender + 
    s(CombinedicuID, bs = "re"),
  family = poisson(),
  data = ped_data,
  offset = offset,
  method = "fREML",
  discrete = TRUE
)

summary(model_BMI)

# Define transitions and create grid
transitions <- c("0->1", "0->2", "0->3", "1->0", "1->3")

# Create prediction dataset
ndf_complex <- make_newdata(
  ped_data,
  tend = unique(tend),
  BMI = seq(min(ped_data$BMI), max(ped_data$BMI)),
  transition = unique(transition)
) %>%
  group_by(transition, BMI) %>%
  add_trans_prob(model_BMI)

summary(ndf_BMI)

# Contour plot
transition_labels <- c(
  "0->1" = "kein Propofol -> Propofol",
  "0->2" = "kein Propofol -> Überlebt",
  "0->3" = "kein Propofol -> Tod in ICU",
  "1->0" = "Propofol -> kein Propofol",
  "1->3" = "Propofol -> Tod in ICU")


ggplot(ndf_BMI, aes(x = tend, y = PropofolDuration, z = trans_prob)) +
  geom_contour_filled() +
  facet_wrap(~ transition, labeller = labeller(transition = transition_labels)) +
  scale_fill_viridis_d(name = "Übergangs-\nwahrscheinlichkeit") +
  xlim(0, 60) +
  ylim(0, 7) +
  labs(
    title = "Übergangswahrscheinlichkeiten nach Aufenthalts- und Propofol-Dauer",
    x = "Zeit in ICU (Tage)",
    y = "Propofol-Therapiedauer (Tage)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold")
  )

ggsave("transition_probs_bmi.png", plot = last_plot(), width = 10, height = 6, dpi = 300)

# 2 Gender
library(survival)
library(mgcv)
library(ggplot2)
library(pammtools)
library(purrr)
library(mstate)
library(checkmate)
library(dplyr)


model_gender <- bam(
  formula = ped_status ~ s(tend, by = interaction(transition, Gender)) + transition +
    Gender + s(Age) + s(BMI) +
    ApacheIIScore + inMV2_4 + Propofol2_4 + EN2_4 + PN2_4 + OralIntake2_4 +
    Year + AdmCatID + DiagID2 +
    s(CombinedicuID, bs = "re"),
  family = poisson(), 
  data = ped_data,
  offset = offset,
  method = "fREML",
  discrete = TRUE
)

summary(model_gender)

nd_gender <- make_newdata(
  ped_data,
  tend = unique(tend),
  Gender = c("Female", "Male"),
  transition = unique(transition)
) %>%
  group_by(transition, Gender) %>%
  add_trans_prob(model_gender)

ggplot(nd_gender, aes(x = tend)) +
  geom_line(aes(y = trans_prob, col = Gender), linewidth = 0.7) +
  facet_wrap(~ transition , labeller = labeller(transition = transition_labels)) +
  scale_color_manual(values = c("Male" = "#1f77b4", "Female" = "#d62728")) +
  labs(
    title = "Übergangswahrscheinlichkeiten nach Zeit in ICU und Geschlecht",
    x = "Zeit in ICU (Tage)",
    y = "Übergangswahrscheinlichkeit",
    color = "Geschlecht"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    strip.text = element_text(size = 13, face = "bold"),
    legend.position = "top",
    plot.title = element_text(size = 16, face = "bold")
  )

ggsave("transition_probs_gender.png", plot = last_plot(), width = 10, height = 6, dpi = 300)

ci_transitions_to_include <- c("0->2", "0->3", "1->3")

ped_data_male <- ped_data %>%
  filter(Gender == "Male")

ped_data_female <- ped_data %>%
  filter(Gender == "Female")

model_male <- bam(
  formula = ped_status ~ s(tend, by = transition) + transition + s(Age) + s(BMI) +
    ApacheIIScore + inMV2_4 + Propofol2_4 + EN2_4 + PN2_4 + OralIntake2_4 +
    Year + AdmCatID + DiagID2 +
    s(CombinedicuID, bs = "re"),
  family = poisson(), 
  data = ped_data_male,
  offset = offset,
  method = "fREML",
  discrete = TRUE
)

model_male <- bam(
  formula = ped_status ~ s(tend, by = transition) + transition + s(Age) + s(BMI) +
    ApacheIIScore + inMV2_4 + Propofol2_4 + EN2_4 + PN2_4 + OralIntake2_4 +
    Year + AdmCatID + DiagID2 +
    s(CombinedicuID, bs = "re"),
  family = poisson(), 
  data = ped_data_male,
  offset = offset,
  method = "fREML",
  discrete = TRUE
)

ndf_complex_male <- make_newdata(
  ped_data_male,
  tend = unique(tend),
  transition = ci_transitions_to_include
) %>%
  group_by(transition) |>
  add_trans_prob(model_male, ci = TRUE)

ndf_complex_female <- make_newdata(
  ped_data_female,
  tend = unique(tend),
  transition = ci_transitions_to_include
) %>%
  group_by(transition) |>
  add_trans_prob(model_female, ci = TRUE)

ndf_gender_ci <- bind_rows(ndf_complex_female, ndf_complex_male)

ggplot(ndf_gender_ci, aes(x = tend)) +
  geom_line(aes(y = trans_prob, col = Gender), linewidth = 0.8) +
  geom_ribbon(aes(ymin = trans_lower, ymax = trans_upper, fill = Gender), alpha = 0.4) +
  facet_wrap(~ transition , labeller = labeller(transition = transition_labels)) +
  scale_color_manual(values = c("Male" = "steelblue", "Female" = "firebrick2")) +
  labs(
    title = "Übergangswahrscheinlichkeiten nach Zeit in ICU und Geschlecht",
    x = "Zeit in ICU (Tage)",
    y = "Übergangswahrscheinlichkeit",
    color = "Geschlecht",
    fill = "Geschlecht"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    strip.text = element_text(size = 13, face = "bold"),
    legend.position = "top",
    plot.title = element_text(size = 16, face = "bold")
  )

ggsave("transition_probs_gender_ci.png", plot = last_plot(), width = 10, height = 6, dpi = 300)

# 3