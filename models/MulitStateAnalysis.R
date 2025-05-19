library(dplyr)
library(pammtools)
library(mgcv)
library(tidyr)
library(ggplot2)

setwd("~/Documents/Propofol-analysis/pammtools")
devtools::load_all()

# Erstellung einer Spalte, die die Dauer der Propofol-Therapie für jeden Patienten anzeigt
data_ordered <- data_ordered %>%
  group_by(CombinedID) %>%
  mutate(PropofolDuration = sum(as.numeric(as.character(Propofol)))) %>%
  ungroup()

data_actual <- data_ordered %>%
  mutate(
    actual_transition = paste0(from, "->", to),
    tstart = Study_Day - 1,
    tstop = Study_Day
  ) %>%
  select(
    CombinedID, tstart, tstop, from, to, actual_transition,
    Age, BMI, ApacheIIScore, inMV2_4, Propofol2_4, EN2_4, PN2_4, OralIntake2_4,
    Year, AdmCatID, DiagID2, Gender, CombinedicuID, DaysInICU, PropofolDuration
  )

extra_transitions <- data_ordered %>%
  group_by(CombinedID) %>%
  filter(Study_Day == 11) %>%
  filter(DaysInICU > 11) %>%
  mutate(
    tstart = 11,
    tstop = DaysInICU,
    from = to,
    to = case_when(
      PatientDischarged == 1 ~ 2,
      PatientDied == 1 ~ 3,
      TRUE ~ from
    ),
    actual_transition = paste0(from, "->", to),
    Study_Day = NA_integer_
  ) %>%
  select(
    CombinedID, tstart, tstop, from, to, actual_transition,
    Age, BMI, ApacheIIScore, inMV2_4, Propofol2_4, EN2_4, PN2_4, OralIntake2_4,
    Year, AdmCatID, DiagID2, Gender, CombinedicuID, DaysInICU, PropofolDuration
  ) %>%
  ungroup()

data_actual_final <- bind_rows(data_actual, extra_transitions)

data_expanded <- data_actual_final %>%
  mutate(
    possible_transitions = case_when(
      from == 0 ~ list(c("0->1", "0->2", "0->3")),
      from == 1 ~ list(c("1->0", "1->3")),
      TRUE ~ list(NA_character_)
    )
  ) %>%
  unnest(possible_transitions) %>%
  mutate(
    transition = possible_transitions,
    status = case_when(
      actual_transition == transition ~ 1,
      TRUE ~ 0
    ),
    id = CombinedID
  ) %>%
  select(
    id, tstart, tstop, from, to, transition, status,
    Age, BMI, ApacheIIScore, inMV2_4, Propofol2_4, EN2_4, PN2_4, OralIntake2_4,
    Year, AdmCatID, DiagID2, Gender, CombinedicuID, PropofolDuration
  ) %>%
  filter(!is.na(transition)) %>%
  mutate(transition = factor(transition))

expand_to_days <- function(df) {
  df %>%
    mutate(n = as.integer(round(tstop - tstart))) %>% 
    filter(n > 0) %>%
    uncount(n, .remove = FALSE) %>%
    group_by(id, from, to, transition, status, Age, BMI, ApacheIIScore, inMV2_4,
             Propofol2_4, EN2_4, PN2_4, OralIntake2_4, Year, AdmCatID, DiagID2,
             Gender, CombinedicuID, PropofolDuration) %>%
    mutate(
      tstart = tstart[1] + row_number() - 1,
      tstop = tstart + 1
    ) %>%
    ungroup()
}

data_expanded_days <- expand_to_days(data_expanded)

ped_data <- as_ped(
  data = data_expanded_days,
  formula = Surv(tstart, tstop, status) ~ transition + Age + BMI + ApacheIIScore +               inMV2_4 + Propofol2_4 + EN2_4 + PN2_4 + OralIntake2_4 + Year + AdmCatID + DiagID2 + Gender + CombinedicuID + PropofolDuration,
  id = "id",
  multiple_id = TRUE
)

model_propofol_dauer <- bam(
  ped_status ~ s(tend, by = transition) + PropofolDuration * transition +
    s(Age) + s(BMI) + ApacheIIScore + 
    inMV2_4 + Propofol2_4 + EN2_4 + PN2_4 + OralIntake2_4 +
    Year + AdmCatID + DiagID2 + Gender + 
    s(CombinedicuID, bs = "re"),
  family = poisson(),
  data = ped_data,
  offset = offset,
  method = "fREML",
  discrete = TRUE
)

summary(model_propofol_dauer)

# Define transitions and create grid
transitions <- c("0->1", "0->2", "0->3", "1->0", "1->3")

# Create prediction dataset
ndf_complex <- make_newdata(
  ped_data,
  tend = unique(tend),
  PropofolDuration = seq(min(ped_data$PropofolDuration), max(ped_data$PropofolDuration)),
  transition = unique(transition)
) %>%
  group_by(transition, PropofolDuration) %>%
  add_trans_prob(model_propofol_dauer)

# Contour plot
transition_labels <- c(
  "0->1" = "kein Propofol -> Propofol",
  "0->2" = "kein Propofol -> Überlebt",
  "0->3" = "kein Propofol -> Tod in ICU",
  "1->0" = "Propofol -> kein Propofol",
  "1->3" = "Propofol -> Tod in ICU")


ggplot(ndf_complex, aes(x = tend, y = PropofolDuration, z = trans_prob)) +
  geom_contour_filled() +
  facet_wrap(~ transition, labeller = labeller(transition = transition_labels)) +
  scale_fill_viridis_d(name = "Übergangs-\nwahrscheinlichkeit") +
  xlim(0, 60) +
  ylim(0, 7) +
  labs(
    title = "Transition Probabilities nach Aufenthalts- und Propofol-Dauer",
    x = "Zeit in ICU (Tage)",
    y = "Propofol-Therapiedauer (Tage)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold")
  )

ggsave("models/results/transition_probs.png", plot = last_plot(), width = 10, height = 6, dpi = 300,
       bg = "white")
# Extrahiere die parametrische Koeffizienten-Tabelle
p_tab <- summary(model_propofol_dauer)$p.table %>%
  as.data.frame(rownames = "term") %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    z = `z value`,
    p.value = `Pr(>|z|)`
  ) %>%
  # Berechne 95%-Konfidenzintervalle
  mutate(
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

# auch die Glättungstermine anzeigen
s_tab <- summary(model_propofol_dauer)$s.table %>%
  as.data.frame(rownames = "smooth") %>%
  rename(
    edf = edf,
    ref.df = Ref.df,
    chi.sq = `Chi.sq`,
    p.value = `p-value`
  )

# Zusammenführen zu einer "langen" Übersicht (parametrisch + smooth)
param_df <- p_tab %>% mutate(type = "parametric")
smooth_df <- s_tab %>% select(smooth, edf, ref.df, chi.sq, p.value) %>% 
  mutate(type = "smooth")

# Falls du nur parametrische Effekte in einer Tabelle brauchst, reiche param_df.
write.csv(param_df, "parametric_effects_table.csv", row.names = TRUE)