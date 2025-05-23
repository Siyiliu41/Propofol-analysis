# 1
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

# 2

