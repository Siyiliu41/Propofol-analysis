#read the rds data
data_merged <- readRDS("~/Propofol-analysis/data/mergedAndCleanedData.Rds"")
data_unique <- data_merged %>%
  group_by(CombinedID) %>%
  summarise(
    status = max(surv_icu_status, na.rm = TRUE),  
    DaysInICU = max(DaysInICU, na.rm = TRUE),  
    propofol = ifelse(all(Propofol[Study_Day >= 5] == 0, na.rm = TRUE), 0, 1),  
    .groups = "drop"
  )

data_patient <- readRDS("~/Propofol-analysis/data/patient.Rds")

data_unique <- data_unique %>%
  left_join(data_patient %>% select(CombinedID, Age), by = "CombinedID")

status_table <- table(data_unique$status)
print(status_table)

event_count <- sum(data_unique$status != 0)
print(event_count)

library(ggplot2)
library(dplyr)


status_labels <- c(
  "0" = "Rechtszensiert",
  "1" = "Entlassen",
  "2" = "Verstorben"
)


status_df <- as.data.frame(table(data_unique$status)) %>%
  rename(code = Var1, count = Freq) %>%
  mutate(
    status = recode(code, !!!status_labels),
    percent = count / sum(count) * 100
  ) %>%
  arrange(desc(percent)) %>%
  mutate(status = factor(status, levels = status))


farben <- c(
  "Rechtszensiert" = "#7BAFD4",  
  "Entlassen" = "#90D190",       
  "Verstorben" = "#F08080"       
)


ggplot(status_df, aes(x = status, y = percent, fill = status)) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(percent, 1), "%\n(", count, ")")),
    vjust = -0.5,
    size = 5
  ) +
  scale_fill_manual(values = farben) +
  labs(
    title = "Verteilung des ICU-Status der Patienten",
    x = "Patientenstatus",
    y = "Prozentsatz"
  ) +
  ylim(0, max(status_df$percent) + 10) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")  


library(dplyr)
library(ggplot2)

data_propofol_status <- data_merged %>%
  group_by(CombinedID) %>%
  summarise(
    propofol = ifelse(any(Propofol == 1, na.rm = TRUE), 1, 0),
    .groups = "drop"
  )


data_unique <- data_merged %>%
  group_by(CombinedID) %>%
  summarise(
    status = max(surv_icu_status, na.rm = TRUE),
    DaysInICU = max(DaysInICU, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(patient %>% select(CombinedID, Age), by = "CombinedID") %>%
  left_join(data_propofol_status, by = "CombinedID")


status_labels <- c(
  "0" = "Rechtszensiert",
  "1" = "Entlassen",
  "2" = "Verstorben"
)


status_by_propofol <- data_unique %>%
  mutate(
    propofol_group = ifelse(propofol == 1, "Propofol verwendet", "Kein Propofol"),
    status = recode(as.character(status), !!!status_labels)
  ) %>%
  group_by(propofol_group, status) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(propofol_group) %>%
  mutate(percent = count / sum(count) * 100) %>%
  ungroup()


status_by_propofol <- status_by_propofol %>%
  group_by(propofol_group) %>%
  arrange(desc(percent)) %>%
  mutate(status = factor(status, levels = unique(status))) %>%
  ungroup()

farben <- c(
  "Rechtszensiert" = "#7BAFD4",
  "Entlassen" = "#90D190",
  "Verstorben" = "#F08080"
)

ggplot(status_by_propofol, aes(x = status, y = percent, fill = status)) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(percent, 1), "%\n(", count, ")")),
    vjust = -0.5,
    size = 4
  ) +
  facet_wrap(~propofol_group) +
  scale_fill_manual(values = farben) +
  labs(
    title = "ICU-Status nach Propofol-Gebrauch",
    x = "Patientenstatus",
    y = "Prozentsatz"
  ) +
  ylim(0, max(status_by_propofol$percent) + 10) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 9)
  )
