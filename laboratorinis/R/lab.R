library(tidyverse)
library(readr)

#duomenų paruošimas
Duomenys <- read_csv("../data/lab_sodra.csv")

FiltruotiDuomenys <- Duomenys %>%
  filter(ecoActCode == 620100)

# 1 užduotis
#išvedame grafiką 
FiltruotiDuomenys %>%
  ggplot(aes(x = avgWage)) +
  geom_histogram(bins = 200) +
  theme_classic() +
  labs(title = "Average wage for company employee", x = "Wage", y = "Count")

# 2 užduotis
#surandame 5 įmones su didžiausiu darbo užmokesčiu
Top5 <- FiltruotiDuomenys %>%
  group_by(name) %>%
  summarise(wage = max(avgWage)) %>%
  arrange(desc(wage)) %>%
  head(5)

#pagal top5 filtruojame duomenis
Top5Duomenys <- FiltruotiDuomenys %>% filter(name %in% Top5$name)

#išvedame grafiką
Top5Duomenys %>%
  mutate(month = as.Date(paste0(month, "01"), format = "%Y%m%d")) %>%
  ggplot(aes(x = month, y = avgWage, group = name)) +
  geom_point(aes(colour = name)) +
  geom_line(aes(colour = name)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme_classic() +
  labs(title = "Top 5 companies average wage change during a year", x = "Month", y = "Average wage")

# 3 užduotis
#išvedame grafiką
Top5Duomenys %>%
  group_by(name) %>%
  summarise(Counts = max(numInsured)) %>%
  mutate(name = fct_reorder(name, desc(Counts))) %>%
  ggplot(aes(x = name, y = Counts)) +
  geom_col(aes(fill = name)) +
  theme_classic() +
  labs(title = "Insured employees count", x = "Company", y = "Insured employees")
