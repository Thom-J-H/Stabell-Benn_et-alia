## Randomised clinical trials of COVID-19 vaccines: do adenovirus-vector 
## vaccines have beneficial non-specific effects?

library(tidyverse)
library(here)
library(scales)


# Load and Wrangle Data ---------------------------------------------------

danish_study <- read_csv(here::here("data", "raw_data", "danish_study.txt"))
##
##  https://ssrn.com/abstract=4072489 
##  http://dx.doi.org/10.2139/ssrn.4072489
##

danish_study %>% glimpse()

danish_study$Concern <- factor(danish_study$Concern, 
                               levels = c("Overall Mortality",
                                          "Covid-19 Deaths",
                                          "Cardiovascular Deaths",
                                          "Other (Non-accidental)") )


## Reverse index to plot categories preferred order
danish_study$Index <- 0 - danish_study$Index  

danish_study %>% visdat::vis_dat()

danish_study %>% glimpse()

## All good!

save(danish_study, file = here::here("data", "tidy_data", "danish_study.rda"))

rm(danish_study)

load(file = here::here("data", "tidy_data", "danish_study.rda"))

# Graphs ------------------------------------------------------------------


## Plot ALL -- Facet

dan_plot <- danish_study %>%
  ggplot( aes(y = Vaccine_Type, x = Rel_Risk,
              xmin = Low_95,
              xmax= High_95,
              color = Vaccine_Type)) +
  facet_wrap(~Concern) +
  geom_point() +   
  geom_errorbar() +
  scale_x_continuous(trans = "log10",
                     breaks = c(0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10) ) +
  guides(color = "none") +
  scale_color_manual(values = c("blue", "green") ) +
  geom_vline(xintercept = 1, lty = 2, color = "red") +
  theme_light()

## Add labels

dan_plot +
  labs(title = "Disparities in all-cause mortality between mRNA and adenovirus vaccines",
       subtitle = "Stabell-Benn, et alia. The Lancet (5 Apr 2022): https://ssrn.com/abstract=4072489", 
       x = "Favors vaccine < 1  | Favors placebo > 1",
       y = "Vaccination Type",
       caption = "Data Humanist, CC0 (Public Domain)")



# Overall Mortality -------------------------------------------------------


danish_study %>%
  filter(Concern == "Overall Mortality") %>%
  ggplot( aes(y = Vaccine_Type, x = Rel_Risk,
              xmin = Low_95,
              xmax= High_95,
              color = Vaccine_Type)) +
  geom_point() +   
  geom_errorbar() +
  scale_x_continuous(trans = "log10",
                     breaks = c(0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10) ) +
  guides(color = "none") +
  scale_color_manual(values = c("blue", "green") ) +
  geom_vline(xintercept = 1, lty = 2, color = "red") +
  theme_light()+
  labs(title = "Overall Mortality: mRNA and Adenovirus Covid vaccines",
       subtitle = "Stabell-Benn, et alia. The Lancet (5 Apr 2022): https://ssrn.com/abstract=4072489", 
       x = "Favors vaccine < 1  | Favors placebo > 1",
       y = "Vaccination Type",
       caption = "Data Humanist, CC0 (Public Domain)")




# Covid 19 Deaths ---------------------------------------------------------



danish_study %>%
  filter(Concern == "Covid-19 Deaths") %>%
  ggplot( aes(y = Vaccine_Type, x = Rel_Risk,
              xmin = Low_95,
              xmax= High_95,
              color = Vaccine_Type)) +
  geom_point() +   
  geom_errorbar() +
  scale_x_continuous(trans = "log10",
                     breaks = c(0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10) ) +
  guides(color = "none") +
  scale_color_manual(values = c("blue", "green") ) +
  geom_vline(xintercept = 1, lty = 2, color = "red") +
  theme_light()+
  labs(title = "Covid-19 Deaths: mRNA and Adenovirus Covid vaccines",
       subtitle = "Stabell-Benn, et alia. The Lancet (5 Apr 2022): https://ssrn.com/abstract=4072489", 
       x = "Favors vaccine < 1  | Favors placebo > 1",
       y = "Vaccination Type",
       caption = "Data Humanist, CC0 (Public Domain)")



# Cardiovascular Deaths ---------------------------------------------------



danish_study %>%
  filter(Concern == "Cardiovascular Deaths") %>%
  ggplot( aes(y = Vaccine_Type, x = Rel_Risk,
              xmin = Low_95,
              xmax= High_95,
              color = Vaccine_Type)) +
  geom_point() +   
  geom_errorbar() +
  scale_x_continuous(trans = "log10",
                     breaks = c(0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10) ) +
  guides(color = "none") +
  scale_color_manual(values = c("blue", "green") ) +
  geom_vline(xintercept = 1, lty = 2, color = "red") +
  theme_light()+
  labs(title = "Cardiovascular Deaths: mRNA and Adenovirus Covid vaccines",
       subtitle = "Stabell-Benn, et alia. The Lancet (5 Apr 2022): https://ssrn.com/abstract=4072489", 
       x = "Favors vaccine < 1  | Favors placebo > 1",
       y = "Vaccination Type",
       caption = "Data Humanist, CC0 (Public Domain)")




# Other -- Non-Accidental -- Deaths ---------------------------------------


danish_study %>%
  filter(Concern == "Other (Non-accidental)") %>%
  ggplot( aes(y = Vaccine_Type, x = Rel_Risk,
              xmin = Low_95,
              xmax= High_95,
              color = Vaccine_Type)) +
  geom_point() +   
  geom_errorbar() +
  scale_x_continuous(trans = "log10",
                     breaks = c(0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10) ) +
  guides(color = "none") +
  scale_color_manual(values = c("blue", "green") ) +
  geom_vline(xintercept = 1, lty = 2, color = "red") +
  theme_light()+
  labs(title = "Other (Non-accidental) Deaths: mRNA and Adenovirus Covid vaccines",
       subtitle = "Stabell-Benn, et alia. The Lancet (5 Apr 2022): https://ssrn.com/abstract=4072489", 
       x = "Favors vaccine < 1  | Favors placebo > 1",
       y = "Vaccination Type",
       caption = "Data Humanist, CC0 (Public Domain)")



# Data Table --------------------------------------------------------------

danish_study %>%
  select(-Index) %>%
  mutate(Sig_Result = case_when(Low_95 < 1 & High_95 > 1 ~ "NO",
                                TRUE ~ "YES"))
library(reactable)

danish_study %>%
  select(-Index) %>%
  mutate(Sig_Result = case_when(Low_95 < 1 & High_95 > 1 ~ "NO",
                                TRUE ~ "YES"))%>%
  reactable::reactable(., highlight = TRUE, 
            striped = TRUE,
            theme = reactableTheme(
              stripedColor = "#EDEDED",
              highlightColor = "#FFE4E1") ) 
