library(tidyverse)
library(dplyr)
library(knitr)
library(kableExtra)
library(ggrepel)

housing <- read_csv("./HODP/housingday/housing.csv")

cleanhousing <- housing %>%
  slice(3:205) %>%
  select(Q2:Q7, Q15) %>%
  rename("bgsize" = "Q2",
         "Adams" = "Q3_1",
         "Cabot" = "Q3_2",
         "Currier" = "Q3_3",
         "Dunster" = "Q3_4",
         "Eliot" = "Q3_5",
         "Kirkland" = "Q3_6",
         "Leverett" = "Q3_7",
         "Lowell" = "Q3_8",
         "Mather" = "Q3_9",
         "Pfoho" = "Q3_10",
         "Quincy" = "Q3_11",
         "Winthrop" = "Q3_12") %>%
  mutate_if(is.character, as.numeric)

popularity <- cleanhousing %>%
  summarise_all(mean) %>%
  select(Adams:Winthrop) %>%
  gather(House, Popularity) %>%
  arrange(Popularity) %>%
  mutate_at("Popularity", round, 2)

popularity %>%
  ggplot(aes(x = reorder(House, Popularity), y = Popularity)) +
  geom_bar(stat = "identity", fill = "#FF6666") + 
  ylab("Average Rank") +
  xlab("House") +
  geom_text(aes(label=Popularity), position=position_dodge(width=0.9), vjust=-0.25, size = 3)

total <- cleanhousing %>%
  select(Adams:Winthrop) %>%
  gather() %>%
  group_by(key) %>%
  count(value) %>%
  spread(value, n) %>%
  replace_na(list(x = 0, y = 0)) %>%
  replace(is.na(.), 0) %>%
  rename("House" = "key")

total %>%
  kable(align = "c") %>%
  kable_styling(bootstrap_options = "striped", full_width = T)

summary <- cleanhousing %>%
  select(Adams:Winthrop)

race <- housing %>%
  slice(3:205) %>%
  select(Q1:Q15) %>%
  rename("bgsize" = "Q2",
         "Adams" = "Q3_1",
         "Cabot" = "Q3_2",
         "Currier" = "Q3_3",
         "Dunster" = "Q3_4",
         "Eliot" = "Q3_5",
         "Kirkland" = "Q3_6",
         "Leverett" = "Q3_7",
         "Lowell" = "Q3_8",
         "Mather" = "Q3_9",
         "Pfoho" = "Q3_10",
         "Quincy" = "Q3_11",
         "Winthrop" = "Q3_12")

bg <- housing %>%
  slice(3:205) %>%
  select(Q1:Q2) %>%
  rename("bgsize" = "Q2") %>%
  count(bgsize) %>%
  mutate(percent = (n/sum(n) * 100)) %>%
  mutate_at("percent", round, 1) %>%
  mutate(percent = paste(percent, "%", sep = ""))

# bg %>%
 # ggplot(aes(x = "", y = n, fill = factor(bgsize))) +
 # geom_bar(width = 1, stat = "identity", color = "white") +
 # geom_label_repel(aes(label = percent), size=5, show.legend = F, nudge_x = 1) +
 # guides(fill = guide_legend(title = "Group")) +
 # coord_polar("y", start = 0) +
 # theme_void()

vals <- bg$n
val_names <- sprintf("%s (%s)", c("1", "2", "3", "4", "5", "6", "7", "8"), bg$percent)
names(vals) <- val_names

waffle::waffle(vals, rows = 9) +
  labs(title = "Blocking Group Size") +
  theme(plot.title = element_text(size = 12))

bg2 <- housing %>%
  slice(3:205) %>%
  select(Q1:Q3_12) %>%
  rename("bgsize" = "Q2",
         "Adams" = "Q3_1",
         "Cabot" = "Q3_2",
         "Currier" = "Q3_3",
         "Dunster" = "Q3_4",
         "Eliot" = "Q3_5",
         "Kirkland" = "Q3_6",
         "Leverett" = "Q3_7",
         "Lowell" = "Q3_8",
         "Mather" = "Q3_9",
         "Pfoho" = "Q3_10",
         "Quincy" = "Q3_11",
         "Winthrop" = "Q3_12") %>%
  filter(bgsize %in% c("1", "2", "3")) %>%
  mutate_if(is.character, as.numeric) %>%
  summarise_all(mean) %>%
  select(Adams:Winthrop) %>%
  gather(House, Popularity) %>%
  arrange(Popularity) %>%
  mutate_at("Popularity", round, 2)

bg3 <- housing %>%
  slice(3:205) %>%
  select(Q1:Q3_12) %>%
  rename("bgsize" = "Q2",
         "Adams" = "Q3_1",
         "Cabot" = "Q3_2",
         "Currier" = "Q3_3",
         "Dunster" = "Q3_4",
         "Eliot" = "Q3_5",
         "Kirkland" = "Q3_6",
         "Leverett" = "Q3_7",
         "Lowell" = "Q3_8",
         "Mather" = "Q3_9",
         "Pfoho" = "Q3_10",
         "Quincy" = "Q3_11",
         "Winthrop" = "Q3_12") %>%
  filter(bgsize %in% c("7", "8")) %>%
  mutate_if(is.character, as.numeric) %>%
  summarise_all(mean) %>%
  select(Adams:Winthrop) %>%
  gather(House, Popularity) %>%
  arrange(Popularity) %>%
  mutate_at("Popularity", round, 2)

bg_compare <- bg3 %>%
  bind_cols(bg2)  %>%
  select(House, Popularity, Popularity1) %>%
  gather(Popularity, Popularity1, Popularity:Popularity1)

bg_compare %>%
  ggplot(aes(x = House, y = Popularity1, fill = Popularity)) +
  geom_bar(stat = "identity", position = "dodge") + 
  ylab("Average Rank") +
  xlab("House") +
  scale_fill_discrete(name = "Blocking Group Size", labels = c('Blocking Groups of 7-8', 
                                                               'Blocking Groups of 1-3')) +
  geom_text(aes(label=Popularity1), position=position_dodge(width=0.9), vjust=-0.25, size = 1.5)

white <- housing %>%
  slice(3:205) %>%
  select(Q1:Q3_12, Q5) %>%
  rename("bgsize" = "Q2",
         "Adams" = "Q3_1",
         "Cabot" = "Q3_2",
         "Currier" = "Q3_3",
         "Dunster" = "Q3_4",
         "Eliot" = "Q3_5",
         "Kirkland" = "Q3_6",
         "Leverett" = "Q3_7",
         "Lowell" = "Q3_8",
         "Mather" = "Q3_9",
         "Pfoho" = "Q3_10",
         "Quincy" = "Q3_11",
         "Winthrop" = "Q3_12",
         "race" = "Q5") %>%
  filter(race == "White") %>%
  mutate_if(is.character, as.numeric) %>%
  summarise_all(mean) %>%
  select(Adams:Winthrop) %>%
  gather(House, Popularity) %>%
  arrange(Popularity) %>%
  mutate_at("Popularity", round, 2)

asian <- housing %>%
  slice(3:205) %>%
  select(Q1:Q3_12, Q5) %>%
  rename("bgsize" = "Q2",
         "Adams" = "Q3_1",
         "Cabot" = "Q3_2",
         "Currier" = "Q3_3",
         "Dunster" = "Q3_4",
         "Eliot" = "Q3_5",
         "Kirkland" = "Q3_6",
         "Leverett" = "Q3_7",
         "Lowell" = "Q3_8",
         "Mather" = "Q3_9",
         "Pfoho" = "Q3_10",
         "Quincy" = "Q3_11",
         "Winthrop" = "Q3_12",
         "race" = "Q5") %>%
  filter(race == "Asian") %>%
  mutate_if(is.character, as.numeric) %>%
  summarise_all(mean) %>%
  select(Adams:Winthrop) %>%
  gather(House, Popularity) %>%
  arrange(Popularity) %>%
  mutate_at("Popularity", round, 2)

black <- housing %>%
  slice(3:205) %>%
  select(Q1:Q3_12, Q5) %>%
  rename("bgsize" = "Q2",
         "Adams" = "Q3_1",
         "Cabot" = "Q3_2",
         "Currier" = "Q3_3",
         "Dunster" = "Q3_4",
         "Eliot" = "Q3_5",
         "Kirkland" = "Q3_6",
         "Leverett" = "Q3_7",
         "Lowell" = "Q3_8",
         "Mather" = "Q3_9",
         "Pfoho" = "Q3_10",
         "Quincy" = "Q3_11",
         "Winthrop" = "Q3_12",
         "race" = "Q5") %>%
  filter(race == "Black or African American") %>%
  mutate_if(is.character, as.numeric) %>%
  summarise_all(mean) %>%
  select(Adams:Winthrop) %>%
  gather(House, Popularity) %>%
  arrange(Popularity) %>%
  mutate_at("Popularity", round, 2)

other <- housing %>%
  slice(3:205) %>%
  select(Q1:Q3_12, Q5) %>%
  rename("bgsize" = "Q2",
         "Adams" = "Q3_1",
         "Cabot" = "Q3_2",
         "Currier" = "Q3_3",
         "Dunster" = "Q3_4",
         "Eliot" = "Q3_5",
         "Kirkland" = "Q3_6",
         "Leverett" = "Q3_7",
         "Lowell" = "Q3_8",
         "Mather" = "Q3_9",
         "Pfoho" = "Q3_10",
         "Quincy" = "Q3_11",
         "Winthrop" = "Q3_12",
         "race" = "Q5") %>%
  filter(race == "Other") %>%
  mutate_if(is.character, as.numeric) %>%
  summarise_all(mean) %>%
  select(Adams:Winthrop) %>%
  gather(House, Popularity) %>%
  arrange(Popularity) %>%
  mutate_at("Popularity", round, 2)

race_compare <- white %>%
  bind_cols(asian, black, other)  %>%
  select(House, Popularity, Popularity1, Popularity2, Popularity3) %>%
  gather(Popularity, Popularity1, Popularity:Popularity3)

race_compare %>%
  ggplot(aes(x = House, y = Popularity1, fill = Popularity)) +
  geom_bar(stat = "identity", position = "dodge") + 
  ylab("Average Rank") +
  xlab("House") +
  scale_fill_discrete(name = "Race/Ethnicity", labels = c('White',
                                                          'Asian',
                                                          'Black',
                                                          'Other')) +
  geom_text(aes(label=Popularity1), position=position_dodge(width=0.9), vjust=-0.25, size = 1.5)

athlete <- housing %>%
  slice(3:205) %>%
  select(Q1:Q3_12, Q15) %>%
  rename("bgsize" = "Q2",
         "Adams" = "Q3_1",
         "Cabot" = "Q3_2",
         "Currier" = "Q3_3",
         "Dunster" = "Q3_4",
         "Eliot" = "Q3_5",
         "Kirkland" = "Q3_6",
         "Leverett" = "Q3_7",
         "Lowell" = "Q3_8",
         "Mather" = "Q3_9",
         "Pfoho" = "Q3_10",
         "Quincy" = "Q3_11",
         "Winthrop" = "Q3_12",
         "athlete" = "Q15") %>%
  filter(athlete == "Yes") %>%
  mutate_if(is.character, as.numeric) %>%
  summarise_all(mean) %>%
  select(Adams:Winthrop) %>%
  gather(House, Popularity) %>%
  arrange(Popularity) %>%
  mutate_at("Popularity", round, 2)

nonathlete <- housing %>%
  slice(3:205) %>%
  select(Q1:Q3_12, Q15) %>%
  rename("bgsize" = "Q2",
         "Adams" = "Q3_1",
         "Cabot" = "Q3_2",
         "Currier" = "Q3_3",
         "Dunster" = "Q3_4",
         "Eliot" = "Q3_5",
         "Kirkland" = "Q3_6",
         "Leverett" = "Q3_7",
         "Lowell" = "Q3_8",
         "Mather" = "Q3_9",
         "Pfoho" = "Q3_10",
         "Quincy" = "Q3_11",
         "Winthrop" = "Q3_12",
         "athlete" = "Q15") %>%
  filter(athlete == "No") %>%
  mutate_if(is.character, as.numeric) %>%
  summarise_all(mean) %>%
  select(Adams:Winthrop) %>%
  gather(House, Popularity) %>%
  arrange(Popularity) %>%
  mutate_at("Popularity", round, 2)

athlete_compare <- athlete %>%
  bind_cols(nonathlete)  %>%
  select(House, Popularity, Popularity1) %>%
  gather(Popularity, Popularity1, Popularity:Popularity1)

athlete_compare$House <- factor(athlete_compare$House, levels = c("Adams", "Dunster", "Eliot", "Kirkland",
                                                                  "Leverett", "Lowell", "Mather", "Quincy",
                                                                  "Winthrop", "Cabot", "Currier", "Pfoho"))

athlete_compare %>%
  ggplot(aes(x = House, y = Popularity1, fill = Popularity)) +
  geom_bar(stat = "identity", position = "dodge") + 
  ylab("Average Rank") +
  xlab("House") +
  scale_fill_discrete(name = "Varsity Athlete", labels = c('Yes',
                                                          'No')) +
  geom_text(aes(label=Popularity1), position=position_dodge(width=0.9), 
            vjust=-0.25, size = 1.75) 

variance <- cleanhousing %>%
  summarise_all(var) %>%
  select(Adams:Winthrop) %>%
  gather(House, Variance) %>%
  mutate_at("Variance", round, 2)

mean <- cleanhousing %>%
  summarise_all(mean) %>%
  select(Adams:Winthrop) %>%
  gather(House, Mean) %>%
  mutate_at("Mean", round, 2)

stdev <- cleanhousing %>%
  summarise_all(sd) %>%
  select(Adams:Winthrop) %>%
  gather(House, StandardDeviation) %>%
  mutate_at("StandardDeviation", round, 2)

sumstat <- mean %>%
  bind_cols(variance, stdev) %>%
  select(House, Mean, Variance, StandardDeviation) %>%
  rename("Standard Deviation" = "StandardDeviation")

sumstat %>%
  kable(align = "c") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)


