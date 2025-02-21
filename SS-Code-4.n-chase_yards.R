library(tidyverse)
library(dplyr)
library(nflfastR)
library(nflplotR)
library(nflreadr)
library(ggplot2)
library(ggrepel)

# load data
nfldata = load_pbp(2024)

# filter data
wrdata6 = nfldata %>%
  filter(week < 19,
         !is.na(yards_gained),
         !is.na(receiver_player_id)) %>%
  group_by(play_id, receiver_player_id, receiver_player_name, posteam) %>%
  summarize(tgts = n(),
            yds = sum(yards_gained),
            .groups = "drop")

# create cumulative rec yds and targets columns
wrdata6$cumyds = ave(wrdata6$yds, wrdata6$receiver_player_id, FUN = cumsum)
wrdata6$cumtgts = ave(wrdata6$tgts, wrdata6$receiver_player_id, FUN = cumsum)

# frame last targets for later geom_point
framewr6 = wrdata6 %>%
  filter(play_id == "5028") %>%
  print(n = Inf)

# plot data
wrplot6 = wrdata6 %>%
  mutate(colorwr6 = ifelse(receiver_player_id == "00-0036900", posteam, "black")) %>%
  ggplot(aes(x = cumtgts, y = cumyds, group = receiver_player_id)) +
  geom_line(aes(color = colorwr6,
                linetype = "solid")) +
  geom_point(data = framewr6,
             aes(color = posteam)) +
  geom_text_repel(data = framewr6,
            aes(label = cumyds,
                color = posteam, fontface = "bold.italic", size = 7)) +
  scale_color_nfl(type = "primary") +
  labs(title = "Targets and Receiving Yards by Player",
       subtitle = "2024 NFL Regular Season",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Targets", y = "Receiving Yards") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#F0F0F0"))

# view plot
wrplot6

# save plot
ggsave("SubSt4.n - chase_yards.png",
       width = 10.5, height = 7, dpi = "retina")
