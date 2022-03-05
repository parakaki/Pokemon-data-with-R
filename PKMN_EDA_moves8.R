# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
# Pokemon EDA: The Moves
#
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Package loading ===============================

library(tidyverse)


# Data loading ==================================

moves_all <- read_csv("Pokemon_moves.csv")


# Some other preliminary things =================

`%!in%` = Negate(`%in%`)

# The types list
types <- list(
  "Bug", "Dark", "Dragon", "Electric", "Fairy", "Fighting",
  "Fire", "Flying", "Ghost", "Grass", "Ground", "Ice", 
  "Normal", "Poison", "Psychic", "Rock", "Steel", "Water"
)

type_colors <- c(
  "Bug" = "#8cc639", 
  "Dark" = "#5b5365", 
  "Dragon" = "#3864c1", 
  "Electric" = "#edd847", 
  "Fairy" = "#ec88e4", 
  "Fighting" = "#ca406b", 
  "Fire" = "#f8a159", 
  "Flying" = "#96a5dd", 
  "Ghost" = "#5c63ac", 
  "Grass" = "#61c05e", 
  "Ground" = "#d37b48", 
  "Ice" = "#79cec0", 
  "Normal" = "#9299a2", 
  "Poison" = "#ad62c6", 
  "Psychic" = "#f5727a", 
  "Rock" = "#c2b98e", 
  "Steel" = "#618ca1", 
  "Water" = "#618ad4"
)


# Some general filtering ========================

# Filtering (improved)
moves_all <- moves_all %>% 
  mutate(
    Generation = 
      case_when(
        Gen == 8 ~ "VIII: Galar and Hisui", 
        Gen == 7 ~ "VII: Alola", 
        Gen == 6 ~ "VI: Kalos", 
        Gen == 5 ~ "V: Unova", 
        Gen == 4 ~ "IV: Sinnoh", 
        Gen == 3 ~ "III: Hoenn", 
        Gen == 2 ~ "II: Johto", 
        TRUE ~ "I: Kanto"
      )
  )

maxes <- grep("^Max", moves_all$Name, value = TRUE)
moves_all <- moves_all %>% 
  mutate(
    Gimmick = 
      case_when(
        PP == 1 & Name != "Struggle" & Name != "Sketch" ~ "Z-move",
        Name %in% maxes ~ "Max move",
        Power == 5000 ~ "OHKO", 
        TRUE ~ "None"
      )
  )
moves <- moves_all %>% 
  filter(Gimmick == "None")
rm(maxes)

# Damaging moves only (without OHKO, Max)
moves_damage <- moves %>% 
  filter(Category != "Status")


# Summaries per type ============================

# Basic summaries

moves_per_type <- moves %>% 
  group_by(Type) %>% 
  summarize(count = n())

moves_summaries <- moves %>% 
  group_by(Type) %>% 
  summarize(
    count = n(),
    pwr_avg = mean(Power),
    acc_avg = mean(Accuracy),
    pp_avg = mean(PP)
  )

moves_dmg_summaries <- moves_damage %>% 
  group_by(Type) %>% 
  summarize(
    count = n(),
    pwr_avg = mean(Power),
    acc_avg = mean(Accuracy),
    pp_avg = mean(PP)
  )

# Contact moves per type
contact_moves_per_type <- moves %>% 
  filter(Contact == TRUE) %>% 
  group_by(Type) %>% 
  summarize(count_contact = n())
contact_moves_per_type <- inner_join(
  moves_per_type,
  contact_moves_per_type, 
  by = "Type",
) %>% 
  mutate(ratio = round(count_contact / count, 3)) %>% 
  select(Type, count_contact, count, ratio) %>% 
  arrange(desc(ratio))

# Some plots
# > scale_fill_manual can color each bar

# All moves by type
ggplot(moves_summaries, aes(reorder(Type, -count), count, fill = Type)) + 
  geom_bar(stat = "identity", size = 0.5) + 
  scale_fill_manual(
    values = type_colors
  ) + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "none"
  ) + 
  labs(
    title = "Moves per Type",
    x = "Type",
    y = "Count"
  )

# Damaging moves by type
ggplot(moves_dmg_summaries, aes(reorder(Type, -count), count, fill = Type)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(
    values = type_colors
  ) + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "none"
  ) + 
  labs(
    title = "Damaging Moves per Type",
    x = "Type",
    y = "Count"
  )

# A survey of damaging moves per type
ggplot(moves_damage, aes(reorder(Type, -Power, median), Power, fill = Type)) + 
  geom_boxplot() + 
  scale_fill_manual(
    values = type_colors
  ) + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "none"
  ) + 
  labs(
    title = "Damaging Move Power per Type, Medians",
    x = "Type",
    y = "Count"
  )


# Summaries by generation =======================

# All moves
moves_summary_gen_all <- moves_all %>% 
  group_by(Gen) %>% 
  summarize(
    Count = n(),
    pwr_avg = mean(Power),
    acc_avg = mean(Accuracy),
    pp_avg = mean(PP)
  )

# Excludes OHKO, Z, Max
moves_summary_gen <- moves %>% 
  group_by(Gen) %>% 
  summarize(
    Count = n(),
    pwr_avg = mean(Power),
    acc_avg = mean(Accuracy),
    pp_avg = mean(PP)
  )

# Gen and category
moves_gen_cat <- moves_all %>% 
  group_by(Gen, Category) %>% 
  summarize(
    Count = n()
  )

# Plot of moves (no OHKO, etc.) per gen
ggplot(moves, aes(Gen, fill = Category)) + 
  geom_bar(position = "dodge") + 
  scale_fill_manual(
    "Category", 
    values = c(
      "Physical" = "#C42617", 
      "Special" = "#52566F", 
      "Status" = "#8C888C"
    )
  )

# Plot of all moves per gen
ggplot(moves_all, aes(Gen, fill = Category)) + 
  geom_bar(position = "dodge") + 
  scale_fill_manual(
    "Category", 
    values = c(
      "Physical" = "#C42617", 
      "Special" = "#52566F", 
      "Status" = "#8C888C", 
      "???" = "#6BA190"
    )
  )

# Category and types
moves %>% 
  group_by(Type, Category) %>%  
  ggplot() + 
  aes(x = Type, color = Type, fill = Category) + 
  geom_bar(size = 1, width = 0.75, position = "dodge") + 
  scale_fill_manual(
    values = c(
      "Physical" = "#C42617", 
      "Special" = "#52566F", 
      "Status" = "#8C888C"
    )
  ) + 
  scale_color_manual(
    values = type_colors
  ) + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) + 
  guides(color = "none") + 
  labs(
    title = "Category Counts per Type",
    x = "Type",
    y = "Count",
    fill = "Category"
  )


# Category and type proportions
ggplot(moves, aes(Type, color = Type, fill = Category)) + 
  geom_bar(width = 0.5, position = "stack") + 
  scale_fill_manual(
    values = c(
      "Physical" = "#C42617", 
      "Special" = "#52566F", 
      "Status" = "#8C888C"
    )
  ) +
  scale_color_manual(values = type_colors) + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) + 
  guides(color = "none") + 
  labs(
    title = "Category Proportions per Type",
    x = "Type",
    y = "Proportion",
    fill = "Category"
  )

# Accuracy vs. Power
colors2 <- c(
  "All moves" = "#433fe7", 
  "Damaging moves" = "#fb7639"
)
ggplot(
  moves_damage, 
  aes(Power, Accuracy, color = "#c00000")
) + 
  geom_jitter(alpha = 0.2) + 
  geom_smooth() + 
  guides(color = "none") + 
  labs(
    title = "Accuracy vs. Power"
  )

ggplot(
  moves, 
  aes(Power, Accuracy, color = "All moves")
) + 
  geom_jitter(alpha = 0.2) + 
  geom_smooth() +
  geom_jitter(
    data = moves_damage, 
    aes(Power, Accuracy, color = "Damaging moves"), 
    alpha = 0.2
  ) + 
  geom_smooth(
    data = moves_damage,
    aes(Power, Accuracy, color = "Damaging moves")
  ) +
  guides(alpha = "none") + 
  labs(
    title = "Accuracy vs. Power", 
    color = "Legend"
  ) + 
  scale_color_manual(values = colors2)

# Power vs. PP
ggplot(
  moves, 
  aes(PP, Power, color = "All moves")
) + 
  geom_jitter(alpha = 0.2) + 
  geom_smooth() +
  geom_jitter(
    data = filter(moves, Power > 0), 
    aes(PP, Power, color = "Damaging moves"), 
    alpha = 0.2
  ) + 
  geom_smooth(
    data = filter(moves, Power > 0),
    aes(PP, Power, color = "Damaging moves")
  ) +
  guides(alpha = "none") + 
  labs(
    x = "PP", 
    y = "Power", 
    title = "Power vs. PP", 
    color = "Legend"
  ) + 
  scale_color_manual(values = colors2)

ggplot(
  moves, 
  aes(PP, Power, color = "All moves")
) + 
  geom_smooth() + 
  geom_smooth(
    data = filter(moves, Power > 0), 
    aes(PP, Power, color = "Damaging moves")
  ) + 
  labs(
    x = "PP", 
    y = "Power", 
    title = "Power vs. PP", 
    color = "Legend"
  ) + 
  scale_color_manual(values = colors2)

# Accuracy vs. PP
ggplot(
  moves, 
  aes(PP, Accuracy, color = "All moves")
) + 
  geom_jitter(alpha = 0.2) + 
  geom_smooth() +
  geom_jitter(
    data = moves_damage, 
    aes(PP, Accuracy, color = "Damaging moves"), 
    alpha = 0.2
  ) + 
  geom_smooth(
    data = moves_damage,
    aes(PP, Accuracy, color = "Damaging moves")
  ) +
  guides(alpha = "none") + 
  labs( 
    title = "Accuracy vs. PP", 
    color = "Legend"
  ) + 
  scale_color_manual(values = colors2)

ggplot(
  moves, 
  aes(PP, Accuracy, color = "All moves")
) + 
  geom_smooth() + 
  geom_smooth(
    data = moves_damage, 
    aes(PP, Accuracy, color = "Damaging moves")
  ) + 
  labs( 
    title = "Accuracy vs. PP, no scatterplot", 
    color = "Legend"
  ) + 
  scale_color_manual(values = colors2)



