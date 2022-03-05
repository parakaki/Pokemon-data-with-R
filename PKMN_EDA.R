# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# A Beginner's Data Exploration
#   of Pokemon
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# =====================================
# Package loading
# =====================================

library(tidyverse)

# =====================================
# Some other preliminary things
# =====================================

`%!in%` = Negate(`%in%`)

# =====================================
# Data loading
# =====================================

Pokemon <- read_csv("E:/Pokémon/Pokemon EDA/Pokemon_list.csv")
moves_all <- read_csv("E:/Pokémon/Pokemon EDA/Pokemon_moves.csv")

# =====================================
# [Pokemon] Some general filtering
# =====================================

# The types list
types <- list(
  "Bug", "Dark", "Dragon", "Electric", "Fairy", "Fighting",
  "Fire", "Flying", "Ghost", "Grass", "Ground", "Ice", 
  "Normal", "Poison", "Psychic", "Rock", "Steel", "Water"
)

# The famed Mega Evolutions
pk_mega <- grep("-Mega", Pokemon$Name, value = TRUE)
Mega_Evolutions <- Pokemon %>% 
  filter(Name %in% pk_mega)

# Alolan varieties
pk_alola <- grep("-Alola", Pokemon$Name, value = TRUE)
Alolan_forms <- Pokemon %>% 
  filter(Name %in% pk_alola)

# Galarian varieties
pk_galar <- grep("-Galar", Pokemon$Name, value = TRUE)
Galarian_forms <- Pokemon %>% 
  filter(Name %in% pk_galar)

# Hisuian varieties
pk_hisui <- grep("-Hisui", Pokemon$Name, value = TRUE)
Hisuian_forms <- Pokemon %>% 
  filter(Name %in% pk_hisui)

# Pseudolegendaries
pseudo_legendaries <- Pokemon %>% 
  filter(
    Name == "Dragonite" | 
    Name == "Tyranitar" | 
    Name == "Salamence" |
    Name == "Metagross" | 
    Name == "Hydreigon" | 
    Name == "Goodra" | 
    Name == "Kommo-o" | 
    Name == "Dragapult" | 
    Name == "Goodra-Hisui"
  )

# Pokemon by type; indexed alphabetically
Pokemon_by_type <- list()
for (i in 1:18) {
  x <- types[[i]]
  Pokemon_by_type[[i]] <- Pokemon %>% 
    filter(Type1 == x | Type2 == x)
}
rm(i, x)


rm(pk_mega, pk_alola, pk_galar, pk_hisui)

# =====================================
# [Moves] Some general filtering
# =====================================

# Z-moves
Z_moves <- moves_all %>% 
  filter(PP == 1) %>% 
  filter(Name != "Struggle" & Name != "Sketch")

# Max moves
maxes <- grep("^Max ", moves_all$Name, value = TRUE)
max_moves <- moves_all %>% 
  filter(Name %in% maxes)
moves_non_max <- moves_all %>% 
  filter(Name %!in% maxes)
rm(maxes)

# This removes OHKO moves and Z-moves
moves <- moves_non_max %>% 
  filter(Power < 1000) %>% 
  filter(PP > 1)

# Damaging moves only (without OHKO, Max)
moves_damage <- moves %>% 
  filter(Power > 0)


# =====================================
# [Pokemon] Pokemon per type
# =====================================

# Pokemon counts by type
templist <- list()
for (i in 1:18) {
  templist[[i]] <- Pokemon_by_type[[i]] %>% 
    summarize(Count = n()) %>% 
    mutate(Type = types[[i]]) %>% 
    select(Type, Count)
}
Pokemon_type_counts <- bind_rows(templist)
rm(templist)


# =====================================
# [Pokemon] Summaries
# =====================================

# Average stats per type
templist <- list()
for (i in 1:18) {
  templist[[i]] <- Pokemon_by_type[[i]] %>% 
    summarize(
      HP_avg = mean(HP),
      Atk_avg = mean(Atk),
      Def_avg = mean(Def),
      SpA_avg = mean(SpA),
      SpD_avg = mean(SpD),
      Spe_avg = mean(Spe),
      BST_avg = mean(BST)
    ) %>% 
    mutate(Type = types[[i]]) %>% 
    select(Type, HP_avg:BST_avg)
}
avg_stats_per_type <- bind_rows(templist)
rm(templist)





# =====================================
# [moves] Summaries by type
# =====================================

# Basic summary stats

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

# Some plots
# > scale_fill_manual can color each bar

# All moves by type
ggplot(moves_summaries, aes(reorder(Type, -count), count, fill = Type)) + 
  geom_bar(stat = "identity", size = 0.5) + 
  scale_fill_manual(
    values = c(
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
    values = c(
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
ggplot(moves_damage, aes(reorder(Type, Power, median), Power, fill = Type)) + 
  geom_boxplot() + 
  scale_fill_manual(
    values = c(
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





# =====================================
# [moves] Summaries by generation
# =====================================

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



