# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
# Pokemon EDA: The Pokemon
#
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# Package loading ===============================

library(tidyverse)


# Data loading ==================================

Pokemon <- read_csv("Pokemon_list.csv")

# Remove two unavailable Pokemon
Pokemon <- Pokemon %>% 
  filter(Name != "Eternatus-Eternamax") %>% 
  filter(Name != "Floette-Eternal")

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

# Add generation numbers
Pokemon <- Pokemon %>% 
  mutate(
    Generation = 
      case_when(
        ID >= 810 ~ "VIII: Galar and Hisui",
        ID >= 722 ~ "VII: Alola",
        ID >= 650 ~ "VI: Kalos",
        ID >= 494 ~ "V: Unova",
        ID >= 387 ~ "IV: Sinnoh",
        ID >= 252 ~ "III: Hoenn",
        ID >= 152 ~ "II: Johto",
        TRUE ~ "I: Kanto"
      )
  )

# Some additional information
pk_mega <- grep("-Mega", Pokemon$Name, value = TRUE)
pk_primal <- grep("-Primal", Pokemon$Name, value = TRUE)
pk_ultra <- grep("-Ultra", Pokemon$Name, value = TRUE)
pk_alola <- grep("-Alola", Pokemon$Name, value = TRUE)
pk_galar <- grep("-Galar", Pokemon$Name, value = TRUE)
pk_hisui <- grep("-Hisui", Pokemon$Name, value = TRUE)

Pokemon <- Pokemon %>% 
  mutate(
    Regional = 
      case_when(
        Name %in% pk_alola ~ "Alola",
        Name %in% pk_galar ~ "Galar", 
        Name %in% pk_hisui ~ "Hisui", 
        TRUE ~ "None"
      ), 
    Mega = 
      case_when(
        Name %in% pk_mega ~ TRUE, 
        Name %in% pk_primal ~ TRUE, 
        Name %in% pk_ultra ~ TRUE,
        TRUE ~ FALSE
      )
  )
rm(pk_mega, pk_alola, pk_galar, pk_hisui, pk_primal, pk_ultra)

# Monotype boolean
Pokemon <- Pokemon %>% 
  mutate(
    Monotype = 
      case_when(
        is.na(Type2) == TRUE ~ TRUE, 
        TRUE ~ FALSE
      )
  )

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

Pokemon_by_type_no_megas <- list()
for (i in 1:18) {
  x <- types[[i]]
  Pokemon_by_type_no_megas[[i]] <- Pokemon %>%
    filter(Mega == FALSE) %>% 
    filter(Type1 == x | Type2 == x)
}
rm(i, x)

# All stats summaries ===========================

x <- Pokemon %>% 
  summarize(
    HP = round(mean(HP), 1), 
    Atk = round(mean(Atk), 1), 
    Def = round(mean(Def), 1), 
    SpA = round(mean(SpA), 1), 
    SpD = round(mean(SpD), 1), 
    Spe = round(mean(Spe), 1), 
    BST = round(mean(BST), 1)
  ) %>% 
  mutate(Summary = "mean") %>% 
  select(Summary, HP:BST)
y <- Pokemon %>% 
  summarize(
    HP = round(sd(HP), 1), 
    Atk = round(sd(Atk), 1),  
    Def = round(sd(Def), 1),  
    SpA = round(sd(SpA), 1),  
    SpD = round(sd(SpD), 1),  
    Spe = round(sd(Spe), 1),  
    BST = round(sd(BST), 1)
  ) %>% 
  mutate(Summary = "sd") %>% 
  select(Summary, HP:BST)

PKMN_stats = list("HP", "Atk", "Def", "SpA", "SpD", "Spe", "BST")

PKMN_summary_2 <- list()
for (i in 1:7) {
  PKMN_summary_2[[i]] <- as_tibble(as.list(quantile(Pokemon[[i+9]]))) %>% 
    mutate(
      Stat = PKMN_stats[[i]]
    ) %>% 
    select(Stat, `0%`:`100%`)
}
PKMN_summary_2 <- bind_rows(PKMN_summary_2)

PKMN_summary_3 <- data.table::transpose(PKMN_summary_2) %>% 
  add_column(
    Summary = c("remove", "0%", "25%", "50%", "75%", "100%"), 
    .before = "V1"
  ) %>% 
  select(
    Summary, 
    HP = V1, 
    Atk = V2, 
    Def = V3, 
    SpA = V4, 
    SpD = V5, 
    Spe = V6, 
    BST = V7
  ) %>% 
  tail(n = 5)
PKMN_summary <- rbind(x, y, PKMN_summary_3)
rm(x, y, PKMN_summary_2, PKMN_summary_3)


# [Pokemon] Pokemon per type ====================

# Pokemon counts by type
Pokemon_type_counts <- list()
for (i in 1:18) {
  Pokemon_type_counts[[i]] <- Pokemon_by_type[[i]] %>% 
    summarize(Count = n()) %>% 
    mutate(Type = types[[i]]) %>% 
    select(Type, Count)
}
Pokemon_type_counts <- bind_rows(Pokemon_type_counts)

# Stats per type ================================

# Average stats per type
avg_stats_per_type <- list()
for (i in 1:18) {
  avg_stats_per_type[[i]] <- Pokemon_by_type[[i]] %>% 
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
avg_stats_per_type <- bind_rows(avg_stats_per_type)

# Stats by type, plotted
ggplot(
  avg_stats_per_type, 
  aes(reorder(Type, -BST_avg), BST_avg, fill = Type)
) + 
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
    title = "Average BST per Type",
    x = "Type",
    y = "Average BST"
  )

# The best and worst of each type
temp1 <- list()
for (i in 1:18) {
  temp1[[i]] <- Pokemon_by_type[[i]] %>% 
    mutate(
      Stat_range = max(Pokemon_by_type[[i]][["BST"]]) - 
                   min(Pokemon_by_type[[i]][["BST"]])
    ) %>% 
    slice_max(BST, with_ties = TRUE) %>% 
    mutate(
      Typing = types[[i]]
    ) %>% 
    select(
      ID, Name, Typing, Stat_range, Type1:Generation
    )
}
temp1 <- bind_rows(temp1)
temp2 <- list()
for (i in 1:18) {
  temp2[[i]] <- Pokemon_by_type[[i]] %>% 
    mutate(
      Stat_range = max(Pokemon_by_type[[i]][["BST"]]) - 
                   min(Pokemon_by_type[[i]][["BST"]])
    ) %>% 
    slice_min(BST, with_ties = TRUE) %>% 
    mutate(
      Typing = types[[i]]
    ) %>% 
    select(
      ID, Name, Typing, Stat_range, Type1:Generation
    )
}
temp2 <- bind_rows(temp2)
maxmin_by_type <- rbind(temp1, temp2)
rm(temp1, temp2)

# Best/worst, no Megas
temp1 <- list()
for (i in 1:18) {
  temp1[[i]] <- Pokemon_by_type_no_megas[[i]] %>% 
    mutate(
      Stat_range = max(Pokemon_by_type_no_megas[[i]][["BST"]]) - 
        min(Pokemon_by_type_no_megas[[i]][["BST"]])
    ) %>% 
    slice_max(BST, with_ties = TRUE) %>% 
    mutate(
      Typing = types[[i]], 
      max_min = "max"
    ) %>% 
    select(
      ID, Name, Typing, Stat_range, Type1:Generation, max_min
    )
}
temp1 <- bind_rows(temp1)
temp2 <- list()
for (i in 1:18) {
  temp2[[i]] <- Pokemon_by_type_no_megas[[i]] %>% 
    mutate(
      Stat_range = max(Pokemon_by_type_no_megas[[i]][["BST"]]) - 
        min(Pokemon_by_type_no_megas[[i]][["BST"]])
    ) %>% 
    slice_min(BST, with_ties = TRUE) %>% 
    mutate(
      Typing = types[[i]], 
      max_min = "min"
    ) %>% 
    select(
      ID, Name, Typing, Stat_range, Type1:Generation, max_min
    )
}
temp2 <- bind_rows(temp2)
maxmin_by_type_no_megas <- rbind(temp1, temp2)
rm(temp1, temp2)



# Max/min by type, ordered by max BST descending
maxmin_by_type_no_megas %>% 
  arrange(max_min, desc(BST)) %>% 
  mutate(
    Typing = factor(Typing, levels = unique(Typing))
  ) %>% 
  ggplot() + 
  aes(
    Typing, BST, color = max_min, fill = Typing
  ) + 
  geom_col(position = "dodge") + 
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
  scale_color_manual(
    values = c(
      "max" = "#ff7f7f", 
      "min" = "#7f7fff"
    )
  ) + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "none"
  ) + 
  labs(
    title = "Max and Min BST per Type",
    x = "Type",
    y = "Max/Min BST"
  )

# BST ranges
maxmin_by_type_no_megas %>% 
  filter(max_min == "min") %>% 
  ggplot() + 
  aes(reorder(Typing, -Stat_range), Stat_range, fill = Typing) + 
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
    title = "BST Range per Type",
    x = "Type",
    y = "BST Range"
  )


# Bug has the lowest average HP and SpA ---------
Bug <- Pokemon %>% 
  filter(Type1 == "Bug" | Type2 == "Bug")

ggplot(Bug, aes(HP)) + geom_boxplot()
# >> No outliers

ggplot(Bug, aes(SpA)) + geom_boxplot()

Bug %>% 
  arrange(desc(SpA)) %>% 
  slice_head(n = 6)
# >> Vikavolt, Pheromosa, Volcarona, Frosmoth, Genesect, Yanmega

# Fairy has the lowest average Atk --------------
Fairy <- Pokemon %>% 
  filter(Type1 == "Fairy" | Type2 == "Fairy")

# Outliers?
ggplot(Fairy, aes(Atk)) + geom_boxplot()

# The outliters
Fairy %>% 
  arrange(desc(Atk)) %>% 
  slice_head(n = 2)
# >> Mega Diancie and Zacian

# Normal has the lowest average Def and SpD -----
Normal <- Pokemon %>% 
  filter(Type1 == "Normal" | Type2 == "Normal")

ggplot(Normal, aes(Def)) + geom_boxplot() 

Normal %>% 
  arrange(desc(Def)) %>% 
  slice_head()
# >> Mega Audino

ggplot(Normal, aes(SpD)) + geom_boxplot()

Normal %>% 
  arrange(desc(SpD)) %>% 
  slice_head()
# >> Blissey

# Rock has the lowest average Spe ---------------

Rock <- Pokemon %>% 
  filter(Type1 == "Rock" | Type2 == "Rock")

ggplot(Rock, aes(Spe)) + geom_boxplot()

Rock %>% 
  arrange(desc(Spe)) %>% 
  slice_head(n = 2)
# >> Aerodactyl and Mega Aerodactyl

# Counts per generation =========================

Pokemon %>% 
  distinct(ID, .keep_all = TRUE) %>% 
  group_by(Generation) %>% 
  summarize(count = n())


# Average stats per generation (with Megas) =====

avg_stats_per_gen <- Pokemon %>% 
  group_by(Generation) %>% 
  summarize(
    HP_avg = mean(HP),
    Atk_avg = mean(Atk),
    Def_avg = mean(Def),
    SpA_avg = mean(SpA),
    SpD_avg = mean(SpD),
    Spe_avg = mean(Spe),
    BST_avg = mean(BST)
  )

ggplot(Pokemon, aes(Generation, BST)) + 
  geom_boxplot() + 
  labs(
    x = "Generation", 
    title = "BST by Generation"
  )

ggplot(Pokemon, aes(BST)) + 
  geom_histogram(binwidth = 25) + 
  facet_wrap(~ Generation, nrow = 2) + 
  labs(
    title = "BST per Generation", 
    caption = "Bin size: 25"
  )

# Average stats per generation (no Megas) =======

avg_stat_per_gen_no_megas <- Pokemon %>%
  filter(Mega == FALSE) %>% 
  group_by(Generation) %>% 
  summarize(
    HP_avg = mean(HP),
    Atk_avg = mean(Atk),
    Def_avg = mean(Def),
    SpA_avg = mean(SpA),
    SpD_avg = mean(SpD),
    Spe_avg = mean(Spe),
    BST_avg = mean(BST)
  )
# >> Power creep is fairly evident via BST average

ggplot(filter(Pokemon, Mega == FALSE), aes(x = Generation, BST)) + 
  geom_boxplot() + 
  labs(
    x = "Generation", 
    title = "BST by Generation, Megas (and equivalent) Removed"
  )

ggplot(filter(Pokemon, Mega == FALSE), aes(BST)) + 
  geom_histogram(binwidth = 25) + 
  facet_wrap(~ Generation, nrow = 2) + 
  labs(
    title = "BST per Generation", 
    caption = "Bin size: 25"
  )

# Powercreep with and without Megas
colors <- c("With Megas" = "#c00000", "Without Megas" = "#0000c0")
ggplot(Pokemon, aes(Generation, BST, color = "With Megas")) + 
  geom_boxplot(
    width = 0.25, 
    position = position_nudge(x = -0.125)
  ) + 
  geom_boxplot(
    data = filter(Pokemon, Mega == FALSE), 
    aes(Generation, BST, color = "Without Megas"), 
    width = 0.25, 
    position = position_nudge(x = 0.125)
  ) + 
  labs(
    x = "Generation", 
    title = "BST by Generation", 
    color = "Legend"
  ) + 
  scale_color_manual(values = colors)


# The most wildly skewed stats ==================

# Stat averages and SDs
PKMN_SD <- Pokemon %>% 
  rowwise() %>% 
  mutate(
    AVG = BST / 6,
    SD = sd(c(HP, Atk, Def, SpA, SpD, Spe))
  )

# The top 25 wildest ones
PKMN_skewed <- PKMN_SD %>% 
  arrange(desc(SD)) %>% 
  head(n = 25)

# Monotype ======================================

monotype1 <- list()
for (i in 1:18) {
  monotype1[[i]] <- Pokemon_by_type[[i]] %>% 
    group_by(Monotype) %>% 
    summarize(
      HP_avg = mean(HP),
      Atk_avg = mean(Atk),
      Def_avg = mean(Def),
      SpA_avg = mean(SpA),
      SpD_avg = mean(SpD),
      Spe_avg = mean(Spe),
      BST_avg = mean(BST)
    ) %>% 
    mutate(
      Type = types[[i]]
    ) %>% 
    select(Type, Monotype, HP_avg:BST_avg)
}
monotype1 <- bind_rows(monotype1)

ggplot(
  monotype1, 
  aes(Type, BST_avg, color = Monotype, fill = Type)
) + 
  geom_col(position = "dodge", size = 0.75, width = 0.75) + 
  scale_fill_manual(values = type_colors) + 
  guides(
    fill = "none"
  ) + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) + 
  labs(
    title = "Average BST per Type",
    x = "Type",
    y = "Average BST"
  )


