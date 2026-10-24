# ==============================================================
# Week 3 Task - Data Visualization and Reporting for Digital Services
# Packages : ggplot2, dplyr, readr, scales, stringr
# Input    : outputs of week2_transform.R (data/*.csv)
# Output   : figures/w3_*.png
# ==============================================================
library(readr)
library(dplyr)
library(ggplot2)
library(scales)

dir.create("figures", showWarnings = FALSE)

# ---------- 1. LOAD TRANSFORMED DATA ----------
egdi    <- read_csv("data/egdi_2024_transformed.csv", show_col_types = FALSE) %>%
  mutate(group = factor(group, levels = c("Low", "Middle", "High", "Very High")))
reg_sum <- read_csv("data/region_summary.csv",    show_col_types = FALSE)
reg_grp <- read_csv("data/region_group_long.csv", show_col_types = FALSE) %>%
  mutate(group = factor(group, levels = c("Low", "Middle", "High", "Very High")))

global_mean <- mean(egdi$egdi)
region_order <- reg_sum$region                 # already sorted by mean EGDI (high -> low)

# ---------- 2. SHARED DESIGN CHOICES ----------
# Colour-blind-safe palette, one colour per EGDI group, reused in every chart
pal_group <- c("Low" = "#D55E00", "Middle" = "#E69F00", "High" = "#009E73", "Very High" = "#0072B2")
india_col <- "#CC0066"

theme_egov <- function() {
  theme_minimal(base_size = 12) +
    theme(plot.title    = element_text(face = "bold", size = 14),
          plot.title.position = "plot", plot.caption.position = "plot",
          plot.subtitle = element_text(colour = "grey35"),
          plot.caption  = element_text(colour = "grey45", size = 8),
          panel.grid.minor = element_blank(),
          legend.position  = "bottom")
}
src <- "Source: UN E-Government Survey 2024 (EGDI). Regions per UN DESA."

# ---------- 3. ITERATION 1 vs 2: REGIONAL AVERAGES ----------
# Version 1: ggplot defaults (no ordering, no labels, no context)
v1 <- ggplot(reg_sum, aes(region, mean_egdi)) + geom_col()
ggsave("figures/w3_regional_v1_default.png", v1, width = 7, height = 4, dpi = 200, bg = "white")

# Version 2: ordered, labelled, global benchmark, horizontal for readability
v2 <- ggplot(reg_sum, aes(x = reorder(region, mean_egdi), y = mean_egdi)) +
  geom_col(fill = "#0072B2", width = 0.65) +
  geom_text(aes(label = sprintf("%.3f", mean_egdi)), hjust = -0.15, size = 4) +
  geom_hline(yintercept = global_mean, linetype = "dashed", colour = "#D55E00") +
  annotate("text", x = 0.55, y = global_mean + 0.01, label = sprintf("World mean %.3f", global_mean),
           hjust = 0, vjust = 0, colour = "#D55E00", size = 3.6) +
  coord_flip(ylim = c(0, 1)) +
  labs(title = "Europe leads digital government;\nAfrica has the most ground to cover",
       subtitle = "Mean EGDI by region, 2024 (0 = lowest, 1 = highest)",
       x = NULL, y = "Mean EGDI score", caption = src) +
  theme_egov()
ggsave("figures/w3_regional_v2_refined.png", v2, width = 7, height = 4, dpi = 200, bg = "white")

# ---------- 4. SPREAD WITHIN REGIONS (box + jitter, India labelled) ----------
egdi <- egdi %>% mutate(region = factor(region, levels = region_order))
india <- egdi %>% filter(country == "India")

p_spread <- ggplot(egdi, aes(x = region, y = egdi)) +
  geom_boxplot(outlier.shape = NA, fill = "grey92", colour = "grey40", width = 0.55) +
  geom_jitter(aes(colour = group), width = 0.12, size = 1.8, alpha = 0.8) +
  geom_point(data = india, shape = 21, size = 4.5, stroke = 1.3, colour = india_col, fill = NA) +
  annotate("text", x = as.numeric(india$region) + 0.38, y = india$egdi, label = "India",
           colour = india_col, fontface = "bold", size = 4) +
  scale_colour_manual(values = pal_group, name = "EGDI group") +
  labs(title = "Asia and Oceania hide the widest gaps\nbetween countries",
       subtitle = "EGDI of every country, by region (each dot = one country)",
       x = NULL, y = "EGDI score", caption = src) +
  theme_egov()
ggsave("figures/w3_regional_spread.png", p_spread, width = 8, height = 4.8, dpi = 200, bg = "white")

# ---------- 5. COMPOSITION: SHARE OF COUNTRIES BY GROUP (100% stacked) ----------
p_share <- reg_grp %>%
  mutate(region = factor(region, levels = rev(region_order))) %>%
  ggplot(aes(x = region, y = share, fill = group)) +
  geom_col(width = 0.7, colour = "white") +
  geom_text(aes(label = ifelse(share >= 6, paste0(round(share), "%"), "")),
            position = position_stack(vjust = 0.5), colour = "white", size = 3.6, fontface = "bold") +
  scale_fill_manual(values = pal_group, name = "EGDI group", breaks = names(pal_group)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  coord_flip() +
  labs(title = "Every European country is High or Very High;\nonly 19 of 54 African countries are",
       subtitle = "Share of countries in each EGDI group, by region (2024)",
       x = NULL, y = "Share of countries", caption = src) +
  theme_egov()
ggsave("figures/w3_group_share.png", p_share, width = 8, height = 4.5, dpi = 200, bg = "white")

# ---------- 6. INDIA vs SOUTH ASIA PEERS ----------
sa <- egdi %>% filter(peer_group != "Other countries") %>%
  mutate(is_india = country == "India")
sa_mean <- mean(sa$egdi)

p_sa <- ggplot(sa, aes(x = reorder(country, egdi), y = egdi, fill = is_india)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.3f  (rank %d)", egdi, rank)), hjust = -0.05, size = 3.6) +
  geom_hline(yintercept = global_mean, linetype = "dashed", colour = "#D55E00") +
  geom_hline(yintercept = sa_mean,     linetype = "dotted", colour = "grey30") +
  annotate("text", x = 8.6, y = global_mean, label = " World mean", hjust = 0, vjust = 0,
           colour = "#D55E00", size = 3.4) +
  annotate("text", x = 8.6, y = sa_mean, label = "South Asia mean ", hjust = 1, vjust = 0,
           colour = "grey30", size = 3.4) +
  scale_fill_manual(values = c("FALSE" = "#7FB3D5", "TRUE" = india_col)) +
  coord_flip(ylim = c(0, 0.95), clip = "off") +
  labs(title = "India ranks second in South Asia,\njust behind the Maldives",
       subtitle = "EGDI 2024 for SAARC member states; India highlighted",
       x = NULL, y = "EGDI score", caption = src) +
  theme_egov()
ggsave("figures/w3_south_asia.png", p_sa, width = 8, height = 4.5, dpi = 200, bg = "white")

# ---------- 7. DECISION VIEW: COUNTRIES CLOSEST TO THE VERY HIGH THRESHOLD ----------
near <- egdi %>% filter(group == "High") %>% arrange(gap_to_very_high) %>% slice_head(n = 12)
near <- bind_rows(near, india %>% filter(!country %in% near$country)) %>%
  mutate(country = stringr::str_wrap(country, 24), is_india = country == "India")

p_near <- ggplot(near, aes(x = reorder(country, -gap_to_very_high), y = egdi)) +
  geom_segment(aes(xend = country, y = egdi, yend = 0.75), colour = "grey60", linewidth = 1) +
  geom_point(aes(colour = is_india), size = 4, show.legend = FALSE) +
  geom_hline(yintercept = 0.75, colour = "#0072B2", linewidth = 1) +
  annotate("text", x = 1, y = 0.752, label = "Very High threshold (0.75)", hjust = 0, vjust = -0.5,
           colour = "#0072B2", size = 3.6) +
  scale_colour_manual(values = c("FALSE" = "#009E73", "TRUE" = india_col)) +
  coord_flip(ylim = c(0.64, 0.81)) +
  labs(title = "Belarus is within 0.006 of Very High; India needs +0.082",
       subtitle = "High-group countries closest to the Very High threshold (India added for reference)",
       x = NULL, y = "EGDI score", caption = src) +
  theme_egov()
ggsave("figures/w3_near_threshold.png", p_near, width = 8, height = 5, dpi = 200, bg = "white")

# ---------- 8. PRIORITY TIERS BY REGION (from Week 2 case_when) ----------
tier_cols <- c("Tier 1: Leader (maintain)" = "#0072B2", "Tier 2: Consolidate" = "#009E73",
               "Tier 3: Accelerate" = "#E69F00", "Tier 4: Foundational support" = "#D55E00")
p_tier <- egdi %>% count(region, priority_tier) %>%
  ggplot(aes(x = region, y = n, fill = priority_tier)) +
  geom_col(colour = "white", width = 0.7) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), colour = "white",
            fontface = "bold", size = 3.6) +
  scale_fill_manual(values = tier_cols, name = NULL) +
  labs(title = "Where support is needed: 55 countries sit\nin the two lowest priority tiers",
       subtitle = "Number of countries per policy priority tier, by region",
       x = NULL, y = "Number of countries", caption = src) +
  theme_egov() + theme(legend.text = element_text(size = 9))
ggsave("figures/w3_priority_tiers.png", p_tier, width = 8, height = 4.8, dpi = 200, bg = "white")

# ---------- 9. OPEN-DATA PARTICIPATION: QUESTIONNAIRE RESPONSE vs SCORE ----------
p_msq <- ggplot(reg_sum, aes(x = msq_response_rate, y = mean_egdi, label = region)) +
  geom_point(aes(size = countries), colour = "#0072B2", alpha = 0.8) +
  geom_text(vjust = -1.4, size = 3.8) +
  scale_size_continuous(range = c(4, 12), name = "Countries") +
  scale_x_continuous(limits = c(10, 75), labels = label_percent(scale = 1)) +
  scale_y_continuous(limits = c(0.35, 0.92)) +
  labs(title = "Questionnaire response and EGDI tend to rise\ntogether (Europe is the exception)",
       subtitle = "Regional response rate to the UN Member State Questionnaire vs mean EGDI",
       x = "Share of countries with a questionnaire response", y = "Mean EGDI", caption = src) +
  theme_egov()
ggsave("figures/w3_msq_vs_egdi.png", p_msq, width = 8, height = 4.8, dpi = 200, bg = "white")

cat("All figures saved.\n")
