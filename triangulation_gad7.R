# Triangulation: GAD-7 x EMA x EOD TET Block Comparison

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(ggplot2)

# =============================================================================
# APA-7 Theme
# =============================================================================
theme_apa7 <- theme_classic(base_size = 8, base_family = "Arial") +
  theme(plot.title       = element_text(size = 10, face = "bold", hjust = 0.5),
        strip.text       = element_text(size = 9, face = "bold"),
        strip.background = element_blank(),
        axis.title       = element_text(size = 9),
        panel.grid.major.y = element_line(color = "#DDDDDD", linewidth = 0.5),
        panel.border     = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
        panel.spacing    = unit(0.6, "cm"),
        legend.position  = "bottom")
theme_set(theme_apa7)

# =============================================================================
# Configuration
# =============================================================================
output_dir <- "figures/triangulation"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

shared_dims  <- c("Anxiety", "Pressure", "Worry/Rumination")
block_def    <- tibble(Block = c("Pre","Guided Breathing","Open Monitoring","Post"),
                       start = c(1, 6, 11, 16), end = c(5, 10, 15, 20))
block_levels <- block_def$Block
block_labels <- c("Pre", "GB", "OM", "Post")

# GAD-7 - Day 20 is a second Post measurement, I used Day 16 for block comparison
gad_days <- tibble(Day_Num = c(1, 6, 11, 16), Block = block_levels)

inst_colors <- c("GAD-7" = "#333333", "EMA" = "#E69F00", "EOD TET" = "#56B4E9")
inst_shapes <- c("GAD-7" = 16, "EMA" = 15, "EOD TET" = 17)

assign_block <- function(day) {
  i <- which(day >= block_def$start & day <= block_def$end)
  if (length(i) == 1) block_def$Block[i] else NA_character_
}

cm_sem <- function(mat) {
  normed <- mat - rowMeans(mat) + mean(mat)
  apply(normed, 2, sd) / sqrt(nrow(mat)) * sqrt(ncol(mat) / max(ncol(mat) - 1, 1))
}

# =============================================================================
# Load & Prepare Data
# =============================================================================
gad <- read_csv("GAD7_scores.csv", na = c("na", "NA", ""), show_col_types = FALSE) %>%
  mutate(across(c(Participant_ID, Day_Num, GAD7), as.numeric)) %>%
  inner_join(gad_days, by = "Day_Num") %>%
  filter(!is.na(GAD7))

ema_block <- read_csv("all_EMA_Preprocessed.csv", show_col_types = FALSE) %>%
  mutate(across(c(Participant_ID, Day_Num, Intensity), as.numeric),
         Block = map_chr(Day_Num, assign_block)) %>%
  group_by(Participant_ID, Block, Dimension) %>%
  summarise(Intensity = mean(Intensity, na.rm = TRUE), .groups = "drop")

# EOD TET: daily mean first, then block mean
eod_block <- read_csv("all_data_master.csv", show_col_types = FALSE) %>%
  mutate(across(c(Participant_ID, Day_Num, Intensity), as.numeric)) %>%
  filter(Session_Type == "EndOfDay", Dimension %in% shared_dims) %>%
  mutate(Block = map_chr(Day_Num, assign_block)) %>%
  group_by(Participant_ID, Day_Num, Block, Dimension) %>%
  summarise(Intensity = mean(Intensity, na.rm = TRUE), .groups = "drop") %>%
  group_by(Participant_ID, Block, Dimension) %>%
  summarise(Intensity = mean(Intensity), .groups = "drop")

# =============================================================================
# Unified Long-Format + Z-Score
# =============================================================================
unified <- bind_rows(
  # GAD-7 is unidimensional -> mapped to Anxiety only
  gad %>% transmute(Participant_ID, Block, Dimension = "Anxiety",
                    Instrument = "GAD-7", value = GAD7),
  ema_block %>% transmute(Participant_ID, Block, Dimension,
                          Instrument = "EMA", value = Intensity),
  eod_block %>% transmute(Participant_ID, Block, Dimension,
                          Instrument = "EOD TET", value = Intensity)
) %>%
  mutate(Participant_ID = as.integer(Participant_ID),
         Block      = factor(Block, levels = block_levels),
         Dimension  = factor(Dimension, levels = shared_dims),
         Instrument = factor(Instrument, levels = names(inst_colors))) %>%
  group_by(Instrument, Participant_ID, Dimension) %>%
  mutate(z = if (sd(value) > 0) (value - mean(value)) / sd(value) else rep(0, n())) %>%
  ungroup()

# Group means + Cousineau-Morey SEM
group_means <- unified %>%
  group_by(Instrument, Dimension, Block) %>%
  summarise(z_mean = mean(z), .groups = "drop")

sem_data <- unified %>%
  group_by(Instrument, Dimension) %>%
  group_modify(~ {
    wide <- .x %>% select(Participant_ID, Block, z) %>%
      pivot_wider(names_from = Block, values_from = z)
    mat <- as.matrix(wide[, -1])
    if (nrow(mat) >= 2 && !any(is.na(mat)))
      tibble(Block = factor(colnames(mat), levels = block_levels), sem = cm_sem(mat))
    else
      tibble(Block = factor(character(0), levels = block_levels), sem = numeric(0))
  }) %>% ungroup()

gp <- left_join(group_means, sem_data, by = c("Instrument", "Dimension", "Block"))

# =============================================================================
# Figure 1: Group-Level (1x3)
# =============================================================================
fig1 <- ggplot() +
  geom_hline(yintercept = 0, color = "#AAAAAA", linewidth = 0.4, linetype = "dashed") +
  # Faded individual lines
  geom_line(data = unified,
            aes(Block, z, group = interaction(Participant_ID, Instrument), color = Instrument),
            linewidth = 0.4, alpha = 0.3) +
  geom_point(data = unified,
             aes(Block, z, color = Instrument, shape = Instrument),
             size = 1.5, alpha = 0.3) +
  # Group mean + SEM
  geom_errorbar(data = filter(gp, !is.na(sem)),
                aes(Block, ymin = z_mean - sem, ymax = z_mean + sem, color = Instrument),
                width = 0.15, linewidth = 0.6) +
  geom_line(data = gp, aes(Block, z_mean, group = Instrument, color = Instrument),
            linewidth = 1.5) +
  geom_point(data = gp, aes(Block, z_mean, color = Instrument, shape = Instrument), size = 3) +
  facet_wrap(~ Dimension, nrow = 1, scales = "free_y") +
  scale_color_manual(values = inst_colors) +
  scale_shape_manual(values = inst_shapes) +
  scale_x_discrete(labels = block_labels) +
  labs(y = "z-scored intensity",
       title = "Multi-Instrument Block Comparison (z-scored within instrument)") +
  theme(strip.placement = "outside")

ggsave(file.path(output_dir, "fig_multi_instrument_block_comparison.png"),
       fig1, width = 9, height = 4, dpi = 300)
cat("Saved ->", file.path(output_dir, "fig_multi_instrument_block_comparison.png"), "\n")

# =============================================================================
# Figure 2: Individual (3x3)
# =============================================================================
fig2 <- ggplot(unified, aes(Block, z, group = Instrument,
                             color = Instrument, shape = Instrument)) +
  geom_hline(yintercept = 0, color = "#AAAAAA", linewidth = 0.4, linetype = "dashed") +
  geom_line(linewidth = 1.5) + geom_point(size = 3) +
  facet_grid(Participant_ID ~ Dimension, scales = "free_y",
             labeller = labeller(Participant_ID = ~ paste0("P", .))) +
  scale_color_manual(values = inst_colors) +
  scale_shape_manual(values = inst_shapes) +
  scale_x_discrete(labels = block_labels) +
  labs(y = "z-scored intensity",
       title = "Individual Block Comparison (z-scored within instrument)")

ggsave(file.path(output_dir, "fig_individual_block_comparison.png"),
       fig2, width = 9, height = 8, dpi = 300)
cat("Saved ->", file.path(output_dir, "fig_individual_block_comparison.png"), "\n")

# =============================================================================
# Convergence Statistics
# =============================================================================
cat("\n", strrep("=", 72), "\n")
cat("BLOCK-LEVEL CONVERGENCE: Spearman rho across 4 blocks\n")
cat(strrep("=", 72), "\n")

safe_rho <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  if (sum(ok) < 3) return(NA_real_)
  cor(a[ok], b[ok], method = "spearman")
}

fmt <- function(r) if (is.na(r)) "   ---   " else sprintf("r=%+.2f", r)

bv <- function(df, pid, dim, col = "Intensity") {
  sub <- df %>% filter(Participant_ID == pid, Dimension == dim)
  tibble(Block = block_levels) %>%
    left_join(sub %>% select(Block, val = all_of(col)), by = "Block") %>% pull(val)
}

for (dim in shared_dims) {
  has_gad <- dim == "Anxiety"
  cat(sprintf("\n  %s\n", dim))
  hdr <- sprintf("  %-14s %12s", "Participant", "EMA vs TET")
  if (has_gad) hdr <- paste0(hdr, sprintf(" %14s %14s", "EMA vs GAD-7", "TET vs GAD-7"))
  cat(hdr, "\n")

  for (pid in 1:3) {
    ev <- bv(ema_block, pid, dim)
    tv <- bv(eod_block, pid, dim)
    line <- sprintf("  P%-13d %12s", pid, fmt(safe_rho(ev, tv)))
    if (has_gad) {
      gv <- tibble(Block = block_levels) %>%
        left_join(gad %>% filter(Participant_ID == pid) %>% select(Block, val = GAD7),
                  by = "Block") %>% pull(val)
      line <- paste0(line, sprintf(" %14s %14s",
                                   fmt(safe_rho(ev, gv)), fmt(safe_rho(tv, gv))))
    }
    cat(line, "\n")
  }
}

cat("\n", strrep("-", 72), "\n")
cat("Note: With k=4 blocks, Spearman rho is descriptive only (min p ~ .083).\n")
