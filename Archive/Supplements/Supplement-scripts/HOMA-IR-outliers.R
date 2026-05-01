library(ggplot2)
library(dplyr)
library(cowplot)

HOMA_insulin_raw_study1 = read.csv("HOMA_insulin_raw_study1.csv")
HOMA_insulin_raw_study2 = read.csv("HOMA_insulin_raw_study2.csv")

# ── DATA ──────────────────────────────────────────────────────────────────────
# Replace these two lines with your actual data frames
s1 <- data.frame(HOMA = HOMA_insulin_raw_study1$HOMA_insulin)          # Study 1
s2 <- data.frame(HOMA = HOMA_insulin_raw_study2$HOMA_insulin)     # Study 2

# ── HELPER: compute 3-SD cutoff and flag outliers ────────────────────────────
flag_outliers <- function(df) {
  cutoff <- median(df$HOMA, na.rm = TRUE) + 3 * sd(df$HOMA, na.rm = TRUE)
  df %>% mutate(flag = ifelse(HOMA > cutoff, "Outlier", "Normal"),
                cutoff = cutoff)
}

remove_outliers <- function(df) {
  df %>% filter(HOMA >= median(HOMA, na.rm = TRUE) - 3 * sd(HOMA, na.rm = TRUE),
                HOMA <= median(HOMA, na.rm = TRUE) + 3 * sd(HOMA, na.rm = TRUE))
}

s1_before <- flag_outliers(s1)
s1_after  <- remove_outliers(s1) %>% mutate(flag = "Normal")

s2_before <- flag_outliers(s2)
s2_after  <- remove_outliers(s2) %>% mutate(flag = "Normal")

# ── HELPER: build one boxplot panel ──────────────────────────────────────────
make_box <- function(df, title, show_cutoff = FALSE) {
  p <- ggplot(df, aes(x = "", y = HOMA)) +
    geom_boxplot(width = 0.4, fill = "#A5D8FF", alpha = 0.7,
                 outlier.shape = NA) +
    geom_jitter(aes(color = flag), width = 0.12, size = 2, alpha = 0.65) +
    scale_color_manual(values = c("Normal" = "#1B6CA8", "Outlier" = "darkred")) +
    labs(title = title, x = "", y = "HOMA-IR") +
    theme_classic(base_size = 11) +
    theme(plot.title    = element_text(face = "bold", hjust = 0, size = 10),
          axis.text.x   = element_blank(),
          axis.ticks.x  = element_blank(),
          legend.position = "none",
          aspect.ratio  = 1)
  
  # Add dashed cutoff line + label only on "before" panels
  if (show_cutoff) {
    cut_val <- unique(df$cutoff)[1]
    p <- p +
      geom_hline(yintercept = cut_val, linetype = "dashed",
                 color = "gray40", linewidth = 0.7) +
      annotate("text", x = 1.22, y = cut_val,
               label = paste0("Med+3SD ", round(cut_val, 1)),
               hjust = 0, vjust = -0.4, color = "gray30", size = 3)
  }
  p
}

# ── BUILD THE 4 PANELS ───────────────────────────────────────────────────────
p_A1 <- make_box(s1_before, "A: Study 1 – Raw Data", show_cutoff = TRUE)
p_A2 <- make_box(s1_after,  "Study 1 – After Outlier Removal",  show_cutoff = FALSE)

p_B1 <- make_box(s2_before, "B: Study 2 – Raw Data", show_cutoff = TRUE)
p_B2 <- make_box(s2_after,  "Study 2 – After Outlier Removal",  show_cutoff = FALSE)

# ── COMBINE ──────────────────────────────────────────────────────────────────
row_A <- plot_grid(p_A1, p_A2, ncol = 2, align = "hv")
row_B <- plot_grid(p_B1, p_B2, ncol = 2, align = "hv")

final_plot_outliers <- plot_grid(row_A, row_B, ncol = 1, rel_heights = c(1, 1))
final_plot_outliers
