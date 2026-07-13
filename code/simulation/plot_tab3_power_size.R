# Read all tab3_type1_power_nc*.csv files (Table 3: two-sample Wald test),
# split each into the null scenario (Type I error = size) and the alternative
# scenarios (power), combine across NC, then make two graphs + two CSVs:
#   * size  vs NC  (should hover around the nominal alpha = 0.05)
#   * power vs NC, one line per true effect size (delta)
#
# Note: the tab3_power_detail_nc*.csv files hold the same rejection rates but
# without the True_delta / beta1 labels, so the *_type1_power_* files are used
# here as the more informative source.

library(ggplot2)

# Set working directory to the location of the CSV files (adjust as needed)
setwd("G:/Shared drives/IOP Inference/R Codes/codeforsimulation")

NOMINAL_ALPHA <- 0.05

# Locate the files. Pattern: "tab3_type1_power_nc<number>.csv".
files <- list.files(
  path       = ".",
  pattern    = "^tab3_type1_power_nc[0-9]+\\.csv$",
  full.names = TRUE
)

if (length(files) == 0) {
  stop("No files matching 'tab3_type1_power_nc<number>.csv' were found.")
}

# Read each file and tag it with the NC value taken from the end of its name.
combined <- do.call(rbind, lapply(files, function(f) {
  df <- read.csv(f, stringsAsFactors = FALSE)
  nc <- as.integer(sub(".*tab3_type1_power_nc([0-9]+)\\.csv$", "\\1", basename(f)))
  df$NC <- nc
  df
}))

# Within each NC, the row with the smallest |True_delta| is the null scenario
# (size / Type I error); every other row is a power scenario. This is robust to
# the source's "Objective" label, which can misclassify the near-null row.
combined$Scenario <- ave(
  abs(combined$True_delta), combined$NC,
  FUN = function(d) ifelse(d == min(d), "Size", "Power")
)

# Exclude NC = 25
combined <- combined[combined$NC != 25, ]
combined <- combined[order(combined$NC), ]

x_breaks <- seq(100, max(combined$NC), by = 100)

# ---- Size (Type I error) vs NC ---------------------------------------------
size_df <- combined[combined$Scenario == "Size", c("NC", "True_delta", "Rej_Rate")]
size_df <- size_df[order(size_df$NC), ]

p_size <- ggplot(size_df, aes(x = NC, y = Rej_Rate)) +
  geom_hline(yintercept = NOMINAL_ALPHA, linetype = "dashed", colour = "gray40") +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = x_breaks) +
  labs(
    x = "Number of Clusters per Stratum",
    y = "Empirical Type I Error Rate (Size)",
    title = "Empirical Size of the Two-Sample Wald Test",
    subtitle = "Dashed line = nominal alpha = 0.05"
  ) +
  theme_bw()

print(p_size)
ggsave("fig3_size_plot.png", plot = p_size, width = 8, height = 5, dpi = 300)
ggsave("fig3_size_plot.pdf", plot = p_size, width = 8, height = 5)
write.csv(size_df, "tab3_size_combined.csv", row.names = FALSE)

# ---- Power vs NC, one line per true effect size (delta) --------------------
power_df <- combined[combined$Scenario == "Power", c("NC", "True_delta", "Rej_Rate")]

# Label each effect size by its true delta (constant across NC); order the
# factor by delta so the legend reads small -> large effect.
delta_levels <- sort(unique(power_df$True_delta))
power_df$Effect <- factor(
  sprintf("delta = %.3f", round(power_df$True_delta, 3)),
  levels = sprintf("delta = %.3f", round(delta_levels, 3))
)
power_df <- power_df[order(power_df$True_delta, power_df$NC), ]

# Legend item labels as expressions so "delta" renders as the Greek letter.
effect_labels <- lapply(delta_levels, function(d)
  bquote(delta == .(sprintf("%.3f", d))))

# Legend title (true IOP difference between the two populations)
effect_lab <- expression(
  "True IOP difference  " *
    (delta == omega[r]^{(1)} - omega[r]^{(2)}))

p_power <- ggplot(power_df, aes(x = NC, y = Rej_Rate,
                                linetype = Effect, shape = Effect,
                                group = Effect)) +
  geom_hline(yintercept = 0.80, linetype = "dotted", colour = "gray40") +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = x_breaks) +
  scale_linetype_discrete(labels = effect_labels) +
  scale_shape_discrete(labels = effect_labels) +
  labs(
    x = "Number of Clusters per Stratum",
    y = "Empirical Power",
    linetype = effect_lab,
    shape = effect_lab,
    title = "Empirical Power of the Two-Sample Wald Test",
    subtitle = "Dotted line = 0.80 power threshold"
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(
    linetype = guide_legend(title.position = "top", title.hjust = 0.5),
    shape    = guide_legend(title.position = "top", title.hjust = 0.5)
  )

print(p_power)
ggsave("fig3_power_plot.png", plot = p_power, width = 8, height = 5, dpi = 300)
ggsave("fig3_power_plot.pdf", plot = p_power, width = 8, height = 5)
write.csv(power_df, "tab3_power_combined.csv", row.names = FALSE)
