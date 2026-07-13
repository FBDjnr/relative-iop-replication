# Read all tab1_consistency_nc*.csv files, extract the Mean Design Effect
# (DEFF) from each, combine into one dataset with an "NC" column parsed from
# the filename, then plot DEFF vs NC.

library(ggplot2)

# Set working directory to the location of the CSV files (adjust as needed)
setwd("G:/Shared drives/IOP Inference/R Codes/codeforsimulation")

# Locate the files.
# The pattern looks for files named like "tab1_consistency_nc<number>.csv".
files <- list.files(
  path       = ".",
  pattern    = "^tab1_consistency_nc[0-9]+\\.csv$",
  full.names = TRUE
)

if (length(files) == 0) {
  stop("No files matching 'tab1_consistency_nc<number>.csv' were found.")
}

# Read each file (long "Statistic","Value" format), pull out the statistics we
# need, and tag with the NC value taken from the end of its name.
combined <- do.call(rbind, lapply(files, function(f) {
  df <- read.csv(f, stringsAsFactors = FALSE)
  nc <- as.integer(sub(".*tab1_consistency_nc([0-9]+)\\.csv$", "\\1", basename(f)))

  # Look up a statistic by its exact label; NA (with a warning) if absent.
  get_stat <- function(label) {
    v <- df$Value[df$Statistic == label]
    if (length(v) == 0) {
      warning(sprintf("'%s' not found in %s", label, basename(f)))
      return(NA_real_)
    }
    as.numeric(v[1])
  }

  data.frame(
    NC       = nc,
    True     = get_stat("True omega_r"),
    Estimate = get_stat("Mean estimate (omega_r_hat)"),
    RMSE     = get_stat("RMSE"),
    DEFF     = get_stat("Mean Design Effect (DEFF)")
  )
}))

# Exclude NC = 25 from the plot
combined <- combined[combined$NC != 25, ]

# Order by NC so the line connects points left-to-right
combined <- combined[order(combined$NC), ]

x_breaks <- seq(100, max(combined$NC), by = 100)

# ---- Design effect (DEFF) vs NC --------------------------------------------
# Black-and-white styling for journal submission.
p <- ggplot(combined, aes(x = NC, y = DEFF)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = x_breaks) +
  labs(
    x = "Number of Clusters per Stratum",
    y = "Mean Design Effect (DEFF)",
    title = "Mean Design Effect by Number of Clusters"
  ) +
  theme_bw()

print(p)

# Save the plot (PNG raster + PDF vector for journal submission) and the data
ggsave("fig1_deff_plot.png", plot = p, width = 8, height = 5, dpi = 300)
ggsave("fig1_deff_plot.pdf", plot = p, width = 8, height = 5)
write.csv(combined, "tab1_deff_combined.csv", row.names = FALSE)

# ---- Consistency: mean estimate converging to the truth as NC grows --------
# The mean omega_r_hat (with an RMSE band) should home in on the true omega_r
# and its spread should shrink as the number of clusters increases.
p_cons <- ggplot(combined, aes(x = NC, y = Estimate)) +
  geom_ribbon(aes(ymin = Estimate - RMSE, ymax = Estimate + RMSE),
              fill = "gray80", alpha = 0.6) +
  geom_hline(aes(yintercept = True[1]), linetype = "dashed", colour = "gray30") +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = x_breaks) +
  labs(
    x = "Number of Clusters per Stratum",
    y = expression("Mean estimate " * hat(theta)[r]),
    title = "Consistency of the Relative IOP Estimator",
    subtitle = "Dashed line = true omega_r; shaded band = +/- RMSE"
  ) +
  theme_bw()

print(p_cons)
ggsave("fig1_consistency_plot.png", plot = p_cons, width = 8, height = 5, dpi = 300)
ggsave("fig1_consistency_plot.pdf", plot = p_cons, width = 8, height = 5)
