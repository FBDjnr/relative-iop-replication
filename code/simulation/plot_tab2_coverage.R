# Read all tab2_coverage_nc*.csv files, combine into one dataset with an "NC"
# column parsed from the filename, then plot Empirical_Coverage vs NC by SE_Estimator.

library(ggplot2)

# Set working directory to the location of the CSV files (adjust as needed)
setwd("G:/Shared drives/IOP Inference/R Codes/codeforsimulation")

# Locate the files.
# The pattern looks for files named like "tab2_coverage_nc<number>.csv".
files <- list.files(
  path       = ".",
  pattern    = "^tab2_coverage_nc[0-9]+\\.csv$",
  full.names = TRUE
)

if (length(files) == 0) {
  stop("No files matching 'tab2_coverage_nc<number>.csv' were found.")
}

# Read each file and tag it with the NC value taken from the end of its name
combined <- do.call(rbind, lapply(files, function(f) {
  df <- read.csv(f, stringsAsFactors = FALSE)
  nc <- as.integer(sub(".*tab2_coverage_nc([0-9]+)\\.csv$", "\\1", basename(f)))
  df$NC <- nc
  df
}))

# Exclude NC = 25 from the plot
combined <- combined[combined$NC != 25, ]

# Order by NC so the lines connect points left-to-right
combined <- combined[order(combined$NC), ]

# Line plot: Empirical_Coverage (y) by NC (x), one line per SE_Estimator.
# Black-and-white styling for journal submission: distinguish series by
# linetype and point shape rather than color.
p <- ggplot(combined, aes(x = NC, y = Empirical_Coverage,
                          linetype = SE_Estimator, shape = SE_Estimator,
                          group = SE_Estimator)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(100, max(combined$NC), by = 100)) +
  labs(
    x = "Number of Clusters per Stratum",
    y = "Empirical Coverage",
    linetype = "SE Estimator",
    shape = "SE Estimator",
    title = "Empirical Coverage by SE Estimator"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

print(p)

# Save the plot (PNG raster + PDF vector for journal submission) and the data
ggsave("fig2_coverage_plot.png", plot = p, width = 8, height = 5, dpi = 300)
ggsave("fig2_coverage_plot.pdf", plot = p, width = 8, height = 5)
write.csv(combined, "tab2_coverage_combined.csv", row.names = FALSE)
