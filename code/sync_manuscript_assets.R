#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Sync Manuscript Assets ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Refreshes the manuscript's self-contained copies of the generated tables and
# figures. The analysis scripts write LaTeX tables to output/tables/ and figures
# to output/figures/; the manuscript (manuscript/*.tex) inputs its OWN copies
# from manuscript/tables/ and manuscript/figures/ so that the manuscript folder
# compiles on its own. This utility copies the freshly generated assets into the
# manuscript so the two stay identical.
#
# Run this from the code/ directory after (re)generating the tables/figures:
#   source("sync_manuscript_assets.R")
#
# It does not modify any analysis script or the output/ folder.

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Paths ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

output_tables_dir <- file.path("..", "output", "tables")
output_figures_dir <- file.path("..", "output", "figures")
manuscript_tables_dir <- file.path("..", "manuscript", "tables")
manuscript_figures_dir <- file.path("..", "manuscript", "figures")


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Copy Helper ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Copy every file matching `pattern` from `from` into `to`, overwriting existing
# copies. Returns the number of files copied (invisibly).
sync_dir <- function(from, to, pattern, label) {
  if (!dir.exists(from)) {
    message("Source folder not found, skipping ", label, ": ", from)
    return(invisible(0L))
  }
  if (!dir.exists(to)) {
    dir.create(to, recursive = TRUE)
  }

  files <- list.files(from, pattern = pattern, full.names = TRUE)
  if (length(files) == 0L) {
    message("No ", label, " to sync in ", from)
    return(invisible(0L))
  }

  ok <- file.copy(files, to, overwrite = TRUE)
  message("Synced ", sum(ok), " ", label, " -> ", to)
  return(invisible(sum(ok)))
}


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Sync ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# LaTeX table fragments the manuscript \input{}s
sync_dir(output_tables_dir, manuscript_tables_dir, "\\.tex$", "tables")

# Figures the manuscript \includegraphics{}s (pdf/png/jpg)
sync_dir(output_figures_dir, manuscript_figures_dir,
         "\\.(pdf|png|jpg|jpeg)$", "figures")
