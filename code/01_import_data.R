library(readr)
library(dplyr)
library(fs)

# ------------------------------------------------------------------------------
# Import

raw_data <- 
  read_delim(path("raw_data", "assay_and_compound_data.txt")) |> 
  mutate(Class = factor(Class, levels = c("toxic", "nontoxic")))

# ------------------------------------------------------------------------------
# Save

save(raw_data, file = path("raw_data", "assay_and_compound_data.RData"))
