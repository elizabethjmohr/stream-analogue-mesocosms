library(tidyverse)
library(gridExtra)
set.seed(8675309)

treatments <- tibble(
  SAM = LETTERS[1:15],
  treatment = sample(rep(0:4, each = 3),replace = FALSE)
)

png(filename = "./plots/treatments.png")
grid.table(treatments)
dev.off()


