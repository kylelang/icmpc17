### Title:    Helper Functions for ICMPC17 Analysis
### Author:   Kyle M. Lang
### Created:  2023-05-18
### Modified: 2023-05-18

getp <- function(x, which) {
  p <- summary(x)$coefficients[which, 4]
  if(p < 0.001)
    "< 0.001"
  else
    paste0("= ", round(p, 3))
}