

# libraries --------------------------------------------------------------------
library(tidyverse)
library(sjPlot)
library(table1)


# model summaries --------------------------------------------------------------
tab_model(mod_1)
tab_model(mod_2)


# miscellaneous tables ---------------------------------------------------------
hgns_table <- hgns %>%
  mutate(
    Q5 = factor(
      Q5,
      levels = c(
        "< 20",
        "20-50",
        "50-100",
        "100-200",
        "> 200"
      )
    )
  )


table1(
  ~ as.factor(dissection) | residency,
  hgns_table
)


table1(
  ~ as.factor(time) | dissection,
  hgns_table
)


table1(
  ~ as.factor(time) | Q5,
  hgns_table
)


table1(
  ~ Q11 | residency,
  hgns_table
)

