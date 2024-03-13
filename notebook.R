general_dt <- read_csv("general_dt.csv")
underwriting_dt <- read_csv('underwriting_dt.csv')

library(tidyr)

names(general_dt) <- sub(".{9}$", "", names(general_dt))
row_as_array <- as.character(unlist(general_dt[1, ]))
names(general_dt) <- paste(names(general_dt), "_", row_as_array, sep = "")


names(general_dt) <- sub(".{2}$", "", names(general_dt))

general_dt <- general_dt %>%
  rename(Firm = ...1_) %>%
  rename(SCR_2019 = `SCR _2019`) %>%
  rename(SCR_2020 = `SCR _2020`)
general_dt <- general_dt[-1,]

general_dt_panel <- long_panel(general_dt, prefix = "_", begin = 2016, end = 2020, label_location = "end")
