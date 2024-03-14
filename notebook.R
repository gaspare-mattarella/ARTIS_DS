library(tidyverse)
library(readr)
library(panelr)

general_dt <- read_csv("general_dt.csv")
underwriting_dt <- read_csv('underwriting_dt.csv')


### CLEANING general_dt ###
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
general_dt_panel <- sapply(general_dt_panel[4:11], as.numeric)
general_dt_panel <- as.data.frame(general_dt_panel)
general_dt_panel$id <- as.factor(general_dt_panel$id)
general_dt_panel$wave <- as.factor(general_dt_panel$wave)

general_dt_panel <- panel_data(general_dt_panel, id = id, wave = wave)

### CLEANING underwritings ###
names(underwriting_dt) <- sub(".{5}$", "", names(underwriting_dt))
row_as_array <- as.character(unlist(underwriting_dt[1, ]))
names(underwriting_dt) <- paste(names(underwriting_dt), "_", row_as_array, sep = "")


names(underwriting_dt) <- sub(".{2}$", "", names(underwriting_dt))

underwriting_dt <- underwriting_dt %>%
  rename(Firm = ...1_)


underwriting_dt <- underwriting_dt[-1,]

underwriting_dt_panel <- long_panel(underwriting_dt, prefix = "_", begin = 2016, end = 2020, label_location = "end")
underwriting_dt_panel <- sapply(underwriting_dt_panel[5:11], as.numeric)
underwriting_dt_panel <- as.data.frame(underwriting_dt_panel)
underwriting_dt_panel$id <- as.factor(underwriting_dt_panel$id)
underwriting_dt_panel$wave <- as.factor(underwriting_dt_panel$wave)

underwriting_dt_panel <- panel_data(underwriting_dt_panel, id = id, wave = wave)


#################################################################################


full_panel <- full_join(general_dt_panel,underwriting_dt_panel)


summary(full_panel, by.wave = FALSE, by.id = TRUE)
line_plot(general_dt_panel,SCR)
line_plot(general_dt_panel,SCR, add.mean = TRUE, alpha = 0.2)
line_plot(general_dt_panel,SCR, overlay = FALSE, add.mean = TRUE)
plotmeans(`SCR coverage r` ~ id, main="Heterogeineity across Firms", data=general_dt_panel)



summary(general_dt_panel,  `SCR coverage r`, by.wave = FALSE, by.id = TRUE)


