library(tidyverse)
library(readr)
library(panelr)
library(xtsum)

general_dt <- read_csv("general_dt.csv")
underwriting_dt <- read_csv('underwriting_dt.csv')


### CLEANING general ###
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
general_dt_panel <- sapply(general_dt_panel[3:11], as.numeric)
general_dt_panel <- as.data.frame(general_dt_panel)

general_dt_panel$id <- as.factor(general_dt_panel$id)
general_dt_panel$wave <- as.factor(general_dt_panel$wave)

general_dt_panel <- panel_data(general_dt_panel, id = id, wave = wave)

ggplot(stack(general_dt_panel), aes(x = ind, y = values)) +
  geom_boxplot()
### CLEANING underwriting ###
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


ggplot(stack(as.data.frame(underwriting_dt_panel)), aes(x = ind, y = values)) +
  geom_boxplot()

underwriting_dt_panel %>%
  filter(if_any(is.numeric, ~ . < -700000))

#################################################################################


full_panel <- full_join(general_dt_panel,underwriting_dt_panel)

boxplot(as.data.frame(full_panel%>% dplyr::filter(id == 118)))

xtsum(na.rm = T, pdata.frame(full_panel))
# Let's start with some basic summay stats for the main metrics
gwp <- summary(full_panel, `GWP `, by.wave = FALSE, by.id = TRUE)
gwp <- gwp %>% dplyr::filter(!is.nan(numeric.mean))


highchart() %>%
  hc_title(text = "GWP Mean per Firm with Min and Max Value") %>%
  hc_xAxis(title = list(text = "Firms")) %>%
  hc_yAxis(title = list(text = "GWP")) %>%
  hc_add_series(gwp, type = "point", hcaes(x = id, y = numeric.mean),tooltip = list(pointFormat = "Firm ID: {point.id} ///
                                                                                    Mean: {point.y}"))%>%
  hc_add_series(gwp, type = "errorbar", hcaes(x = id, y = numeric.mean, low = numeric.p0, high = numeric.p100))

top15_gwp <- gwp %>% arrange(desc(numeric.mean)) %>% dplyr::slice_head(n=15)


# Let's start with some basic summary stats for the main metrics
scr_r <- summary(full_panel, `SCR coverage r`, by.wave = FALSE, by.id = TRUE)
scr_r <- scr_r %>% dplyr::filter(!is.nan(numeric.mean))


highchart() %>%
  hc_title(text = "SCR_ratio Mean per Firm with Min and Max Value") %>%
  hc_xAxis(title = list(text = "Firms")) %>%
  hc_yAxis(title = list(text = "scr_ratio")) %>%
  hc_add_series(scr_r, type = "point", hcaes(x = id, y = numeric.mean),tooltip = list(pointFormat = "Firm ID: {point.id} ///
                                                                                    Mean: {point.y}"))%>%
  hc_add_series(scr_r, type = "errorbar", hcaes(x = id, y = numeric.mean, low = numeric.p0, high = numeric.p100))

scr_r_outliers <- scr_r %>% dplyr::filter(numeric.mean > 150)
print(scr_r_outliers)
# removing the outliers
scr_r <- scr_r[-as.numeric(scr_r_outliers$id),]

highchart() %>%
  hc_title(text = "SCR_ratio Mean per Firm with Min and Max Value") %>%
  hc_xAxis(title = list(text = "Firms")) %>%
  hc_yAxis(title = list(text = "scr_ratio")) %>%
  hc_add_series(scr_r, type = "point", hcaes(x = id, y = numeric.mean),tooltip = list(pointFormat = "Firm ID: {point.id} ///
                                                                                    Mean: {point.y}"))%>%
  hc_add_series(scr_r, type = "errorbar", hcaes(x = id, y = numeric.mean, low = numeric.p0, high = numeric.p100))


worst15_scr_r <- scr_r %>% arrange(numeric.mean) %>% dplyr::slice_head(n=15)
top15_scr_r <- scr_r %>% arrange(desc(numeric.mean)) %>% dplyr::slice_head(n=15)

# Let's start with some basic summary stats for the main metrics
tota <- summary(full_panel,`Total assets `, by.wave = FALSE, by.id = TRUE)
tota <- gwp %>% dplyr::filter(!is.nan(numeric.mean))


highchart() %>%
  hc_title(text = "Total Asset Mean per Firm with Min and Max Value") %>%
  hc_xAxis(title = list(text = "Firms")) %>%
  hc_yAxis(title = list(text = "Total Assets")) %>%
  hc_add_series(tota, type = "point", hcaes(x = id, y = numeric.mean),tooltip = list(pointFormat = "Firm ID: {point.id} ///
                                                                                    Mean: {point.y}"))%>%
  hc_add_series(tota, type = "errorbar", hcaes(x = id, y = numeric.mean, low = numeric.p0, high = numeric.p100))

top15_tota <- tota %>% arrange(desc(numeric.mean)) %>% dplyr::slice_head(n=15)

# Let's calculate the CAGR for GCI

df <- full_panel %>%
    group_by(id) %>%
    mutate(lagD = dplyr::lag(`Gross claims incurred (£m`,1), #get lad Data

         CAGR = (`Gross claims incurred (£m` / lagD)^(1/1) - 1)%>%
  select(`Gross claims incurred (£m`,CAGR) %>%
   dplyr::filter(!is.infinite(CAGR) )

df_gci_cagr <- as.data.frame(df) %>%
  arrange(desc(CAGR)) %>%
  slice_head(n=15)


full_panelx <- full_panel %>%
  dplyr::filter(id %in% df_gci_cagr$id)


line_plot(full_panelx, `Gross claims incurred (£m`, add.mean = TRUE, alpha = 0.2)

highchart() %>%
  hc_title(text = "GCI of 15 Firms with highest CAGR") %>%
  hc_xAxis(title = list(text = "Firms")) %>%
  hc_yAxis(title = list(text = "GCI")) %>%
  hc_add_series(full_panelx, type = "line", hcaes(name = id, y = `Gross claims incurred (£m`, x = wave), tooltip = list(pointFormat = "Firm ID: {point.id} ///
                                                                                    CGI: {point.y}"))

x <- pdata.frame(full_panel)

within_min(pdata.frame(full_panel), "Gross.claims.incurred...m",  na.rm = T)
# Let's start with some basic summay stats for the main metrics
gwp <- summary(full_panel, `GWP `, by.wave = FALSE, by.id = TRUE)
gwp <- gwp %>% dplyr::filter(!is.nan(numeric.mean))


highchart() %>%
  hc_title(text = "GWP Mean per Firm with Min and Max Value") %>%
  hc_xAxis(title = list(text = "Firms")) %>%
  hc_yAxis(title = list(text = "GWP")) %>%
  hc_add_series(gwp, type = "point", hcaes(x = id, y = numeric.mean),tooltip = list(pointFormat = "ID: {point.id} ///
                                                                                    Mean: {point.y}"))%>%
  hc_add_series(gwp, type = "errorbar", hcaes(x = id, y = numeric.mean, low = numeric.p0, high = numeric.p100))


NWP <- summary(full_panel, NWP, by.wave = FALSE, by.id = TRUE)
NCr <- summary(full_panel, `Net combined ratio`, by.wave = FALSE, by.id = TRUE)

NCr <- NCr[-c(418,99),]
highchart() %>%
  hc_title(text = "Dual Axis Chart") %>%
  hc_yAxis_multiples(
    list(title = list(text = "Primary Axis")),
    list(title = list(text = "Secondary Axis"), opposite = T)
  ) %>%
  hc_add_series(NWP, "point", hcaes(x=id, y=numeric.mean), yAxis = 1) %>%
  hc_add_series(NCr, "point", hcaes(x=id, y=numeric.mean), yAxis = 0)
line_plot(general_dt_panel, `GWP `)
line_plot(full_panel, `Gross claims incurred (£m`, add.mean = TRUE, alpha = 0.2)
line_plot(full_panel, `Gross claims incurred (£m`, overlay = FALSE, add.mean = TRUE)
plotmeans(`SCR coverage r` ~ id, main="Heterogeineity across Firms", data=general_dt_panel)



summary(general_dt_panel,  `SCR coverage r`, by.wave = FALSE, by.id = TRUE)


