library(tidyverse)
library(padr)
library(akima)
library(lubridate)

############
### Akima
############

## Interpolation and convert to a dataframe
dpinterp <- function(df=df) {
  interp_df <- interp(x=df$DateTime_day, y=df$Depth, z=df$Temp, duplicate="strip")
  interp2xyz(interp_df, data.frame=TRUE)
}


## Read in the data, interpolate, and perform some data cleaning
ctd <- read_csv("2015Prof.csv") %>%
  thicken("day") %>%
  do(dpinterp(.)) %>%
  tbl_df() %>%
  rename(Date = x, Depth = y, Temp = z) %>%
  mutate(Date = as_date(Date, origin = lubridate::origin)) %>%
  filter(!is.na(Temp))


## Contour plot
ctd %>%
  ggplot(aes(x = Date, y = Depth, z = Temp, fill = Temp)) +
  geom_tile() +
  scale_y_reverse(expand = c(0,0)) +
  scale_x_date(expand = c(0, 0), date_labels = "%b", date_breaks = "1 month") +
  stat_contour(aes(fill=..level..), geom="polygon", binwidth=0.005) +
  geom_contour(color = "white", alpha = 0.5) +
  scale_fill_distiller(
    palette="RdYlGn",
    na.value="white") +
  theme_minimal()


#############
### MBA
##############
library(MBA)

##Compare to other method Spline on both?

df <- read_csv("2015Prof.csv") %>%
  thicken("day") %>%






