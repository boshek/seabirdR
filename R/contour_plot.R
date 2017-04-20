library(tidyverse)
library(padr)
library(akima)
library(lubridate)
library(MBA)
library(gridExtra)

############
### DATA ###
############

data_df <- read_csv("2015Prof.csv") %>%
  thicken("day") %>%
  mutate(Date = decimal_date(DateTime_day))

#############################
### AKIMA solution linear ###
#############################

## Interpolation and convert to a dataframe
dpinterp <- function(df=df) {
  interp_df <- interp(x=df$DateTime_day, y=df$Depth, z=df$Temp, duplicate="strip", linear = TRUE, nx = 100, ny = 100)
  interp2xyz(interp_df, data.frame=TRUE)
}


## Read in the data, interpolate, and perform some data cleaning
ctd <- data_df %>%
  do(dpinterp(.)) %>%
  tbl_df() %>%
  rename(Date = x, Depth = y, Temp = z) %>%
  mutate(Date = as_date(Date, origin = lubridate::origin)) %>%
  filter(!is.na(Temp))


## Contour plot
akima_plt_linear <- ctd %>%
  ggplot(aes(x = Date, y = Depth, z = Temp, fill = Temp)) +
  geom_tile() +
  scale_y_reverse(expand = c(0,0)) +
  scale_x_date(expand = c(0, 0), date_labels = "%b", date_breaks = "1 month") +
  stat_contour(aes(fill=..level..), geom="polygon", binwidth=0.005) +
  geom_contour(color = "white", alpha = 0.5) +
  scale_fill_distiller(
    palette="RdYlGn",
    na.value="white") +
  labs(title = "AKIMA package solution - linear") +
  theme_minimal()



#############################
### AKIMA solution spline ###
#############################
## Interpolation and convert to a dataframe
dpinterp <- function(df=df) {
  interp_df <- interp(x=df$DateTime_day, y=df$Depth, z=df$Temp, duplicate="strip", linear = FALSE, jitter = 0.001, nx = 100, ny = 100)
  interp2xyz(interp_df, data.frame=TRUE)
}


## Read in the data, interpolate, and perform some data cleaning
ctd <- data_df %>%
  do(dpinterp(.)) %>%
  tbl_df() %>%
  rename(Date = x, Depth = y, Temp = z) %>%
  mutate(Date = as_date(Date, origin = lubridate::origin)) %>%
  filter(!is.na(Temp))


## Contour plot
akima_plt_spline <- ctd %>%
  ggplot(aes(x = Date, y = Depth, z = Temp, fill = Temp)) +
  geom_tile() +
  scale_y_reverse(expand = c(0,0)) +
  scale_x_date(expand = c(0, 0), date_labels = "%b", date_breaks = "1 month") +
  stat_contour(aes(fill=..level..), geom="polygon", binwidth=0.005) +
  geom_contour(color = "white", alpha = 0.5) +
  scale_fill_distiller(
    palette="RdYlGn",
    na.value="white") +
  labs(title = "AKIMA package solution - spline") +
  theme_minimal()

####################
### MBA solution ###
####################

mba <- mba.surf(df[,c('Date', 'Depth', 'Temp')], no.X = 100, no.Y = 100)
dimnames(mba$xyz.est$z) <- list(mba$xyz.est$x, mba$xyz.est$y)

df2 <- as.data.frame(mba$xyz.est$z) %>%
  tbl_df() %>%
  rownames_to_column(., var = "Date") %>%
  gather(Depth, Temp, -Date) %>%
  mutate(Depth = as.numeric(Depth), Date = as.numeric(Date)) %>%
  mutate(Date = dmy(format(date_decimal(Date), "%d-%m-%Y"))) %>%
  filter(!is.na(Temp))

mba_plt <- df2 %>%
  ggplot(aes(x = Date, y = Depth, z = Temp, fill = Temp)) + 
  geom_tile() + 
  scale_y_reverse(expand = c(0,0)) +
  scale_x_date(expand = c(0, 0), date_labels = "%b", date_breaks = "1 month") +
  stat_contour(aes(fill=..level..), geom="polygon", binwidth=0.005) + 
  geom_contour(color = "white", alpha = 0.5) + 
  scale_fill_distiller( 
    palette="RdYlGn", 
    na.value="white") + 
  labs(title = "MBA package solution") +
  theme_minimal() 


#########################
### Combine the plots ###
#########################

gAs <- ggplotGrob(akima_plt_spline)
gAl <- ggplotGrob(akima_plt_linear)
gM <- ggplotGrob(mba_plt)



## Have to draw a blank canvas 
gridExtra::grid.arrange(gAs,gAl,gM)