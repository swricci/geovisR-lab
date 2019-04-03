library(ggplot2)
library(dplyr)
library(readr)

# load data, tidy and filter (code from Buzzfeed)
files <- list.files("data/us_fires")

us_fires <- data_frame()

#note: status bar will pop up for only a few of the files, but all files are processed.
for (f in files) {
  tmp <- read_csv(paste0("data/us_fires/",f), col_types = cols(
    .default = col_character(),
    stat_cause_code = col_double(),
    cont_date = col_datetime(format = ""),
    discovery_date = col_datetime(format = ""),
    cont_doy = col_integer(),
    cont_time = col_integer(),
    fire_size = col_double(),
    latitude = col_double(),
    longitude = col_double()
  ))
  us_fires <- bind_rows(us_fires,tmp)
}
rm(tmp)

# assign fires to main causes
us_fires <- us_fires %>%
  mutate(cause = case_when(stat_cause_code == 1  ~ "Natural",
                           stat_cause_code == 13 | is.na(stat_cause_code) ~ "Unknown",
                           stat_cause_code >= 2 | stat_cause_code <= 12 ~ "Human"),
         date = as.Date(case_when(is.na(discovery_date) ~ cont_date,
                                  !is.na(discovery_date) ~ discovery_date)))


##data exploration##

fire.summary<-count(us_fires,state, sort = T)
#top 5: CA, GA, TX, NC, FL

nc.fires<-filter(us_fires, state=="NC")


ggplot(nc.fires)+
  geom_bar(aes(x=fire_year))+
  facet_wrap(.~cause)+
  theme_classic()+
  coord_flip()

ggplot(nc.fires)+
  geom_point(aes(size = fire_size, x = date, y=fire_size, color = cause))+
  theme_classic()


#need to create a "plot date" which is just the date of the fire, but all in one year (ex 2017) This is so that the fires are plotted by month on the x and year is in the y position

nc.fires<-nc.fires %>%
  mutate(plot_date = as.Date(format(date,"2017-%m-%d")))

plot_template_nc <- ggplot(nc.fires, aes(y=as.numeric(fire_year))) +
  geom_hline(yintercept = seq(1992, 2015, by = 1), color = "gray", size = 0.05) +
  scale_size_area(max_size = 10, guide = FALSE) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_y_reverse(limits = c(2015,1992), breaks = c(2010,2005,2000,1995)) +
  xlab("") +
  ylab("") 

plot_template+
  geom_point(aes(size = fire_size, x = plot_date),alpha=0.7)+
  facet_wrap(.~cause)


se.fires<-filter(us_fires, state=="NC" | state=="SC" | state=="FL" | state=="GA")

se.fires<-se.fires %>%
  mutate(plot_date = as.Date(format(date,"2017-%m-%d")))


ggplot(se.fires,aes(log1p(fire_size),fill=cause))+
  geom_histogram(binwidth = 1)

plot_template_se <- ggplot(se.fires, aes(y=as.numeric(fire_year))) +
  geom_hline(yintercept = seq(1992, 2015, by = 1), color = "gray", size = 0.05) +
  scale_size_area(max_size = 10, guide = FALSE) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_y_reverse(limits = c(2015,1992), breaks = c(2010,2005,2000,1995)) +
  xlab("") +
  ylab("") 

plot_template_se+
  geom_point(aes(size = fire_size, x = plot_date,color=cause),alpha=0.7)+
  facet_wrap(.~state)


#super bubble chart
us_fires<-us_fires %>%
  mutate(plot_date = as.Date(format(date,"2017-%m-%d")))

plot_template_us<- ggplot(us_fires, aes(y=as.numeric(fire_year))) +
  geom_hline(yintercept = seq(1992, 2015, by = 1), color = "gray", size = 0.05) +
  scale_size_area(max_size = 10, guide = FALSE) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_y_reverse(limits = c(2015,1992), breaks = c(2010,2005,2000,1995)) +
  xlab("") +
  ylab("") 

plot_template_us+
  geom_point(aes(size = fire_size, x = plot_date),alpha=0.7)+
  facet_wrap(.~cause)

us_bubble<-plot_template_us+
  geom_point(aes(size = fire_size, x = plot_date, color=cause),alpha=0.7)

ggplotly(us_bubble)
