library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)

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

#KIM: USE ONLY DATA FROM THE SE US STATES...use code below to get se_fires data that we used for the figures
#filter data to just the Southeast US states
se_fires<-filter(us_fires, state=="AL" | state == "AR" | state=="FL" | state=="GA" | state == "KY" | state == "LA" | state == "MS" | state=="NC" | state=="SC" | state == "TN" | state == "VA" | state == "WV")

#add column called plot_date to help with plotting the data
se_fires<-se_fires %>%
  mutate(plot_date = as.Date(format(date,"2017-%m-%d"))) #this gives you the data frame we worked with!


#BELOW HERE IS THE CODE FOR GGPLOT (AND GGPLOTLY) FIGURES


#simple bar plot showing proportion of different fire causes
barplot<-ggplot(se_fires)+
  geom_bar(aes(x=state,fill=stat_cause_descr),position = "fill")+
  theme_classic()

#create bubble plot - all fires in SE
#plot template modified from Buzzfeed code
plot_template_se <- ggplot(se_fires, aes(y=as.numeric(fire_year))) +
  geom_hline(yintercept = seq(1992, 2015, by = 1), color = "gray", size = 0.05) +
  scale_size_area(max_size = 10, guide = FALSE) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_y_reverse(limits = c(2015,1992), breaks = c(2010,2005,2000,1995)) +
  xlab("") +
  ylab("") 

#this takes about 10 sec to run, and a total of 30 sec to appear, so still a little slow.
se_bubble<- plot_template_se +
  geom_point(aes(size = fire_size, x = plot_date, color = cause),alpha=0.7)

# see bubble plot by state
se_bubble +
  facet_wrap(.~state)

#summarize data by month to plot time series

#add new column fire_month
se_fires <- se_fires %>% mutate(fire_month = month(date))

#summarize by state by month and year
se_fires_month <- se_fires %>%
  group_by(state, fire_month, fire_year) %>%
  summarise(area = sum(fire_size), n_fires = n())

#create month/year variable
se_fires_month <- mutate(se_fires_month, mdate = make_date(fire_year,fire_month))

time_series_area<- ggplot(data = se_fires_month, aes (x = mdate))+
  geom_line(aes(y = area, col = state))
  
time_series_firecount <- ggplot(data = se_fires_month, aes (x = mdate))+
  geom_line(aes(y = n_fires, col = state))


#extra treemap plot
##data table way to summarize
#se.fires.dt<-as.data.table(se.fires)
#se.fires.summary<-se.fires.dt[,.(count=.N),by=c("state","stat_cause_descr")]

se_fires_cause <- se_fires %>%
  group_by(state, stat_cause_descr) %>%
  summarise(count = n())

ggplot(data = subset(se_fires_cause, state == "FL"), aes(area=count,fill=stat_cause_descr,label=stat_cause_descr))+
  geom_treemap()+
  geom_treemap_text(colour = "white", place = "centre")

treemap<-ggplot(se.fires.summary,aes(area=count,fill=stat_cause_descr,label=stat_cause_descr))+
  geom_treemap()+
  geom_treemap_text( colour = "white", place = "centre")+
  facet_wrap(.~state)

