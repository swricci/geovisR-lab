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

##Nikki stuff below!!!
#Install and load plotly

library(plotly)


#California fires

ca_fires<-filter(us_fires, state=="CA" & fire_size>1000)

plot_template_ca <- ggplot(ca_fires, aes(y=as.numeric(fire_year))) +
  geom_hline(yintercept = seq(1992, 2015, by = 1), color = "gray", size = 0.05) +
  scale_size_area(max_size = 10, guide = FALSE) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_y_reverse(limits = c(2015,1992), breaks = c(2010,2005,2000,1995)) +
  xlab("") +
  ylab("") 

ca_bubble<- plot_template_ca +
  geom_point(aes(size = fire_size, x = plot_date, color = cause),alpha=0.7)



ca_bubble +
  facet_wrap(.~cause)


ggplotly(ca_bubble)



## Cool, let's customize the hover text
## To do that, we need to add a text aesthetic to the ggplot object. It recognizes html line breaks to make your labels look awesome! 

ca_bubble2 <- plot_template_ca +
  geom_point(aes(size = fire_size, x = plot_date, color = cause, group = 1, text=paste('Name:', mtbs_fire_name, '<br>Cause:', cause)), alpha=0.7)

ggplotly(ca_bubble2, tooltip = "text")


#Add discover and control dates (or anything else you think is interesting) to labels on your own. If you want to be really fancy, calculate fire duration and add that to hover text! (HINT: difftime() function )


ca_fires$duration_days<-difftime(ca_fires$cont_date, ca_fires$discovery_date, units="days")

plot_template_ca <- ggplot(ca_fires, aes(y=as.numeric(fire_year))) +
  geom_hline(yintercept = seq(1992, 2015, by = 1), color = "gray", size = 0.05) +
  scale_size_area(max_size = 10, guide = FALSE) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_y_reverse(limits = c(2015,1992), breaks = c(2010,2005,2000,1995)) +
  xlab("") +
  ylab("") 

ca_bubble3 <- plot_template_ca +
  geom_point(aes(size = fire_size, x = plot_date, color = cause, group = 1, text=paste('Name:', mtbs_fire_name, '<br>Cause:', cause, '<br>Duration:', duration_days, "days")), alpha=0.7)

g<-ggplotly(ca_bubble3, tooltip = "text")

#Let's add a range slider

g %>%
  rangeslider()

## That's kind of terrible for this type of graph - when might it be useful?

##Animation
#Plotly acccount - get your api settings
#Set them in your R environment
Sys.setenv("plotly_username"="ncinglis")
Sys.setenv("plotly_api_key"="RoIIfvlshFSZGNjeXZ5H")



##Use unoffical "frame" aesthetic in ggplotly

plot_template_ca <- ggplot(ca_fires, aes(y=as.numeric(fire_year))) +
  geom_hline(yintercept = seq(1992, 2015, by = 1), color = "gray", size = 0.05) +
  scale_size_area(max_size = 10, guide = FALSE) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_y_reverse(limits = c(2015,1992), breaks = c(2010,2005,2000,1995)) +
  xlab(month(ca_fires$plot_date)) +
  ylab("") 


ca_bubble4 <- plot_template_ca +
  geom_point(aes(frame=as.numeric(fire_year), size = fire_size, x = plot_date, color = cause, group = 1, 
                 text=paste('Name:', mtbs_fire_name, '<br>Cause:', cause, '<br>Duration:', duration_days, 'days')),
             alpha=0.7)

g<-ggplotly(ca_bubble4, tooltip = "text")


a<- g %>%
  animation_opts(1000, transition=500, easing="linear")
chart_link = api_create(a, filename="plotly-lab-an")
chart_link


#Try on your own: animate by month (use month() to extract month from date field) or cause, play with some of the other options in opts and in animationslider() and animationbutton()

ca_bubble5 <- plot_template_ca +
  geom_point(aes(frame=cause, size = fire_size, x = plot_date, color = cause, group = 1, 
                 text=paste('Name:', mtbs_fire_name, '<br>Cause:', cause, '<br>Duration:', duration_days, 'days')),
             alpha=0.7)

g<-ggplotly(ca_bubble5, tooltip = "text")


a<- g %>%
  animation_opts(frame=2000, transition=500, easing="elastic", mode="next")
chart_link = api_create(a, filename="plotly-lab-an")
chart_link



###Linking views with facet wrap 
f<- ca_bubble3 +
  facet_wrap(.~cause)

ggplotly(f, tooltip="text")

#Linking with highlighting 
#Hightlight the dataset, then remake the ggplots with that dataset


n <- highlight_key(ca_fires, ~cause)

p1 <- ggplot(n, aes(x=plot_date, fill = cause)) + 
  geom_density(alpha=0.7, colour=NA) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_y_continuous(position = "right")

plot_template_highlight <- ggplot(n, aes(y=as.numeric(fire_year))) +
  geom_hline(yintercept = seq(1992, 2015, by = 1), color = "gray", size = 0.05) +
  scale_size_area(max_size = 10, guide = FALSE) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_y_reverse(limits = c(2015,1992), breaks = c(2010,2005,2000,1995)) +
  xlab(month(ca_fires$plot_date)) +
  ylab("") 


p2 <- plot_template_highlight +
  geom_point(aes(size = fire_size, x = plot_date, color = cause, group = 1, 
                 text=paste('Name:', mtbs_fire_name, '<br>Cause:', cause, '<br>Duration:', duration_days, 'days')),
             alpha=0.7)




subplot(p2, p1) %>% hide_legend() %>% highlight("plotly_click")


##Task: Link a 3rd chart: A line chart of fire area over time. OR, change the highlight key to year. 



#SHINY >> Kimia - This linked plot OR ca_bubble3 for Shiny app if you want! 

nikki_finalplot<-subplot(p2, p1) %>% hide_legend() %>% highlight("plotly_click")
ca_bubble3

