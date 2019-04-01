library(data.table)

se.fires.dt<-as.data.table(se.fires)
se.fires.summary<-se.fires.dt[,.(count=.N),by=c("state","stat_cause_descr")]


ggplot(se.fires.summary,aes(area=count,fill=stat_cause_descr,label=stat_cause_descr))+
  geom_treemap()+
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                    grow = TRUE)+
  facet_wrap(.~state)

treemap<-ggplot(se.fires.summary,aes(area=count,fill=stat_cause_descr,label=stat_cause_descr))+
  geom_treemap()+
  geom_treemap_text( colour = "white", place = "centre")+
  facet_wrap(.~state)

ggplotly(treemap)

bar<-ggplot(se.fires)+
  geom_bar(aes(x=state,fill=stat_cause_descr),position = "fill")+
  theme_classic()

ggplotly(bar)

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
