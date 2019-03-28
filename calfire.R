calfire <- read_csv("data/calfire_frap.csv") %>%
  mutate(cause2 = case_when(cause == 1 | cause == 17 ~ "Natural",
                            cause == 14 | is.na(cause) ~ "Unknown",
                            cause != 1 | cause != 14 | cause != 17 ~ "Human"),
         plot_date = as.Date(format(alarm_date,"2017-%m-%d")))

# plot template
plot_template <- ggplot(calfire, aes(y=year_)) +
  geom_hline(yintercept = seq(1950, 2017, by = 1), color = "gray", size = 0.05) +
  scale_size_area(max_size = 10, guide = FALSE) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_y_reverse(limits = c(2017,1950), breaks = c(2010,1990,1970,1950)) +
  xlab("") +
  ylab("") #+
  #theme_hc(bgcolor = "darkunica", base_size = 20, base_family = "ProximaNova-Semibold") +
  #theme(axis.text = element_text(color = "#ffffff"))

plot_template +
  geom_point(aes(size=gis_acres, x=plot_date), color="#ffa500", alpha=0.7)
