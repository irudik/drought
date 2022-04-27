setwd("C:\\Users\\anton\\Dropbox\\drought")
int_folder = ".\\data\\intermediate\\"
out_folder = ".\\output\\"

library(tidyverse)
library(tidyr)
library(dplyr)
library(fixest)
library(lubridate)
library(RColorBrewer)
library(knitr)
library(broom)
library(ggpubr)


#Get drought months data
load(paste(int_folder,"drought_df.RData", sep = ""))

#Get sector-level data
load(paste(int_folder, "main_sector_df.RData", sep = ""))

#Get water intensity data
load(paste0(int_folder, "est_water_intensity.RData"))

#This chunk prepares the data
full_sector_df<-merge(main_sector_df, drought_df, by=c("GeoFIPS", "year"), all = TRUE)
full_sector_df$drought_month12<--1*full_sector_df$drought_month12
full_sector_df$log_inc<-log(as.numeric(full_sector_df$inc)+1)
full_sector_df$log_emp<-log(full_sector_df$emp+1)

out1<-feols(log_emp~ drought_month12| GeoFIPS^naics2 + year^naics2 ,
            data = full_sector_df,
            vcov = "conley"
)

out2<-feols(log_emp~west:drought_month12 + drought_month12| GeoFIPS^naics2 + year^naics2 ,
            data = full_sector_df,
            vcov = "conley"
)

out3<-feols(log_inc~ drought_month12| GeoFIPS^naics2 + year^naics2 ,
            data = full_sector_df,
            vcov = "conley"
)

out4<-feols(log_inc~west:drought_month12 + drought_month12| GeoFIPS^naics2 + year^naics2 ,
            data = full_sector_df,
            vcov = "conley"
)

etable(out1, out2, out3, out4, tex = TRUE)


### Regressions with water intensity
est_water_intensity<-est_water_intensity %>% subset(select = c("naics2", "total_win"))
full_sector_df<-merge(full_sector_df, est_water_intensity, by = "naics2", all = TRUE)
full_sector_df$log_win<-log(full_sector_df$total_win)

out1<-feols(log_inc~ west*total_win*drought_month12 | GeoFIPS^naics2 + year^naics2 ,
            data = full_sector_df,
            vcov = "conley"
)

out2<-feols(log_emp~ west*total_win*drought_month12 | GeoFIPS^naics2 + year^naics2 ,
            data = full_sector_df,
            vcov = "conley"
)

etable(out1, out2, tex = TRUE)


##FOR NOW, ADD BARPLOTS TO PAPER

out_west_inc<-tidy(feols(
  log_inc~i(naics2, drought_month12)| GeoFIPS^naics2 + naics2^year,
  data = filter(full_sector_df, west == 1),
  vcov = "conley"
))

out_west_inc$term<-c("11","21","22","23","31-33","42","44-45","48-49","51","52","53","54",
                     "55","56","61","62","71","72","81") #replace with naics codes

barplot_inc_west <- out_west_inc %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, color = "grey50", size = 1, linetype = "dashed") +
  geom_bar(position = position_dodge(), stat = "identity", fill = "maroon", alpha = 0.4, width = .25) +
  geom_errorbar(
    position = position_dodge(), width = 0.125,
    aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error)
  ) +
  # annotate(
  #   geom = "text", x = 4.5, y = .1, label = "Reduction in % of\nstudents at highest\nachievement levels",
  #   color = "black", size = 5
  # ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    title = element_text(size = 26),
    axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18),
    panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black"), axis.ticks = element_line()
  ) +
  labs(
    title = "Percent change in sectoral income, west",
    x = "NAICS 2-digit code",
    y = "Change in Percent by industry"
  )

barplot_inc_west

# ggsave(
#   file = paste0(out_folder, "barplot_inc_west.png"),
#   width = 10,
#   height = 10
# )

out_east_inc<-tidy(feols(
  log_inc~i(naics2, drought_month12)| GeoFIPS^naics2 + naics2^year,
  data = filter(full_sector_df, west == 0),
  vcov = "conley"
))

out_east_inc$term<-c("11","21","22","23","31-33","42","44-45","48-49","51","52","53","54",
                     "55","56","61","62","71","72","81") #replace with naics codes

barplot_inc_east <- out_east_inc %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, color = "grey50", size = 1, linetype = "dashed") +
  geom_bar(position = position_dodge(), stat = "identity", fill = "blue", alpha = 0.4, width = .25) +
  geom_errorbar(
    position = position_dodge(), width = 0.125,
    aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error)
  ) +
  # annotate(
  #   geom = "text", x = 4.5, y = .1, label = "Reduction in % of\nstudents at highest\nachievement levels",
  #   color = "black", size = 5
  # ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    title = element_text(size = 26),
    axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18),
    panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black"), axis.ticks = element_line()
  ) +
  labs(
    title = "Percent change in sectoral income, east",
    x = "NAICS 2-digit code",
    y = "Change in Percent by industry"
  )

barplot_inc_east

# ggsave(
#   file = paste0(out_folder, "barplot_inc_east.png"),
#   width = 10,
#   height = 10
# )

######

out_west_emp<-tidy(feols(
  log_emp~i(naics2, drought_month12)| GeoFIPS^naics2 + naics2^year,
  data = filter(full_sector_df, west == 1 & naics2 != "92" & naics2 != "95" & naics2 != "99"),
  vcov = "conley"
))

out_west_emp$term<-c("11","21","22","23","31-33","42","44-45","48-49","51","52","53","54",
                     "55","56","61","62","71","72","81") #replace with naics codes

barplot_emp_west <- out_west_emp %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, color = "grey50", size = 1, linetype = "dashed") +
  geom_bar(position = position_dodge(), stat = "identity", fill = "maroon", alpha = 0.4, width = .25) +
  geom_errorbar(
    position = position_dodge(), width = 0.125,
    aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error)
  ) +
  # annotate(
  #   geom = "text", x = 4.5, y = .1, label = "Reduction in % of\nstudents at highest\nachievement levels",
  #   color = "black", size = 5
  # ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    title = element_text(size = 26),
    axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18),
    panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black"), axis.ticks = element_line()
  ) +
  labs(
    title = "Percent change in sectoral employment, west",
    x = "NAICS 2-digit code",
    y = "Change in Percent by industry"
  )

barplot_emp_west

# ggsave(
#   file = paste0(out_folder, "barplot_emp_west.png"),
#   width = 10,
#   height = 10
# )

out_east_emp<-tidy(feols(
  log_emp~i(naics2, drought_month12)| GeoFIPS^naics2 + naics2^year,
  data = filter(full_sector_df, west == 0 & naics2 != "92" & naics2 != "95" & naics2 != "99"),
  vcov = "conley"
))

out_east_emp$term<-c("11","21","22","23","31-33","42","44-45","48-49","51","52","53","54",
                     "55","56","61","62","71","72","81") #replace with naics codes

barplot_emp_east <- out_east_emp %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, color = "grey50", size = 1, linetype = "dashed") +
  geom_bar(position = position_dodge(), stat = "identity", fill = "blue", alpha = 0.4, width = .25) +
  geom_errorbar(
    position = position_dodge(), width = 0.125,
    aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error)
  ) +
  # annotate(
  #   geom = "text", x = 4.5, y = .1, label = "Reduction in % of\nstudents at highest\nachievement levels",
  #   color = "black", size = 5
  # ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    title = element_text(size = 26),
    axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18),
    panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black"), axis.ticks = element_line()
  ) +
  labs(
    title = "Percent change in sectoral employment, east",
    x = "NAICS 2-digit code",
    y = "Change in Percent by industry"
  )

# ggsave(
#   file = paste0(out_folder, "barplot_emp_east.png"),
#   width = 10,
#   height = 10
# )

barplot_emp_east

ggarrange(barplot_inc_west, barplot_inc_east, barplot_emp_west, barplot_emp_east, 
          ncol = 2, nrow = 2)

ggsave(
  file = paste0(out_folder, "combined.png"),
  width = 20,
  height = 20
)

mini<-main_sector_df %>% filter(year == 1979 | year == 2018) %>% 
   filter(naics2 == "22") %>% subset(select = c("year", "GeoFIPS", "emp", "population")) %>%
  mutate(year2 = 2*year) %>% pivot_wider(id_cols = GeoFIPS, names_from = year, values_from = c("population", "emp"))
mini$emp_diff<-mini$emp_2018 - mini$emp_1979
mini$pop_diff<-(mini$population_2018 - mini$population_1979)*1000
mini$comp_diff<-mini$emp_diff / mini$pop_diff

plot_df<-merge(county_v, mini, by.x="GEOID", by.y = "GeoFIPS")

plot(plot_df, "comp_diff", type="interval", breaks = c(-5000,0,5000),
     main = "Increases and decreases in employment in the utilities sector")

################################################################################
###### Regressions by industry, by precipitation: a first pass. ################
################################################################################


# Clear working directory
rm(list = ls())

setwd("C:\\Users\\anton\\Dropbox\\drought")
int_folder = ".\\data\\intermediate\\"
out_folder = ".\\output\\"

library(tidyverse)
library(tidyr)
library(dplyr)
library(fixest)
library(lubridate)
library(RColorBrewer)
library(knitr)
library(broom)
library(ggpubr)

#Get precipitation data
load(paste(int_folder,"precip_by_year.RData", sep = ""))

#Get sector-level data
load(paste(int_folder, "main_sector_df.RData", sep = ""))

#Get dataset, not by sector
load(paste(int_folder, "main_df.RData", sep = ""))

full_df<-merge(main_df, precip_by_year, by.x = c("GeoFIPS", "year"), by.y = c("GEOID", "year"), all = TRUE)

full_df$log_inc<-log(as.numeric(full_df$total_income)+1)
full_df$log_emp<-log(full_df$total_employment+1)
full_df$precip2<-full_df$precip^2
full_df<- full_df %>%
  group_by(GeoFIPS) %>%
  mutate(lag_precip = lag(precip, n=1), lag_precip2 = lag(precip2, n=1))

out1<-feols(log_emp~ precip + precip2 | GeoFIPS + year ,
            data = filter(full_df, west == 0),
            vcov = "conley"
)

out2<-feols(log_emp~ precip + precip2 | GeoFIPS + year ,
            data = filter(full_df, west == 1),
            vcov = "conley"
)

out3<-feols(log_inc~ precip + precip2 | GeoFIPS + year ,
            data = filter(full_df, west == 0),
            vcov = "conley"
)

out4<-feols(log_inc~ precip + precip2 | GeoFIPS + year ,
            data = filter(full_df, west == 1),
            vcov = "conley"
)

etable(out1, out2, out3, out4, tex = TRUE)

##################### BY SECTORS ###############################################

full_sector_df<-merge(main_sector_df, precip_by_year, by.x = c("GeoFIPS", "year"), by.y = c("GEOID", "year"), all = TRUE)

full_sector_df$log_inc<-log(as.numeric(full_sector_df$inc)+1)
full_sector_df$log_emp<-log(full_sector_df$emp+1)

out1<-feols(log_emp~ precip| GeoFIPS^naics2 + naics2^year ,
            data = filter(full_sector_df, west == 0),
            vcov = "conley"
)

out2<-feols(log_emp~ precip| GeoFIPS^naics2 + naics2^year ,
            data = filter(full_sector_df, west == 1),
            vcov = "conley"
)

out3<-feols(log_inc~ precip| GeoFIPS^naics2 + naics2^year ,
            data = filter(full_sector_df, west == 0),
            vcov = "conley"
)

out4<-feols(log_inc~ precip| GeoFIPS^naics2 + naics2^year ,
            data = filter(full_sector_df, west == 1),
            vcov = "conley"
)

etable(out1, out2, out3, out4)


############ MAKE PLOTS ########################################################

out_west_inc<-tidy(feols(
  log_inc~i(naics2, precip)| GeoFIPS^naics2 + naics2^year,
  data = filter(full_sector_df, west == 1),
  vcov = "conley"
))

out_west_inc$term<-c("11","21","22","23","31-33","42","44-45","48-49","51","52","53","54",
                     "55","56","61","62","71","72","81") #replace with naics codes

barplot_inc_west <- out_west_inc %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, color = "grey50", size = 1, linetype = "dashed") +
  geom_bar(position = position_dodge(), stat = "identity", fill = "maroon", alpha = 0.4, width = .25) +
  geom_errorbar(
    position = position_dodge(), width = 0.125,
    aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error)
  ) +
  # annotate(
  #   geom = "text", x = 4.5, y = .1, label = "Reduction in % of\nstudents at highest\nachievement levels",
  #   color = "black", size = 5
  # ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    title = element_text(size = 26),
    axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18),
    panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black"), axis.ticks = element_line()
  ) +
  labs(
    title = "Percent change in sectoral income, west",
    x = "NAICS 2-digit code",
    y = "Change in Percent by industry"
  )

barplot_inc_west

# ggsave(
#   file = paste0(out_folder, "barplot_inc_west.png"),
#   width = 10,
#   height = 10
# )

out_east_inc<-tidy(feols(
  log_inc~i(naics2, precip)| GeoFIPS^naics2 + naics2^year,
  data = filter(full_sector_df, west == 0),
  vcov = "conley"
))

out_east_inc$term<-c("11","21","22","23","31-33","42","44-45","48-49","51","52","53","54",
                     "55","56","61","62","71","72","81") #replace with naics codes

barplot_inc_east <- out_east_inc %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, color = "grey50", size = 1, linetype = "dashed") +
  geom_bar(position = position_dodge(), stat = "identity", fill = "blue", alpha = 0.4, width = .25) +
  geom_errorbar(
    position = position_dodge(), width = 0.125,
    aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error)
  ) +
  # annotate(
  #   geom = "text", x = 4.5, y = .1, label = "Reduction in % of\nstudents at highest\nachievement levels",
  #   color = "black", size = 5
  # ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    title = element_text(size = 26),
    axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18),
    panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black"), axis.ticks = element_line()
  ) +
  labs(
    title = "Percent change in sectoral income, east",
    x = "NAICS 2-digit code",
    y = "Change in Percent by industry"
  )

barplot_inc_east

# ggsave(
#   file = paste0(out_folder, "barplot_inc_east.png"),
#   width = 10,
#   height = 10
# )

######

out_west_emp<-tidy(feols(
  log_emp~i(naics2, precip)| GeoFIPS^naics2 + naics2^year,
  data = filter(full_sector_df, west == 1 & naics2 != "92" & naics2 != "95" & naics2 != "99"),
  vcov = "conley"
))

out_west_emp$term<-c("11","21","22","23","31-33","42","44-45","48-49","51","52","53","54",
                     "55","56","61","62","71","72","81") #replace with naics codes

barplot_emp_west <- out_west_emp %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, color = "grey50", size = 1, linetype = "dashed") +
  geom_bar(position = position_dodge(), stat = "identity", fill = "maroon", alpha = 0.4, width = .25) +
  geom_errorbar(
    position = position_dodge(), width = 0.125,
    aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error)
  ) +
  # annotate(
  #   geom = "text", x = 4.5, y = .1, label = "Reduction in % of\nstudents at highest\nachievement levels",
  #   color = "black", size = 5
  # ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    title = element_text(size = 26),
    axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18),
    panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black"), axis.ticks = element_line()
  ) +
  labs(
    title = "Percent change in sectoral employment, west",
    x = "NAICS 2-digit code",
    y = "Change in Percent by industry"
  )

barplot_emp_west

# ggsave(
#   file = paste0(out_folder, "barplot_emp_west.png"),
#   width = 10,
#   height = 10
# )

out_east_emp<-tidy(feols(
  log_emp~i(naics2, precip)| GeoFIPS^naics2 + naics2^year,
  data = filter(full_sector_df, west == 0 & naics2 != "92" & naics2 != "95" & naics2 != "99"),
  vcov = "conley"
))

out_east_emp$term<-c("11","21","22","23","31-33","42","44-45","48-49","51","52","53","54",
                     "55","56","61","62","71","72","81") #replace with naics codes

barplot_emp_east <- out_east_emp %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, color = "grey50", size = 1, linetype = "dashed") +
  geom_bar(position = position_dodge(), stat = "identity", fill = "blue", alpha = 0.4, width = .25) +
  geom_errorbar(
    position = position_dodge(), width = 0.125,
    aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error)
  ) +
  # annotate(
  #   geom = "text", x = 4.5, y = .1, label = "Reduction in % of\nstudents at highest\nachievement levels",
  #   color = "black", size = 5
  # ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    title = element_text(size = 26),
    axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18),
    panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black"), axis.ticks = element_line()
  ) +
  labs(
    title = "Percent change in sectoral employment, east",
    x = "NAICS 2-digit code",
    y = "Change in Percent by industry"
  )

# ggsave(
#   file = paste0(out_folder, "barplot_emp_east.png"),
#   width = 10,
#   height = 10
# )

barplot_emp_east

ggarrange(barplot_inc_west, barplot_inc_east, barplot_emp_west, barplot_emp_east, 
          ncol = 2, nrow = 2)

ggsave(
  file = paste0(out_folder, "combined.png"),
  width = 20,
  height = 20
)
