## Figures of Means (in Paper + Appendix, unordered)
## Stephanie Pedron (pedron.2@osu.edu)

rm(list = ls())

source("C:/Users/steph/Documents/Courses/PhD/Papers/RR Stuff/RR Codes/RR_datacleaner.r")


## FIGURE 1: Mean Racial Resentment by Race (2010-2022)
plot_data1_means <- aggregate(combined_data$racialresentment, by = list(combined_data$year, combined_data$race), 
                              FUN=mean, na.rm=T)
plot1_means <- ggplot(plot_data1_means, aes(x = Group.1, y = x, color = Group.2)) + geom_point() + geom_line() +
  labs(x = "Year", y = "Racial Resentment", title = "Figure 1.1: Mean Racial Resentment by Race (2010-2022)",
       col = "Race") + scale_x_continuous(labels = c("2010", "2012", "2014", "2018", "2020", "2022")) + theme_minimal()
plot1_means


## APPENDIX FIGURE: Mean Racial Resentment by Race and Nativity (Asians and Whites, 2010-2022)
plot_data2_means <- aggregate(combined_data$racialresentment, 
                              by = list(combined_data$year, combined_data$race, combined_data$immstat), 
                              FUN=mean, na.rm=T)
plot_data2_means <- plot_data2_means %>%
  select(Group.1, Group.2, Group.3, x) %>% 
  filter(Group.2 != "Latinx" & Group.2 != "Black") %>% 
  mutate(Group.3 = paste(Group.2, Group.3))

plot2_means <- ggplot(plot_data2_means, aes(x = Group.1, y = x, color = as.factor(Group.3))) + 
  geom_line() + geom_point() +
  labs(x = "Year", y = "Racial Resentment", title = "Mean Racial Resentment by Race and Nativity (2010-2022)",
       col = "Race & Nativity") + scale_color_manual(labels = c("US Born Asian", "Foreign Born Asian", 
                                                                "US Born White", "Foreign Born White"),
                                                     values = c("purple", "green", "red", "steelblue")) +
  scale_x_continuous(labels = c("2010", "2012", "2014", "2018", "2020", "2022")) + theme_minimal()
plot2_means


## FIGURE 2: Mean Racial Resentment by Nativity for Asians
plot_data3_means <- aggregate(combined_data$racialresentment, 
                              by = list(combined_data$year, combined_data$race, combined_data$immstat), 
                              FUN=mean, na.rm=T)
plot_data3_means <- plot_data3_means %>%
  select(Group.1, Group.2, Group.3, x) %>% 
  filter(Group.2 == "Asian")

plot3_means <- ggplot(plot_data3_means, aes(x = Group.1, y = x, color = as.factor(Group.3))) + 
  geom_line() + geom_point() + labs(x = "Year", y = "Racial Resentment", 
                                    title = "Figure 1.2: Racial Resentment by Asian Nativity (2010-2022)",
                                    col = "Nativity") + scale_color_manual(labels = c("US Born Asian", "Foreign Born Asian"), 
                                                                           values = c("purple", "green")) + 
  scale_x_continuous(labels = c("2010", "2012", "2014", "2018", "2020", "2022")) + theme_minimal()
plot3_means


## FIGURE 4: Mean Racial Resentment by Immigrant Generation (Asians)
plot_data5_means <- aggregate(combined_data$racialresentment, 
                              by = list(combined_data$year, combined_data$race, combined_data$immgen), 
                              FUN=mean, na.rm=T)
plot_data5_means <- plot_data5_means %>%
  select(Group.1, Group.2, Group.3, x) %>% 
  filter(Group.2 == "Asian")

plot5_means <- ggplot(plot_data5_means, aes(x = Group.1, y = x, color = as.factor(Group.3))) + 
  geom_line() + geom_point() + labs(x = "Year", y = "Racial Resentment", 
                                    title = "Figure 1.4: Racial Resentment by Immigrant Generation (2010-2022)",
                                    col = "Immigrant Generation\n(Asian Respondents)") + 
  scale_color_manual(labels = c("First Generation", "Second Generation", "Third Generation"), 
                     values = c("purple", "green", "red")) + 
  scale_x_continuous(labels = c("2010", "2012", "2014", "2018", "2020", "2022")) + theme_minimal()
plot5_means


## FIGURE 3: Mean Racial Resentment for Whites (Partisan) and Asians (Nativity)
plot_data4_means <- aggregate(combined_data$racialresentment,
                              by = list(combined_data$year, combined_data$race, combined_data$immstat,
                                        combined_data$pid7), FUN=mean, na.rm=T)
plot_data4_means$newvar <- 0

plot_data4_means <- plot_data4_means %>%
  select(Group.1, Group.2, Group.3, Group.4, x, newvar) %>% 
  mutate(newvar = case_when(
    Group.2 == "Asian" & Group.3 == "0" ~ "US Born Asian",
    Group.2 == "Asian" & Group.3 == "1" ~ "Foreign Born Asian",
    Group.2 == "White" & Group.4 == "0" ~ "White Democrat",
    Group.2 == "White" & Group.4 == "2"~ "White Republican",
    Group.2 == "White" & Group.4 == "1" ~ "White Independent")) %>%
  filter(Group.2 != "Black" & Group.2 != "Latinx")

library(collapse) # need this to get means of observations with the same newvar and year value
plot_data4_means <- collap(plot_data4_means, x ~ Group.1 + newvar, FUN = list(fmean))

plot4_means <- ggplot(plot_data4_means, aes(x = Group.1, y = x, color = newvar)) + geom_line() + geom_point() +
  labs(x = "Year", y = "Racial Resentment", 
       title = "Figure 1.3: Racial Resentment by Asian Nativity and White Partisanship (2010-2022)",
       col = "Legend") + scale_x_continuous(labels = c("2010", "2012", "2014", "2018", "2020", "2022")) + theme_minimal()
plot4_means


## Putting together data for extra plots
A <- subset(CCES10, select = c(year, race, immstat, immgen, pid7, ideo5))
B <- subset(CCES12, select = c(year, race, immstat, immgen, pid7, ideo5))
C <- subset(CCES14, select = c(year, race, immstat, immgen, pid7, ideo5))
D <- subset(CCES18, select = c(year, race, immstat, immgen, pid7, ideo5))
E <- subset(CCES20, select = c(year, race, immstat, immgen, pid7, ideo5))
G <- subset(CCES22, select = c(year, race, immstat, immgen, pid7, ideo5))
H <- subset(CCES16, select = c(year, race, immstat, immgen, pid7, ideo5))
combined_data <- rbind(A, B, C, D, E, G, H)
rm(A, B, C, D, E, G, H)


## APPENDIX FIGURE: Ideology by AAPI Immigrant Generation
plot_data6_means <- aggregate(combined_data$ideo5, 
                              by = list(combined_data$year, combined_data$race, combined_data$immgen), 
                              FUN=mean, na.rm=T)
plot_data6_means <- plot_data6_means %>%
  select(Group.1, Group.2, Group.3, x) %>% 
  filter(Group.2 == "Asian")

plot6_means <- ggplot(plot_data6_means, aes(x = Group.1, y = x, color = as.factor(Group.3))) + 
  geom_line() + geom_point() + labs(x = "Year", y = "Liberal (0) to Conservative (2)", 
                                    title = "Ideology by Immigrant Generation (2010-2022)",
                                    col = "Immigrant Generation\n(Asian Respondents)") + 
  scale_color_manual(labels = c("First Generation", "Second Generation","Third Generation"), 
                     values = c("purple", "green", "red")) + 
  scale_x_continuous(breaks = unique(plot_data6_means$Group.1),  # Use unique years in your data
                     labels = unique(plot_data6_means$Group.1)) + theme_minimal()
plot6_means


## APPENDIX FIGURE: Party ID by AAPI Immigrant Generation
plot_data7_means <- aggregate(combined_data$pid7, 
                              by = list(combined_data$year, combined_data$race, combined_data$immgen), 
                              FUN=mean, na.rm=T)
plot_data7_means <- plot_data7_means %>%
  select(Group.1, Group.2, Group.3, x) %>% 
  filter(Group.2 == "Asian")

plot7_means <- ggplot(plot_data7_means, aes(x = Group.1, y = x, color = as.factor(Group.3))) + 
  geom_line() + geom_point() + labs(x = "Year", y = "Democrat (0) to Republican (2)", 
                                    title = "PID Scale by Immigrant Generation (2010-2022)",
                                    col = "Immigrant Generation\n(Asian Respondents)") + 
  scale_color_manual(labels = c("First Generation", "Second Generation","Third Generation"), 
                     values = c("purple", "green", "red")) + 
  scale_x_continuous(breaks = unique(plot_data6_means$Group.1),  # Use unique years in your data
                     labels = unique(plot_data6_means$Group.1)) + theme_minimal()
plot7_means
