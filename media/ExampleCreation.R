############
# script to produce examples for slides
############


# libraries
library(tidyverse)

# whitesace example, plot percentage motivation by day of the week
whitespace_example <- tibble(
    Day = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
    Motivation = c(0.5, 0.6, 0.8, 0.5, 0.3)
)
p1=whitespace_example %>%
    ggplot(aes(x = Day, y = Motivation)) +
    geom_bar(stat = "identity", width = 0.5) +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent)

p2=whitespace_example %>%
    ggplot(aes(x = Day, y = Motivation)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent) +
    coord_flip() 

ggsave("media/whitespace_example1.jpg", p1, width = 6, height = 4)
ggsave("media/whitespace_example2.jpg", p2, width = 6, height = 2)


# log axis example: plot two relative risk once with and once without log axis
log_axis_example <- tibble(
    Exposure = c("Third", "Full", "Triple"),
    RelativeRisk = c(0.33, 1, 3),
    RR_low = c(0.33, 1, 3)*0.7,
    RR_high = c(0.33, 1, 3)*1.3
)
p3=log_axis_example %>%
    ggplot(aes(x = Exposure, y = RelativeRisk)) +
    geom_pointrange(aes(ymin = RR_low, ymax = RR_high)) +
    theme_minimal() 
p4=log_axis_example %>%
    ggplot(aes(x = Exposure, y = RelativeRisk)) +
    geom_pointrange(aes(ymin = RR_low, ymax = RR_high)) +
    theme_minimal() +
    scale_y_log10()
ggsave("media/log_axis_example1.jpg", p3, width = 2, height = 4)
ggsave("media/log_axis_example2.jpg", p4, width = 2, height = 4)


# zero in axis. use barplots to show how a small difference can become a big one if 0 is not included in the axis
zero_axis_example <- tibble(
    Group = c("A", "B", "C"),
    Value = c(100, 102, 103),
    value_low = c(100, 102, 103)*0.7,
    value_high = c(100, 102, 103)*1.3
)
p5=zero_axis_example %>%
    ggplot(aes(x = Group, y = Value)) +
    geom_point(stat = "identity") +
    theme_minimal() +
    scale_y_continuous(limits = c(99, 104))
p6=zero_axis_example %>%
    ggplot(aes(x = Group, y = Value)) +
    geom_bar(stat = "identity", width = 0.5) +
    theme_minimal() 
ggsave("media/zero_axis_example1.jpg", p5, width = 4, height = 4)
ggsave("media/zero_axis_example2.jpg", p6, width = 4, height = 4)

