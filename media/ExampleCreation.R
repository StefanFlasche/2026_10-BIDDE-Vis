############
# script to produce examples for slides
# all examples share one theme, palette and size so the slide pairs read as
# "before" (soft red) vs "after" (soft green)
############

# libraries
library(tidyverse)

# shared look ------------------------------------------------------------------
col_bad  <- "#E88A85"   # soft red: the weaker version
col_good <- "#6DBE7A"   # soft green: the better version
col_warn <- "#C62828"   # highlight for what misleads
alpha_fill <- 0.6       # transparency for all data marks
tint <- function(col) colorRampPalette(c("white", col))(100)[round(alpha_fill * 100)]  # opaque look-alike of alpha

theme_slide <- function(base_size = 16) {
    theme_minimal(base_size = base_size) +
        theme(
            plot.title = element_text(face = "bold", size = rel(1.05)),
            plot.subtitle = element_text(colour = "grey35", margin = margin(b = 10)),
            plot.title.position = "plot",
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.title = element_text(colour = "grey30"),
            axis.text = element_text(colour = "grey20"),
            plot.background = element_rect(fill = "white", colour = NA),
            plot.margin = margin(14, 18, 10, 14)
        )
}

save_slide <- function(plot, file, width = 6, height = 4.5) {
    ggsave(file, plot, width = width, height = height, dpi = 200)
}


# white space: percentage motivation by day of the week -------------------------
whitespace_example <- tibble(
    Day = factor(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
                 levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")),
    Motivation = c(0.5, 0.6, 0.8, 0.5, 0.3)
)

# wasted space: thin bars, lots of empty canvas, labels crammed
p1 <- whitespace_example %>%
    ggplot(aes(x = Day, y = Motivation)) +
    geom_col(width = 0.3, fill = col_bad, alpha = alpha_fill) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(title = "Ink lost in empty space",
         subtitle = "Thin bars, a lot of canvas, little information",
         x = NULL, y = "Motivation") +
    theme_slide()

# compact: horizontal bars, direct labels, no axis needed
p2 <- whitespace_example %>%
    ggplot(aes(x = Motivation, y = fct_rev(Day))) +
    geom_col(width = 0.75, fill = col_good, alpha = alpha_fill) +
    geom_text(aes(label = scales::percent(Motivation)),
              hjust = 1.15, colour = "grey15", fontface = "bold", size = 5) +
    scale_x_continuous(limits = c(0, 0.8), expand = expansion(mult = c(0, 0.02))) +
    labs(title = "Space used for data",
         subtitle = "Wide bars, labels read directly",
         x = NULL, y = NULL) +
    theme_slide() +
    theme(axis.text.x = element_blank(), panel.grid = element_blank())

save_slide(p1, "media/whitespace_example1.png")
save_slide(p2, "media/whitespace_example2.png")


# log axis: relative risks are symmetric around 1 only on a log scale ------------
log_axis_example <- tibble(
    Exposure = factor(c("Third", "Full", "Triple"), levels = c("Third", "Full", "Triple")),
    RelativeRisk = c(1/3, 1, 3),
    RR_low = RelativeRisk * 0.7,
    RR_high = RelativeRisk * 1.3
)

log_base <- log_axis_example %>%
    ggplot(aes(x = Exposure, y = RelativeRisk)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
    labs(x = "Dose", y = "Relative risk") +
    theme_slide()

p3 <- log_base +
    geom_linerange(aes(ymin = RR_low, ymax = RR_high),
                   colour = col_bad, alpha = alpha_fill, linewidth = 1.5) +
    geom_point(shape = 21, size = 5, stroke = 1.2, colour = col_bad,
               fill = tint(col_bad)) +
    scale_y_continuous(breaks = c(0, 1, 2, 3, 4), limits = c(0, 4)) +
    labs(title = "Linear axis",
         subtitle = "1/3 looks close to 1, 3 looks far away")

p4 <- log_base +
    geom_linerange(aes(ymin = RR_low, ymax = RR_high),
                   colour = col_good, alpha = alpha_fill, linewidth = 1.5) +
    geom_point(shape = 21, size = 5, stroke = 1.2, colour = col_good,
               fill = tint(col_good)) +
    scale_y_log10(breaks = c(1/4, 1/3, 1/2, 1, 2, 3, 4),
                  labels = c("1/4", "1/3", "1/2", "1", "2", "3", "4"),
                  limits = c(1/5, 5)) +
    labs(title = "Log axis",
         subtitle = "1/3 and 3 are equally far from 1")

save_slide(p3, "media/log_axis_example1.png")
save_slide(p4, "media/log_axis_example2.png")


# zero in axis: a ~3% difference looks dramatic when the axis starts near the data
zero_axis_example <- tibble(
    Group = c("A", "B", "C"),
    Value = c(100, 102, 103)
)

p5 <- zero_axis_example %>%
    ggplot(aes(x = Group, y = Value)) +
    geom_col(width = 0.6, fill = col_bad, alpha = alpha_fill) +
    coord_cartesian(ylim = c(99, 103.5), expand = FALSE) +
    labs(title = "Axis starts at 99",
         subtitle = "C looks four times larger than A",
         x = NULL, y = "Value") +
    theme_slide() +
    theme(axis.text.y = element_text(colour = col_warn, face = "bold"))

p6 <- zero_axis_example %>%
    ggplot(aes(x = Group, y = Value)) +
    geom_col(width = 0.6, fill = col_good, alpha = alpha_fill) +
    geom_text(aes(label = Value), vjust = -0.5, size = 5, colour = "grey20") +
    scale_y_continuous(limits = c(0, 115), breaks = seq(0, 100, 25),
                       expand = expansion(mult = c(0, 0))) +
    labs(title = "Axis starts at 0",
         subtitle = "Values differ by only 3%",
         x = NULL, y = "Value") +
    theme_slide()

save_slide(p5, "media/zero_axis_example1.png")
save_slide(p6, "media/zero_axis_example2.png")


# uncertainty: four ways to show the same estimates and their uncertainty ------
uncertainty_example <- tibble(
    Country = factor(c("Canada", "Austria", "Belgium", "Peru"),
                     levels = rev(c("Canada", "Austria", "Belgium", "Peru"))),
    Estimate = c(0.17, 0.09, -0.06, -0.26),
    SE = c(0.055, 0.11, 0.09, 0.14)
) %>%
    mutate(lo95 = Estimate - 1.96 * SE, hi95 = Estimate + 1.96 * SE,
           lo50 = Estimate - 0.674 * SE, hi50 = Estimate + 0.674 * SE,
           row = as.numeric(Country))

# sampling distribution of each estimate on a grid
unc_density <- uncertainty_example %>%
    reframe(x = seq(-0.7, 0.5, length.out = 400), .by = c(Country, row, Estimate, SE)) %>%
    mutate(dens = dnorm(x, Estimate, SE))

unc_base <- ggplot(uncertainty_example) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    scale_x_continuous(limits = c(-0.7, 0.5), breaks = round(seq(-0.6, 0.4, 0.2), 1),
                       labels = scales::label_number(accuracy = 0.1)) +
    scale_y_continuous(breaks = 1:4, labels = levels(uncertainty_example$Country)) +
    labs(x = "Difference in mean rating", y = NULL) +
    theme_slide() +
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line())

estimate_mark <- geom_point(aes(x = Estimate, y = row), shape = 21, size = 4, stroke = 1.2,
                            colour = col_good, fill = tint(col_good))

# 1. point with nested intervals
p7 <- unc_base +
    geom_linerange(aes(xmin = lo95, xmax = hi95, y = row), colour = tint(col_good), linewidth = 1.2) +
    geom_linerange(aes(xmin = lo50, xmax = hi50, y = row), colour = tint(col_good), linewidth = 3.5) +
    estimate_mark +
    labs(title = "Interval",
         subtitle = "Thick: 50%, thin: 95% interval")

# 2. full distribution as a half-eye
p8 <- unc_base +
    geom_ribbon(data = unc_density,
                aes(x = x, ymin = row, ymax = row + 0.8 * dens / max(dens), group = Country),
                fill = col_good, alpha = alpha_fill) +
    geom_linerange(aes(xmin = lo95, xmax = hi95, y = row), colour = col_good, linewidth = 1) +
    estimate_mark +
    labs(title = "Distribution",
         subtitle = "The shape shows where values are likely")

# 3. gradient: ink fades with probability
p9 <- unc_base +
    geom_tile(data = unc_density,
              aes(x = x, y = row, alpha = 0.9 * dens / dnorm(0, 0, SE)),
              fill = col_good, height = 0.5, width = 1.2 / 400) +
    scale_alpha_identity() +
    geom_tile(aes(x = Estimate, y = row), width = 0.008, height = 0.6, fill = "grey20") +
    labs(title = "Gradient",
         subtitle = "Fades out where values become unlikely")

# 4. quantile dotplot: 20 equally likely outcomes per country
bin_w <- 0.04
unc_dots <- uncertainty_example %>%
    reframe(x = qnorm(ppoints(20), Estimate, SE), .by = c(Country, row)) %>%
    mutate(x = round(x / bin_w) * bin_w) %>%
    mutate(stack = row_number() - 1, .by = c(Country, x))
p10 <- unc_base +
    geom_point(data = unc_dots, aes(x = x, y = row + 0.12 * stack + 0.08),
               shape = 21, size = 2.6, colour = col_good, fill = tint(col_good)) +
    labs(title = "Quantile dots",
         subtitle = "Each dot is a 1 in 20 chance")

save_slide(p7, "media/uncertainty_example1.png")
save_slide(p8, "media/uncertainty_example2.png")
save_slide(p9, "media/uncertainty_example3.png")
save_slide(p10, "media/uncertainty_example4.png")


# multi panel: one figure, several plot types, shared theme and region colours ---
library(gridExtra)
set.seed(1)
regions <- c("North", "East", "South", "West")
col_region <- setNames(c("#6DBE7A", "#6FA8DC", "#F2B36B", "#B48CC8"), regions)
scale_region <- list(scale_colour_manual(values = col_region, guide = "none"),
                     scale_fill_manual(values = col_region, guide = "none"))

multipanel_example <- expand_grid(Region = factor(regions, levels = regions), Week = 1:20) %>%
    mutate(peak = c(North = 400, East = 250, South = 120, West = 40)[as.character(Region)],
           Cases = rpois(n(), peak * exp(-((Week - 10) / 3.5)^2) + 3))
region_summary <- multipanel_example %>%
    summarise(Cases = sum(Cases), .by = Region) %>%
    mutate(Population = c(60, 45, 30, 15)[as.integer(Region)] * 1000,
           AR = Cases / Population,
           lo = qbeta(0.025, Cases, Population - Cases + 1),
           hi = qbeta(0.975, Cases + 1, Population - Cases))
age_example <- expand_grid(Region = factor(regions, levels = regions),
                           Age = factor(c("0-4", "5-17", "18-64", "65+"),
                                        levels = c("0-4", "5-17", "18-64", "65+"))) %>%
    mutate(Share = c(0.35, 0.30, 0.20, 0.15, 0.25, 0.35, 0.25, 0.15,
                     0.30, 0.25, 0.30, 0.15, 0.20, 0.30, 0.30, 0.20))

theme_panel <- theme_slide(base_size = 13) +
    theme(plot.tag = element_text(face = "bold", size = 16),
          strip.text = element_text(face = "bold", hjust = 0),
          panel.spacing = unit(0.8, "lines"))

# A: the ggplot grid: one epidemic curve per region, shared axes
pA <- multipanel_example %>%
    ggplot(aes(Week, Cases, fill = Region)) +
    geom_col(alpha = alpha_fill) +
    facet_wrap(~Region, nrow = 1) +
    scale_region +
    labs(tag = "A", title = "Weekly cases", x = "Week", y = "Cases") +
    theme_panel

# B: cumulative cases, colours carry over so no legend needed
pB <- multipanel_example %>%
    mutate(Cumulative = cumsum(Cases), .by = Region) %>%
    ggplot(aes(Week, Cumulative, colour = Region)) +
    geom_line(linewidth = 1.3) +
    geom_text(data = ~ filter(.x, Week == max(Week)), aes(label = Region),
              hjust = -0.1, size = 4, fontface = "bold") +
    scale_x_continuous(limits = c(1, 25), breaks = seq(0, 20, 5)) +
    scale_region +
    labs(tag = "B", title = "Cumulative cases", x = "Week", y = NULL) +
    theme_panel

# C: attack rate with 95% interval
pC <- region_summary %>%
    ggplot(aes(x = AR, y = fct_rev(Region), colour = Region)) +
    geom_linerange(aes(xmin = lo, xmax = hi), linewidth = 1.2) +
    geom_point(aes(fill = Region), shape = 21, size = 4, stroke = 1.2, colour = "white") +
    scale_x_continuous(labels = scales::percent) +
    scale_region +
    labs(tag = "C", title = "Attack rate", x = NULL, y = NULL) +
    theme_panel +
    theme(panel.grid.major.x = element_line())

# D: age profile of cases
pD <- age_example %>%
    ggplot(aes(Age, Share, colour = Region, group = Region)) +
    geom_line(linewidth = 1.1, alpha = 0.8) +
    geom_point(size = 2.5) +
    scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
    scale_region +
    labs(tag = "D", title = "Cases by age", x = "Age group", y = NULL) +
    theme_panel

p11 <- arrangeGrob(pA, pB, pC, pD, layout_matrix = rbind(c(1, 1, 1), c(2, 3, 4)),
                   heights = c(1, 1.1))
ggsave("media/multipanel_example.png", p11, width = 12, height = 7, dpi = 200,
       bg = "white")
