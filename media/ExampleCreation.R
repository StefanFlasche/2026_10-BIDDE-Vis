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
    scale_x_continuous(labels = scales::percent, limits = c(0, 1),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(title = "Space used for data",
         subtitle = "Wide bars, labels read directly",
         x = "Motivation", y = NULL) +
    theme_slide() +
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line())

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


# transparency: overplotting hides structure, alpha reveals it --------------------
set.seed(2)
scatter_example <- bind_rows(
    tibble(x = rnorm(4500, 0, 1), y = 0.6 * x + rnorm(4500, 0, 0.8)),
    tibble(x = rnorm(500, 1.2, 0.25), y = rnorm(500, -1.2, 0.25))  # hidden cluster
)
scatter_base <- scatter_example %>%
    ggplot(aes(x, y)) +
    labs(x = "Exposure", y = "Outcome") +
    theme_slide() +
    theme(panel.grid.major.x = element_line(), axis.text = element_blank())

p14 <- scatter_base +
    geom_point(colour = col_good, size = 1.6, alpha = 0.08) +
    labs(title = "Transparent points", subtitle = "The dense core and a second cluster appear")

# many simulated epidemic trajectories
spaghetti_example <- expand_grid(run = 1:200, Day = 0:120) %>%
    mutate(R0 = rep(rlnorm(200, log(1.6), 0.12), each = 121),
           peak_day = rep(rnorm(200, 55, 6), each = 121),
           size = rep(rlnorm(200, log(300), 0.2), each = 121),
           Cases = size * exp(-((Day - peak_day) / (18 / R0 * 1.6))^2))
spag_base <- spaghetti_example %>%
    ggplot(aes(Day, Cases, group = run)) +
    labs(x = "Day", y = "Daily cases") +
    theme_slide() +
    theme(panel.grid.major.x = element_line())

p16 <- spag_base +
    geom_line(colour = col_good, linewidth = 0.6, alpha = 0.06) +
    labs(title = "200 model runs, transparent", subtitle = "Where most runs fall becomes visible")

# overlapping distributions: incubation periods of three pathogens
incubation_example <- tibble(
    Pathogen = factor(rep(c("Pathogen A", "Pathogen B", "Pathogen C"), c(800, 800, 800)),
                      levels = c("Pathogen A", "Pathogen B", "Pathogen C")),
    Days = c(rlnorm(800, log(4), 0.35), rlnorm(800, log(6), 0.3), rlnorm(800, log(5), 0.45))
)
p13 <- incubation_example %>%
    ggplot(aes(Days, fill = Pathogen, colour = Pathogen)) +
    geom_density(alpha = 0.35, linewidth = 0.8) +
    annotate("text", x = c(2.6, 6.9, 10.5), y = c(0.3, 0.2, 0.055),
             label = levels(incubation_example$Pathogen), colour = unname(col_region[1:3]),
             fontface = "bold", size = 4.5, hjust = c(1, 0, 0)) +
    scale_colour_manual(values = unname(col_region[1:3]), guide = "none") +
    scale_fill_manual(values = unname(col_region[1:3]), guide = "none") +
    scale_x_continuous(limits = c(0, 15)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(title = "Overlapping distributions", subtitle = "All three shapes stay visible where they overlap",
         x = "Incubation period (days)", y = NULL) +
    theme_slide() +
    theme(axis.text.y = element_blank())

# overlapping uncertainty ribbons: two intervention scenarios
scenario_example <- expand_grid(Scenario = c("No intervention", "School closure"), Day = 0:120) %>%
    mutate(peak = if_else(Scenario == "No intervention", 55, 70),
           size = if_else(Scenario == "No intervention", 300, 200),
           width = if_else(Scenario == "No intervention", 16, 22),
           Cases = size * exp(-((Day - peak) / width)^2),
           lo = Cases * 0.6, hi = Cases * 1.5)
p15 <- scenario_example %>%
    ggplot(aes(Day, Cases, colour = Scenario, fill = Scenario)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), colour = NA, alpha = 0.3) +
    geom_line(linewidth = 1.2) +
    annotate("text", x = c(30, 98), y = c(330, 250), label = c("No intervention", "School closure"),
             colour = unname(col_region[c(4, 2)]), fontface = "bold", size = 4.5) +
    scale_colour_manual(values = unname(col_region[c(4, 2)]), guide = "none") +
    scale_fill_manual(values = unname(col_region[c(4, 2)]), guide = "none") +
    labs(title = "Overlapping uncertainty", subtitle = "Both scenarios and their overlap stay readable",
         x = "Day", y = "Daily cases") +
    theme_slide() +
    theme(panel.grid.major.x = element_line())

save_slide(p14, "media/transparency_example1.png")
save_slide(p16, "media/transparency_example2.png")
save_slide(p13, "media/transparency_example3.png")
save_slide(p15, "media/transparency_example4.png")


# labels, headings, legends: same data, default vs annotated ----------------------
labels_data <- multipanel_example %>%
    transmute(rgn = Region, wk = Week + c(North = -2, East = 0, South = 1, West = 2)[as.character(Region)],
              inc_per_100k = Cases / c(North = 60, East = 45, South = 30, West = 15)[as.character(Region)] * 100) %>%
    filter(between(wk, 1, 20))

p17 <- labels_data %>%
    ggplot(aes(wk, inc_per_100k, colour = rgn)) +
    geom_line(linewidth = 1) +
    scale_colour_manual(values = c(North = "#E88A85", East = "#C9605B", South = "#F2B8B5", West = "#A94742")) +
    labs(title = "Plot 1") +
    theme_slide() +
    theme(plot.title = element_text(face = "plain"))

p18 <- labels_data %>%
    ggplot(aes(wk, inc_per_100k, colour = rgn)) +
    geom_line(linewidth = 1.2, alpha = 0.9) +
    geom_text(data = labels_data %>% slice_max(inc_per_100k, n = 1, by = rgn),
              aes(label = rgn), hjust = 0, nudge_x = 0.4, vjust = 0, fontface = "bold", size = 4.5) +
    scale_colour_manual(values = col_region, guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = "North peaked two weeks earlier",
         subtitle = "Weekly incidence by region",
         x = "Week of outbreak", y = "Cases per 100,000",
         caption = "Simulated surveillance data") +
    theme_slide() +
    theme(panel.grid.major.x = element_line(),
          plot.caption = element_text(colour = "grey45", size = rel(0.75)))

save_slide(p17, "media/labels_example1.png")
save_slide(p18, "media/labels_example2.png")


# simplicity: everything at once on a 3d globe -----------------------------------
set.seed(3)
n_nodes <- 150
to_xyz <- function(lat, lon) cbind(cos(lat) * cos(lon), cos(lat) * sin(lon), sin(lat))

# view: tilt the globe towards the viewer, then project orthographically (z = depth)
view_tilt <- 25 * pi / 180
project <- function(m) {
    y <- m[, 2] * cos(view_tilt) - m[, 3] * sin(view_tilt)
    z <- m[, 2] * sin(view_tilt) + m[, 3] * cos(view_tilt)
    tibble(px = m[, 1], py = z, depth = -y)   # depth > 0 faces the viewer
}

nodes <- tibble(id = 1:n_nodes,
                lat = asin(runif(n_nodes, -0.6, 0.95)),
                lon = runif(n_nodes, -pi, pi),
                weight = rlnorm(n_nodes, 0, 1))
node_xyz <- to_xyz(nodes$lat, nodes$lon)

# routes as great circles lifted off the surface, higher for longer routes
n_routes <- 3000
routes <- tibble(route = 1:n_routes,
                 from = sample(n_nodes, n_routes, replace = TRUE, prob = nodes$weight),
                 to = sample(n_nodes, n_routes, replace = TRUE, prob = nodes$weight)) %>%
    filter(from != to)
steps <- seq(0, 1, length.out = 40)
route_paths <- routes %>%
    reframe({
        a <- node_xyz[from, ]; b <- node_xyz[to, ]
        omega <- acos(pmin(1, sum(a * b)))
        pts <- t(sapply(steps, \(s) (sin((1 - s) * omega) * a + sin(s * omega) * b) / sin(omega)))
        pts <- pts * (1 + 0.35 * omega / pi * sin(pi * steps))
        project(pts) %>% mutate(step = steps)
    }, .by = route) %>%
    mutate(front = depth > 0 | px^2 + py^2 > 1)  # hidden only if behind the sphere

# sphere shading: stacked discs drifting towards the light source
shade <- tibble(k = seq(1, 0.02, length.out = 60)) %>%
    reframe(t = seq(0, 2 * pi, length.out = 120), .by = k) %>%
    mutate(x = k * cos(t) - 0.35 * (1 - k), y = k * sin(t) + 0.35 * (1 - k),
           layer = match(k, unique(k)),
           fill = colorRampPalette(c("#94A3B8", "#F8FAFC"))(60)[layer])

node_proj <- project(node_xyz) %>% filter(depth > 0)

p19 <- ggplot() +
    geom_path(data = filter(route_paths, !front), aes(px, py, group = route),
              colour = col_bad, alpha = 0.04, linewidth = 0.3) +
    geom_polygon(data = shade, aes(x, y, group = layer, fill = fill)) +
    scale_fill_identity() +
    geom_path(data = filter(route_paths, front), aes(px, py, group = route),
              colour = col_bad, alpha = 0.09, linewidth = 0.3) +
    geom_point(data = node_proj, aes(px, py), colour = col_warn, size = 0.9, alpha = 0.8) +
    coord_equal(xlim = c(-1.45, 1.45), ylim = c(-1.25, 1.3), clip = "off") +
    labs(title = "Everything at once",
         subtitle = "3,000 routes: impressive, but what is the message?") +
    theme_void(base_size = 16) +
    theme(plot.title = element_text(face = "bold", size = rel(1.05)),
          plot.subtitle = element_text(colour = "grey35", margin = margin(b = 10)),
          plot.title.position = "plot",
          plot.background = element_rect(fill = "white", colour = NA),
          plot.margin = margin(14, 18, 10, 14))

save_slide(p19, "media/simplicity_example.png", width = 6, height = 5.5)


# reference points: the same estimates against a -10 margin vs. against 0 -------
# values read off the published PCV20 vs PCV13 forest plot
reference_example <- tribble(
    ~Serotype, ~Est, ~lo, ~hi,
    "1", -1.4, -4.4, 1.4,    "3", -2.7, -5.8, 0.2,    "4", -2.3, -5.4, 0.6,
    "5", -5.0, -9.6, -0.8,   "6A", -8.1, -13.0, -3.9, "6B", -8.6, -13.8, -4.0,
    "7F", -3.2, -6.6, -0.2,  "9V", -2.7, -6.4, 0.3,   "14", -0.9, -4.4, 2.6,
    "18C", -2.3, -5.4, 0.6,  "19A", 0.0, -2.3, 2.2,   "19F", 0.0, -1.9, 1.8,
    "23F", -4.1, -9.4, 1.1,  "8", 6.0, 3.0, 10.0,     "10A", -33.7, -40.6, -26.7,
    "11A", 6.5, 3.6, 10.6,   "12F", -19.2, -25.8, -12.6, "15B", 5.6, 2.4, 9.7,
    "22F", 6.5, 3.6, 10.6,   "33F", 1.4, -3.0, 6.0
) %>%
    mutate(Group = if_else(row_number() <= 13, "In both vaccines", "PCV20 only"),
           Serotype = fct_rev(fct_inorder(Serotype)))
col_group <- c("In both vaccines" = "#6FA8DC", "PCV20 only" = "#F2B36B")

reference_plot <- function(ref, ref_label) {
    reference_example %>%
        ggplot(aes(Est, Serotype, colour = Group)) +
        geom_vline(xintercept = ref, linetype = "dashed", colour = "grey30", linewidth = 0.7) +
        annotate("text", x = ref + 0.8, y = 20.6, label = ref_label, hjust = 0,
                 colour = "grey30", size = 4) +
        geom_linerange(aes(xmin = lo, xmax = hi), linewidth = 1.2, alpha = 0.7) +
        geom_point(aes(fill = Group, shape = Group), size = 3.5, stroke = 1, colour = "white") +
        scale_colour_manual(values = col_group, guide = "none") +
        scale_fill_manual(values = col_group, name = NULL) +
        scale_shape_manual(values = c(21, 24), name = NULL) +
        scale_x_continuous(limits = c(-42, 12), breaks = seq(-40, 10, 10)) +
        scale_y_discrete(expand = expansion(add = c(0.6, 1.2))) +
        labs(x = "Difference in % responders (PCV20 - PCV13)", y = "Serotype") +
        theme_slide(base_size = 14) +
        theme(panel.grid.major.x = element_line(), panel.grid.major.y = element_blank(),
              legend.position = "bottom")
}

p21 <- reference_plot(-10, "Non-inferiority margin") +
    labs(title = "Reference at -10", subtitle = "Judged against the non-inferiority margin")
p22 <- reference_plot(0, "No difference") +
    labs(title = "Reference at 0", subtitle = "Most shared serotypes respond slightly less to PCV20")

save_slide(p21, "media/reference_example1.png", width = 6, height = 6.5)
save_slide(p22, "media/reference_example2.png", width = 6, height = 6.5)
