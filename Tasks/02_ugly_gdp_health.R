# Task: Make this plot more visually appealing
#
# Below is a bare-bones ggplot comparing income and health across
# countries for a single year, with population and region also
# encoded in the plot. It works, but it is not presentation- or
# publication-ready: the axes hide most of the variation, the
# colors are hard to tell apart, points overlap heavily, and there
# is no informative title, labels or legend layout.
#
# Your job: apply the design principles from the "How visualize
# practically?" session (log axes, transparency, color consistency,
# labels/legends, simplicity, ...) to turn this into a figure you'd
# be happy to put in a paper or a slide.
#
# You may change anything: geoms, colors, theme, scales, labels,
# legend, annotations - as long as the plot still shows the same
# information (one point per country: income, health, population
# size, region).

library(ggplot2)
library(gapminder)

# --- Get one cross-section of the data -----------------------------

country_data <- subset(gapminder, year == 2007)

# --- The plot students will improve --------------------------------

p <- ggplot(country_data, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(pch = continent)) +
  labs(title = "chart", x = "x", y = "y")

p
