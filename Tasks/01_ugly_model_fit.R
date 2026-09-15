# Task: Make this plot more visually appealing
#
# Below is a bare-bones ggplot showing observed case counts (data,
# with 95% uncertainty intervals) against model predictions (also
# with 95% intervals) at the same checkpoints. It works, but it is
# not presentation- or publication-ready - and with both data and
# model shown as overlapping points and errorbars, it's hard to
# tell them apart.
#
# Your job: apply the design principles from the "How visualize
# practically?" session to turn this into a figure you'd be happy to put in a
# paper or a slide.
#
# You may change anything: geoms, colors, theme, labels, axis
# scales, legend, annotations - as long as the plot still shows
# the same information (data points, model fit, uncertainty).

library(ggplot2)

set.seed(42)

# --- Simulate a simple epidemic-like time series ------------------

days <- 0:60

# "True" underlying epidemic curve (gamma-shaped epidemic curve)
true_curve <- function(t) {
  peak <- 30
  width <- 12
  height <- 500
  height * exp(-((t - peak)^2) / (2 * width^2))
}

model_mean <- true_curve(days)

# Uncertainty grows slightly away from the data-rich middle of the outbreak
model_sd <- 15 + 0.15 * abs(days - 30)

# Only report the model prediction every few days, like a model that
# was fit and evaluated at discrete checkpoints
model_days <- seq(0, 60, by = 3)
model_mean_pts <- true_curve(model_days)
model_sd_pts <- 15 + 0.15 * abs(model_days - 30)

model_df <- data.frame(
  day = model_days,
  fit = model_mean_pts,
  lower = pmax(model_mean_pts - 1.96 * model_sd_pts, 0),
  upper = model_mean_pts + 1.96 * model_sd_pts
)

# Noisy observed case counts (Poisson noise around the true curve)
obs_days <- seq(0, 60, by = 3)
obs_cases <- rpois(length(obs_days), lambda = pmax(true_curve(obs_days), 1))

observed_df <- data.frame(
  day = obs_days,
  cases = obs_cases,
  # crude 95% uncertainty interval on the counts (Wald-type on Poisson)
  lower = pmax(obs_cases - 1.96 * sqrt(obs_cases), 0),
  upper = obs_cases + 1.96 * sqrt(obs_cases)
)

# --- The plot students will improve --------------------------------

p <- ggplot() +
  geom_errorbar(data = model_df, aes(x = day, ymin = lower, ymax = upper, color = "model")) +
  geom_point(data = model_df, aes(x = day, y = fit, color = "model"), shape = 15) +
  geom_errorbar(data = observed_df, aes(x = day, ymin = lower, ymax = upper, color = "data")) +
  geom_point(data = observed_df, aes(x = day, y = cases, color = "data")) +
  labs(title = "model vs data", x = "day", y = "cases", color = "")

p
