library(ggplot2)
library(reshape2)
library(VisualStats)

theme_set(theme_minimal())

# colors <- c('mean' = '#66c2a5', 'median' = '#8da0cb')
colors <- c('#66c2a5', '#8da0cb')

N <- 100000 # Population size
n <- 50     # Sample size
n_samples <- 1000 # Number of samples to draw to estimate the sampling distribution

# Pick a population
pop <- rnbinom(N, 10, .5)
pop <- runif(N, 0, 1)
pop <- rbeta(N, 5, 2)


ggplot(data.frame(x = pop), aes(x = x)) +
	geom_vline(xintercept = mean(pop), color = colors[1], linewidth = 1.25) +
	geom_vline(xintercept = median(pop), color = colors[2], linewidth = 1.25) +
	geom_density()

mean(pop)
median(pop)

##### Sampling Distribution
samp_dist_mean <- numeric(n_samples)
samp_dist_median <- numeric(n_samples)
for(i in 1:n_samples) {
	samp <- sample(pop, size = n)
	samp_dist_mean[i] <- mean(samp)
	samp_dist_median[i] <- median(samp)
}

ggplot(data = data.frame(x = samp_dist_mean, y = 0), mapping = aes(x = x, y = y)) +
	geom_dist(samp_dist_mean) +
	geom_rug(alpha = 0.1)


ggplot() +
	geom_dist(list(mean = samp_dist_mean, median = samp_dist_median), color = colors) +
	annotate(geom = 'point', x = mean(pop), y = 0, color = colors[1], fill = colors[1], size = 4, pch = 22) +
	annotate(geom = 'point', x = median(pop), y = 0, color = colors[2], fill = colors[2], size = 4, pch = 22) +
	ggtitle('Estimates of the sampling distributions')

mean(pop)
mean(samp_dist_mean)
diff(c(mean(pop), mean(samp_dist_mean)))

median(pop)
mean(samp_dist_median)
diff(c(median(pop), mean(samp_dist_median)))

##### Bootstrapping
samp <- sample(pop, size = n)

ggplot() +
	geom_dist(samp, cv = c(mean(samp) - 1.96 * se(samp), mean(samp) + 1.96 * se(samp))) +
	annotate(geom = 'point', x = mean(pop), y = 0, color = colors[1], fill = colors[1], size = 4, pch = 22) +
	ggtitle('Sample distribution', subtitle = 'Shaded area is the 95% confidence interval \nSquare point is the population mean')

mean(samp)
median(samp)
se(samp)

n_boot <- 1000
boot_dist_mean <- numeric(n_boot)
boot_dist_median <- numeric(n_boot)
for(i in 1:n_boot) {
	boot_samp <- sample(samp, size = length(samp), replace = TRUE)
	boot_dist_mean[i] <- mean(boot_samp)
	boot_dist_median[i] <- median(boot_samp)
}

# Median bootstrap distibution only
# ggplot() +
# 	geom_dist(boot_dist_median, color = colors[2], adjust = 1.5) +
# 	annotate(geom = 'point', x = median(pop), y = 0, color = colors[2], fill = colors[2], size = 4, pch = 22)

ggplot() +
	geom_dist(list(mean = boot_dist_mean, median = boot_dist_median), color = colors) +
	annotate(geom = 'point', x = mean(pop), y = 0, color = colors[1], fill = colors[1], size = 4, pch = 22) +
	annotate(geom = 'point', x = median(pop), y = 0, color = colors[2], fill = colors[2], size = 4, pch = 22) +
	ggtitle('Bootstrap distributions')

mean(pop)
mean(boot_dist_mean)

# Shapiro-Wilk’s method
shapiro.test(boot_dist_mean)
shapiro.test(boot_dist_median)

# Kolmogorov-Smirnov Tests
ks.test(boot_dist_mean, 'pnorm')
ks.test(boot_dist_median, 'pnorm')

# If skewness is less than -1 or greater than 1, the distribution is highly skewed.
# If skewness is between -1 and -0.5 or between 0.5 and 1, the distribution is moderately skewed.
# If skewness is between -0.5 and 0.5, the distribution is approximately symmetric.
psych::skew(boot_dist_mean)
psych::skew(boot_dist_median)

psych::kurtosi(boot_dist_mean)
psych::kurtosi(boot_dist_median)

median(pop)
median(samp_dist_median)
median(boot_dist_median)
mean(boot_dist_median)
