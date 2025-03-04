library(ggplot2)
data(mtcars)

lm_out <- lm(mpg ~ wt, data = mtcars)
summary(lm_out)

ggplot(mtcars, aes(x = wt, y = mpg)) +
	geom_smooth(formula = y ~ x, method = 'lm', se = FALSE) +
	geom_point()

# ggplot(mtcars, aes(x = wt, y = mpg)) +
# 	geom_abline(intercept = 0, slope = lm_out$coefficients[2], color = 'red') +
# 	geom_smooth(formula = y ~ x, method = 'lm', se = FALSE) +
# 	geom_point() +
# 	ylim(c(-25, 25))

mtcars$mpg_z <- (mtcars$mpg - mean(mtcars$mpg)) / sd(mtcars$mpg)
mtcars$wt_z <- (mtcars$wt - mean(mtcars$wt)) / sd(mtcars$wt)

lm_out_z <- lm(mpg ~ wt_z, data = mtcars)
summary(lm_out_z)

ggplot(mtcars, aes(x = wt_z, y = mpg)) +
	geom_smooth(formula = y ~ x, method = 'lm', se = FALSE) +
	geom_point()

# Thinking about effect size
t_out <- t.test(mpg ~ am, data = mtcars)
t_out
unname(diff(t_out$estimate) / sd(mtcars$mpg)) # Cohen's d, i.e. effect size

lm(mpg ~ am, data = mtcars) |> summary()
# If we standarize our dependent variable, the coefficent for our "treatment" (i.e. am here), is
# the same as Cohen's d (i.e. effect size).
mtcars$mpg_z <- (mtcars$mpg - mean(mtcars$mpg)) / sd(mtcars$mpg)
lm(mpg_z ~ am, data = mtcars) |> summary()
# We can now ajust for other variables
lm(mpg_z ~ am + wt, data = mtcars) |> summary()



lm_out_am <- lm(mpg ~ factor(am), data = mtcars)
summary(lm_out_am)

ggplot(mtcars, aes(x = am, y = mpg)) +
	geom_smooth(formula = y ~ x, method = 'lm', se = FALSE) +
	geom_point()

# lm_out_am_z <- lm(mpg_z ~ factor(am), data = mtcars)
# summary(lm_out_am_z)
#
# ggplot(mtcars, aes(x = am, y = mpg_z)) +
# 	geom_smooth(formula = y ~ x, method = 'lm', se = FALSE) +
# 	geom_point()

mtcars$wt_centered <- mtcars$wt - mean(mtcars$wt)
lm(mpg ~ wt_centered, data = mtcars) |> summary()
ggplot(mtcars, aes(x = wt_centered, y = mpg)) +
	geom_smooth(method = 'lm', formula = y ~ x, se = FALSE) +
	geom_point()


lm(mpg_centered ~ wt_centered, data = mtcars) |> summary()
lm(mpg_centered ~ 0 + wt_centered, data = mtcars) |> summary()



mtcars$mpg_centered <- mtcars$mpg - mean(mtcars$mpg)
mean(mtcars$mpg_centered)

lm_out_am_centered <- lm(mpg_centered ~ factor(am), data = mtcars)
summary(lm_out_am_centered)

ggplot(mtcars, aes(x = am, y = mpg_centered)) +
	geom_smooth(formula = y ~ x, method = 'lm', se = FALSE) +
	geom_point()

ggplot(mtcars, aes(x = cyl, y = mpg)) +
	geom_segment(aes(x = 4, y = mean(mtcars[mtcars$cyl == 4,]$mpg),
				 xend = 6, yend = mean(mtcars[mtcars$cyl == 6,]$mpg))
				 ) +
	geom_segment(aes(x = 4, y = mean(mtcars[mtcars$cyl == 4,]$mpg),
					 xend = 8, yend = mean(mtcars[mtcars$cyl == 8,]$mpg))
	) +
	# geom_smooth(formula = y ~ x, method = 'lm', se = FALSE) +
	geom_point()

lm_no_intercept <- lm(mpg ~ 0 + wt, data = mtcars)
summary(lm_no_intercept)

ggplot(mtcars, aes(x = wt, y = mpg)) +
	geom_smooth(formula = y ~ 0 + x, method = 'lm', se = FALSE) +
	geom_point()


lm_out <- lm(mpg ~ wt + factor(cyl), data = mtcars)
summary(lm_out)
anova_out <- anova(lm_out)
anova_out
# Var = (x - mean(x))^2 / (n - 1) # The numerator is call the sum of squares
var(mtcars$mpg) * (nrow(mtcars) - 1)
total_ss_mpg <- sum((mtcars$mpg - mean(mtcars$mpg))^2)
total_ss_mpg

var_explained <- anova_out$`Sum Sq` / total_ss_mpg
var_explained
var_explained[-length(var_explained)] # Dropping off residuals
sum(var_explained[-length(var_explained)]) # this is the R^2

aov(mpg ~ factor(cyl), data = mtcars) |> summary()
psych::describeBy(mtcars$mpg, group = mtcars$cyl, mat = TRUE)


aov(mpg ~ factor(cyl) + wt, data = mtcars) |> summary()
psych::describeBy(mtcars$mpg, group = mtcars$cyl, mat = TRUE)

lm(mpg_z ~ wt_z, data = mtcars)
lm(mpg ~ wt, data = mtcars)
lm(mpg_z ~ wt, data = mtcars)


