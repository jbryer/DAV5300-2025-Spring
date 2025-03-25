library(ggplot2)

# Source: https://en.wikipedia.org/wiki/Logistic_regression
study <- data.frame(
	Hours=c(0.50,0.75,1.00,1.25,1.50,1.75,1.75,2.00,2.25,2.50,2.75,3.00,
			3.25,3.50,4.00,4.25,4.50,4.75,5.00,5.50),
	Pass=c(0,0,0,0,0,0,1,0,1,0,1,0,1,0,1,1,1,1,1,1)
)

# Estimate a linear regression
glm_out_normal <- glm(Pass ~ Hours, data  = study, family = gaussian()) # Note the family, this is the default
summary(glm_out_normal)
study$predict_normal <- predict(glm_out_normal)

ggplot(study, aes(x = Hours, y = Pass)) +
	geom_hline(yintercept = c(0, 1, 0.5), linetype = 2) +
	geom_smooth(method = 'glm', formula = y ~ x,
				method.args = list(family = gaussian()),
				se = FALSE, color = 'maroon', alpha = 0.5, linetype = 2, linewidth = 0.5) +
	geom_point() +
	geom_point(aes(y = predict_normal), color = 'maroon', size = 3, pch = 21) +
	theme_minimal()

# Estimate a logistic regression
glm_out_binomial <- glm(Pass ~ Hours, data = study, family = binomial(link = 'logit'))
summary(glm_out_binomial)
study$predict_binomial <- predict(glm_out_binomial, type = 'response')

ggplot(study, aes(x = Hours, y = Pass)) +
	geom_hline(yintercept = c(0, 1, 0.5), linetype = 2) +
	geom_smooth(method = 'glm', formula = y ~ x,
				method.args = list(family = binomial(link='logit')),
				se = FALSE, color = 'darkgreen', alpha = 0.5, linetype = 2, linewidth = 0.5) +
	geom_point() +
	geom_point(aes(y = predict_binomial), color = 'darkgreen', size = 3, pch = 21) +
	theme_minimal()

# Compare accuracy (assume cut point at 0.5)
table(study$predict_normal > 0.5, study$Pass) |> prop.table() # linear regression (i.e. normal or guassian)
table(study$predict_binomial > 0.5, study$Pass) |> prop.table() # logistic regression (i.e. binomial)

# Make new predictions
hours_studied <- 4
predict(glm_out_binomial, newdata = data.frame(Hours = hours_studied), type = 'response')

# Putting it together
ggplot(study, aes(x = Hours, y = Pass)) +
	geom_hline(yintercept = c(0, 1, 0.5), linetype = 2) +
	geom_smooth(method = 'glm', formula = y ~ x,
				method.args = list(family = gaussian()),
				se = FALSE, color = 'maroon', alpha = 0.5, linetype = 2, linewidth = 0.5) +
	geom_smooth(method = 'glm', formula = y ~ x,
				method.args = list(family = binomial(link='logit')),
				se = FALSE, color = 'darkgreen', alpha = 0.5, linetype = 2, linewidth = 0.5) +
	geom_point(size = 2) +
	geom_point(aes(y = predict_normal), color = 'maroon', size = 3, pch = 21) +
	geom_point(aes(y = predict_binomial), color = 'darkgreen', size = 3, pch = 21) +
	theme_minimal()
