library(rstanarm)

study <- data.frame(
	Hours=c(0.50,0.75,1.00,1.25,1.50,1.75,1.75,2.00,2.25,2.50,2.75,3.00,
			3.25,3.50,4.00,4.25,4.50,4.75,5.00,5.50),
	Pass=c(0,0,0,0,0,0,1,0,1,0,1,0,1,0,1,1,1,1,1,1)
)

t_prior <- student_t(df = 7, location = 0, scale = 2.5)

stan_out <- stan_glm(
	Pass ~ Hours,
	data = study,
	family = binomial(link = "logit"),
	# prior = t_prior,
	# prior_intercept = t_prior,
	seed = 2112,
	refresh = 0)

summary(stan_out)

plot(stan_out, plotfun = 'areas')

glm_out <- glm(
	Pass ~ Hours,
	data = study,
	family = binomial(link = 'logit')
)

summary(glm_out)

jtools::plot_summs(glm_out, plot.distributions = TRUE, inner_ci_level = .9, omit.coefs = NULL)
