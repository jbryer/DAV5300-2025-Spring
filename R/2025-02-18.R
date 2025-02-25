library(ggplot2)
library(VisualStats)


##### Sampling distribution for a proportion
pop <- sample(c(0, 1), size = 100000, replace = TRUE, prob = c(.15, .85))
pop |> table() |> prop.table()

samp_dist_prop <- numeric(1000)
for(i in 1:length(samp_dist_prop)) {
	samp <- sample(pop, size = 67)
	samp_dist_prop[i] <- mean(samp)
}
ggplot() + geom_dist(samp_dist_prop)

mean(samp_dist_prop)
sd(samp_dist_prop)

se(samp)

##### Sampling distribution
pop <- runif(100000)

ggplot() + geom_dist(pop)

samp_dist_mean <- numeric(1000)
for(i in 1:length(samp_dist_mean)) {
	samp <- sample(pop, size = 30)
	samp_dist_mean[i] <- mean(samp)
}
ggplot() + geom_dist(samp_dist_mean)


samp <- sample(pop, size = 10)
mean(samp)
se(samp)

bootstrap_dist_prop <- numeric(1000)
for(i in 1:length(bootstrap_dist_prop)) {
	boot_samp <- sample(samp, size = length(samp), replace = TRUE)
	bootstrap_dist_prop[i] <- mean(boot_samp)
}

ggplot() + geom_dist(bootstrap_dist_prop)

#####

propA <- .55 # Proportion for group A
propB <- .56 # Proportion for group B
pop.n <- 100000 # Population size
sampleA.n <- 1000
sampleB.n <- 1000

replications <- 10

results <- data.frame(
	percent = rep(seq(0.01, 0.5, 0.01), each = replications),
	p_hat_pop = NA_real_,
	p_hat_popA = NA_real_,
	p_hat_popB = NA_real_,
	p_hat_A = NA_real_,
	p_hat_A_not_B = NA_real_,
	p_hat_B = NA_real_
)

for(i in seq_len(nrow(results))) {
	A.n <- pop.n * (1 - results[i,]$percent)
	B.n <- pop.n * results[i,]$percent

	pop <- data.frame(
		group = c(rep('A', A.n),
				  rep('B', B.n) ),
		response = c(
			sample(c(1,0),
				   size = A.n,
				   prob = c(propA, 1 - propA),
				   replace = TRUE),
			sample(c(1,0),
				   size = B.n,
				   prob = c(propB, 1 - propB),
				   replace = TRUE) )
	)

	tmp <- aggregate(pop$response, by = list(pop$group), FUN = mean)
	results[i,]$p_hat_pop <- mean(pop$response)
	results[i,]$p_hat_popA <- tmp[1,]$x
	results[i,]$p_hat_popB <- tmp[2,]$x

	sampA <- pop[sample(nrow(pop), size = sampleA.n),]
	sampB <- pop[sample(which(pop$group == 'B'), size = sampleB.n),]

	results[i,]$p_hat_A <- mean(sampA$response)
	results[i,]$p_hat_A_not_B <- mean(sampA[sampA$group == 'A',]$response)
	results[i,]$p_hat_B <- mean(sampB$response)
}

results.melt <- melt(results[,c(1,5,6,7)],
					 id.vars = 'percent',
					 value.name = 'p_hat')
ggplot(results.melt, aes(x = percent, y = p_hat, color = variable)) +
	geom_hline(yintercept = propA) +
	geom_hline(yintercept = propB) +
	geom_point(alpha = 0.2) +
	geom_smooth(se = TRUE, method = 'loess', formula = y ~ x) +
	scale_color_brewer('', palette = 2, type = 'qual') +
	xlab('Size of group B as a percentage of population')# +
	# ylab(TeX('$\\hat{p}$'))


##### Weldon's dice simulated
dice <- sample(1:6, size = 315672, replace = TRUE, prob = c(1, 1, 1, 1, 1.01, 1.01))
dice <- sample(1:6, size = 315672, replace = TRUE, prob = c(1, 1, 1, 1, 1, 1.02))
length(dice)
315672 / 6
dice |> table()

chisq.test(table(dice))

