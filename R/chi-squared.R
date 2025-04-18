library(ggplot2)

#' Calculate p-value from a chi-squared test with varying sample sizes
#'
#' This algorithm will start with an initial sample size (`n_start`) and perform a chi-squared test
#' with a vector of counts equal to `n * probs`. This will repeat increasing the sample size by
#' `n_step` until the p-value from the chi-squared test is less than `p_stop`.
#'
#' @param vector of cell probabilities. The sum of the values must equal 1.
#' @param sig_level signicance level.
#' @param p_stop the p-value to stop estimating chi-squared tests.
#' @param max_n maximum n to attempt if `p_value` is never less than `p_stop`.
#' @param min_cell_size minimum size per cell to perform the chi-square test.
#' @param n_start the starting sample size.
#' @param n_step the increment for each iteration.
#' @return a data.frame with three columns: n (sample size), p_value, and sig (TRUE if p_value < sig_level).
chi_squared_power <- function(
		probs,
		sig_level = 0.05,
		p_stop = 0.01,
		max_n = 10000,
		min_cell_size = 10,
		n_start = 10,
		n_step = 10
) {
	if(sum(probs) != 1) { # Make sure the sum is equal to 1
		stop('The sum of the probabilities must equal 1.')
	}

	n <- n_start
	p_values <- numeric()
	repeat {
		x <- (probs * n) |> round()
		if(all(x > 10)) {
			cs <- chisq.test(x, rescale.p = TRUE, simulate.p.value = FALSE)
			p_values <- c(p_values, cs$p.value)
			if(cs$p.value < p_stop | n > max_n) {
				break;
			}
		} else {
			p_values <- c(p_values, NA)
		}
		n <- n + n_step
	}
	result <- data.frame(n = seq(10, length(p_values) * n_step, n_step),
						 p_value = p_values,
						 sig = p_values < sig_level)
	class(result) <- c('chisqpower', 'data.frame')
	attr(result, 'probs') <- probs
	attr(result, 'sig_level') <- sig_level
	attr(result, 'p_stop') <- p_stop
	attr(result, 'max_n') <- max_n
	attr(result, 'n_step') <- n_step
	return(result)
}

#' Plot the results of chi-squared power estimation
#'
#' @param x result of [chi_squared_power()].
#' @param digits number of digits to round to.
#' @parma ... currently not used.
#' @return a ggplot2 expression.
plot.chisqpower <- function(x, digits = 4, ...) {
	ggplot(x[!is.na(x$p_value),], aes(x = n, y = p_value)) +
		geom_segment(x = 0, xend = min(x[x$sig,]$n, na.rm = TRUE),
					 y = attr(x, 'sig_level'), yend = attr(x, 'sig_level')) +
		annotate(geom = 'text', x = 0, y =  attr(x, 'sig_level'),
				 label = paste0('p = ',  attr(x, 'sig_level')),
				 vjust = -1, hjust = 0) +
		geom_segment(x = min(x[x$sig,]$n, na.rm = TRUE), xend = min(x[x$sig,]$n, na.rm = TRUE),
					 y = attr(x, 'sig_level'), yend = 0) +
		annotate(geom = 'text', x = min(x[x$sig,]$n, na.rm = TRUE), y = 0,
				 label = paste0('n = ', prettyNum(min(x[x$sig,]$n, na.rm = TRUE), big.mark = ',')),
				 vjust = 0, hjust = -0.1) +
		geom_path(alpha = 0.7) +
		geom_point(aes(color = sig), size = 1) +
		scale_color_brewer(paste0('p < ', attr(x, 'sig_level')), type = 'qual', palette = 6) +
		theme_minimal() +
		ylab('p-value') + xlab('Sample Size') +
		ggtitle(paste0('Smallest n where p < ', attr(x, 'sig_level'), ': ',
					   prettyNum(min(x[x$sig,]$n, na.rm = TRUE), big.mark = ',')),
				subtitle = paste0('Probabilities: ', paste0(round(attr(x, 'probs'), digits = digits), collapse = ', ')))
}

# Select the proportions for each group. The length is arbitrary, however the sum needs to equal 1
csp1 <- chi_squared_power(probs =  c(.33, .25, .42))
csp1[csp1$sig,]$n |> min(na.rm = TRUE) # Minimal n that results in p < 0.05
plot(csp1)

csp2 <- chi_squared_power(probs = c(.33, .35, .32))
csp2[csp2$sig,]$n |> min(na.rm = TRUE) # Minimal n that results in p < 0.05
plot(csp2)

csp3 <- chi_squared_power(probs = c(.25, .25, .24, .26), max_n = 20000)
csp3[csp3$sig,]$n |> min(na.rm = TRUE) # Minimal n that results in p < 0.05
plot(csp3)

# From the chisq.test help documentation which was drawn from Agresti (2007, p. 39)
M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(gender = c("F", "M"),
					party = c("Democrat", "Independent", "Republican"))
M_prob <- M / sum(M) # Convert the counts to percentages
csp4 <- chi_squared_power(probs = M_prob)
plot(csp4)

sum(M) # study n
chisq.test(M)

##### What about t-test
se <- function(sd, n) {
	sd / sqrt(n)
}

es <- 0.01
n <- seq(10, 10000, by = 1)
se_out <- se(1, n)
n[(1.96 * se_out) < 0.05] |> min()

ggplot(data.frame(n = n, se = 1.96 * se_out), aes(x = n, y = se)) +
	geom_path() +
	theme_minimal()

tmp <- rnorm(100, mean = es, sd = 1)
t.test(tmp)
