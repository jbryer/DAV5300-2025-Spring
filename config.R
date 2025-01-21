year <- 2025
semester <- 'Spring'
course <- 'DAV 5300'
description <- paste0(course, ' Computational Math and Statistics ', semester, ' ', year)
github_link <- "DAV5300-2025-Spring"
one_minute_paper <- 'https://forms.gle/sTwKB3HivjtbafBb7'
one_minute_paper_results <- 'https://docs.google.com/spreadsheets/d/17XMDW0tSl1Z74NsJpSzg9szCzLwooZZVk8qMa50nVAA/edit?resourcekey=&gid=1539979105#gid=1539979105'
formative_assessment <- 'https://forms.gle/9vM7GH8S4XjSXsPq7'
formative_assessmnet_results <- 'https://docs.google.com/spreadsheets/d/1McPzt1HZgZQUNxZPx4jxGFvG_gLURVML-8qu3LIXrko/edit?resourcekey=&gid=533072014#gid=533072014'
course_site <- paste0('https://', tolower(semester), year, '.dav5300.net')
slack_invite <- 'https://join.slack.com/t/dav5300spring2025/shared_invite/zt-2yja0zs7k-GCJTqNKlLNgYYmetdl~OMw'
slack_link <- 'https://dav5300spring2025.slack.com'

library("knitr")
library("tidyverse")
library("likert")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("VisualStats")
library("DATA606")
library("reshape2")
library("latex2exp")
library("psych")
library("cowplot")
library("rmarkdown")
library("qrcode")

color_palette <- c('#325A89', '#85878A', '#555150')

opts_chunk$set(digits = 3, width = 120)

knitr::opts_chunk$set(warning = FALSE, message = FALSE, error = FALSE,
					  fig.width = 12, fig.height=6, fig.align = 'center',
					  digits = 3)

# The following is to fix a DT::datatable issue with Xaringan
# https://github.com/yihui/xaringan/issues/293
options(htmltools.dir.version = FALSE, htmltools.preserve.raw = FALSE)

# This style was adapted from Max Kuhn: https://github.com/rstudio-conf-2020/applied-ml
# And Rstudio::conf 2020: https://github.com/rstudio-conf-2020/slide-templates/tree/master/xaringan
# This slide deck shows a lot of the features of Xaringan: https://www.kirenz.com/slides/xaringan-demo-slides.html

# To use, add this to the slide title:   `r I(hexes(c("DATA606")))`
# It will use images in the images/hex_stickers directory (i.e. the filename is the parameter)
hexes <- function(x) {
	x <- rev(sort(x))
	markup <- function(pkg) glue::glue('<img src="images/hex/{pkg}.png" class="title-hex">')
	res <- purrr::map_chr(x, markup)
	paste0(res, collapse = "")
}

printLaTeXFormula <- function(fit, digits=2) {
	vars <- all.vars(fit$terms)
	result <- paste0('\\hat{', vars[1], '} = ', prettyNum(fit$coefficients[[1]], digits=2))
	for(i in 2:length(vars)) {
		val <- fit$coefficients[[i]]
		result <- paste0(result, ifelse(val < 0, ' - ', ' + '),
						 prettyNum(abs(val), digits=digits),
						 ' ', names(fit$coefficients)[i])
	}
	return(result)
}

PlotDist <- function(alpha, from = -5, to = 5, n = 1000, filename = NULL,
					 alternative = c("two.tailed", "greater", "lesser"),
					 distribution = c("normal", "t", "F", "chisq", "binomial"),
					 colour = "black", fill = "skyblue2",
					 ...)
{
	alternative <- match.arg(alternative)
	alt.alpha <- switch(alternative, two.tailed = alpha/2, greater = alpha,
						lesser = alpha)
	MyDen <- switch(distribution, normal = dnorm, t = dt, F = df,
					chisq = dchisq, binomial = dbinom)
	MyDist <- switch(distribution, normal = qnorm, t = qt, F = qf,
					 chisq = qchisq, binomial = qbinom)
	crit.lower <- MyDist(p = alt.alpha, lower.tail = TRUE, ...)
	crit.upper <- MyDist(p = alt.alpha, lower.tail = FALSE, ...)
	cord.x1 <- c(from, seq(from = from, to = crit.lower, length.out = 100),
				 crit.lower)
	cord.y1 <- c(0, MyDen(x = seq(from = from, to = crit.lower,
								  length.out = 100), ...), 0)
	cord.x2 <- c(crit.upper, seq(from = crit.upper, to = to,
								 length.out = 100), to)
	cord.y2 <- c(0, MyDen(x = seq(from = crit.upper, to = to,
								  length.out = 100), ...), 0)
	if (!is.null(filename)) pdf(file = filename)
	curve(MyDen(x, ...), from = from, to = to, n = n, col = colour,
		  lty = 1, lwd = 2, ylab = "Density", xlab = "Values")
	if (!identical(alternative, "greater")) {
		polygon(x = cord.x1, y = cord.y1, col = fill)
	}
	if (!identical(alternative, "lesser")) {
		polygon(x = cord.x2, y = cord.y2, col = fill)
	}
	if (!is.null(filename)) dev.off()
}

ompWordCloud <- function(text) {
	if(length(text) < 1) {
		warning('No text to create a word cloud.')
		return()
	}
	docs <- Corpus(VectorSource(text))
	# Convert the text to lower case
	docs <- tm_map(docs, content_transformer(tolower))
	# Remove numbers
	docs <- tm_map(docs, removeNumbers)
	# Remove english common stopwords
	docs <- tm_map(docs, removeWords, stopwords("english"))
	# Remove your own stop word
	# specify your stopwords as a character vector
	docs <- tm_map(docs, removeWords, c("blabla1", "blabla2"))
	# Remove punctuations
	docs <- tm_map(docs, removePunctuation)
	# Eliminate extra white spaces
	docs <- tm_map(docs, stripWhitespace)
	dtm <- TermDocumentMatrix(docs)
	m <- as.matrix(dtm)
	v <- sort(rowSums(m),decreasing=TRUE)
	d <- data.frame(word = names(v),freq=v)
	head(d, 10)
	wordcloud(words = d$word, freq = d$freq, min.freq = 2,
			  max.words=200, random.order=FALSE, rot.per=0.35,
			  colors=brewer.pal(8, "Dark2"))
}

##### Meetup banner
meetup_image <- function(title,
						 date = format(Sys.Date(), '%B %d, %Y'),
						 url = paste0('https://', tolower(semester), year, '.dav5300.net'),
						 out_file) {
	# print(here::here())
	cuny_logo <- png::readPNG(paste0(here::here(), '/website/images/yeshiva_university_logo.png')) |>
		grid::rasterGrob(interpolate = TRUE)
	course_logo <- png::readPNG(paste0(here::here(), '/',
									   paste0(sub(' ', '', course), '-', semester, year, '.png'))) |>
		grid::rasterGrob(interpolate = TRUE)

	ggplot2::ggplot() +
		ggplot2::annotate(geom = 'text', label = likert:::label_wrap_mod(title, width = 20),
						  x = 0, y = 7.5, color = color_palette[1],
						  hjust = 0, size = 14, size.unit = 'pt', fontface = 'bold') +
		ggplot2::annotate(geom = 'text', label = date, x = 0, y = 5.5, color = color_palette[1],
						  hjust = 0, size = 10, size.unit = 'pt') +
		ggplot2::annotate(geom = 'text', label = url, x = 10, y = 0, color = color_palette[2],
						  hjust = 1, size = 6, size.unit = 'pt') +
		ggplot2::annotation_custom(cuny_logo, xmin = 0, xmax = 4, ymin = 0, ymax = 2) +
		ggplot2::annotation_custom(course_logo, xmin = 6, xmax = 10, ymin = 0, ymax = 10) +
		ggplot2::xlim(0, 10) + ggplot2::ylim(0, 10) +
		ggplot2::geom_point() +
		ggplot2::theme_void() +
		ggplot2::theme(plot.background = ggplot2::element_rect(fill = 'white'))
	ggplot2::ggsave(filename = out_file,
					width = 1280, height = 720, units = 'px', device = 'png')
}
