# remotes::install_github('jbryer/brickset')
library(brickset)
library(ggplot2)
data(legosets)


ggplot(legosets, aes(x=pieces, y=US_retailPrice, size=minifigs, color=availability)) +
	geom_point(alpha = 0.1)

sum_out <- psych::describeBy(legosets$US_retailPrice, group = legosets$availability, mat =TRUE)

ggplot(legosets, aes(x=availability, y=US_retailPrice)) +
	geom_boxplot() +
	geom_point(data = sum_out, aes(x = group1, y = mean), color = 'blue', size = 3)


ggplot(legosets, aes(x = US_retailPrice)) + geom_histogram()


x <- seq(0, 1000, by = 0.01)
# x

df <- data.frame(x = x, y = log(x))

ggplot(df, aes(x = x, y = y)) + geom_point()

ggplot(legosets, aes(x = pieces, y = US_retailPrice)) +
	geom_point()

ggplot(legosets, aes(x = pieces, y = US_retailPrice)) +
	geom_point(aes(color = availability))

ggplot(legosets, aes(x = pieces, y = US_retailPrice)) +
	geom_point()

ggplot(legosets, aes(x = pieces, y = US_retailPrice)) +
	geom_point(color = 'blue', size = 1)
