install.packages(c("devtools", "tidyverse", "knitr", "likert", "tm", "SnowballC",
				   "wordcloud", "RColorBrewer", "reshape2", "latex2exp", "psych",
				   "icons", "cowplot", "rmarkdown", "qrcode", "sysfonts",
				   "tidymodels", "ggfortify",
				   "hexSticker", "showtext"))
install.packages('pdftools')

remotes::install_github('jbryer/DATA606')
remotes::install_github('jbryer/VisualStats')
remotes::install_github("ropenscilabs/icon")
remotes::install_github("gadenbuie/countdown", subdir = "r")
icons::download_fontawesome()
remotes::install_github("thomasp85/patchwork")
remotes::install_github("gadenbuie/ggweekly")
remotes::install_github("jhelvy/renderthis")
remotes::install_github('rstudio/chromote')

source('config.R')

##### Course Logo ##############################################################
library(hexSticker)
library(showtext)
library(ggplot2)

course <- 'DAV 5300'
url <- paste0('https://', tolower(semester), year, '.dav5300.net')

sysfonts::font_add_google(name = "Open Sans", family = "opensans")

# Use Logo
# https://github.com/allisonhorst/stats-illustrations
p <- "website/static/images/CUNY_SPS_Logo.png"
# p <- "website/static/images/CUNY_SPS_Logo_Wide.png"
p <- 'website/static/images/cupcake.png'

# Or ggplot2
color2 <- '#893286'
color1 <- '#325A89'
color3 <- '#328935'
bg.color <- '#FFFFFF'

# Use Logo
# https://github.com/allisonhorst/stats-illustrations
# p <- "website/static/images/CUNY_SPS_Logo.png"
# # p <- "website/static/images/CUNY_SPS_Logo_Wide.png"
# p <- 'website/static/images/cupcake.png'

# Or ggplot2
# p <- ggplot(data = mtcars, aes(x = -1 * mpg, y = wt)) +
# 	geom_point(size = 1, color = color2, alpha = 0.75) +
# 	geom_smooth(formula = y ~ x, method = loess, size = 0.75, color = color1, se = FALSE) +
# 	# geom_smooth(formula = y ~ x, se = FALSE, method = lm, size = .5, color = color1) +
# 	theme_void() + theme_transparent()
# p

library(VisualStats)

mean_x <- 20
mean_y <- 40
sd_x <- 2
sd_y <- 3
n <- 30
rho <- 0.8
set.seed(2112)
df <- mvtnorm::rmvnorm(
	n = n,
	mean = c(mean_x, mean_y),
	sigma = matrix(c(sd_x^2, rho * (sd_x * sd_y),
					 rho * (sd_x * sd_y), sd_y^2), 2, 2)) |>
	as.data.frame() |>
	dplyr::rename(x = V1, y = V2) |>
	dplyr::mutate(x_deviation = x - mean(x),
				  y_deviation = y - mean(y),
				  cross_product = x_deviation * y_deviation)

p <- ggplot(df, aes(x = x, y = y)) +
	geom_point(color = color1, size = 0.75) +
	geom_smooth(color = color2) +
	theme_void()
p

out_file <- paste0(sub(' ', '', course), '-', sub(' ', '', semester), year, '.png')
sticker <- sticker(p,
				   package = paste0(course, ' ', semester, ' ', year),
				   p_x = 1, p_y = 1.45,
				   p_size = 12,
				   p_color = color1,
				   p_family = 'opensans',
				   white_around_sticker = FALSE,
				   s_x = 1, s_y = .85, s_width=1.5, s_height = 1,
				   h_color = color1,
				   h_fill = bg.color,
				   spotlight = FALSE,
				   url = url,
				   u_size = 4.7,
				   u_color = color2,
				   filename = out_file )

sticker

# Save a square version to use as an icon for Slack
ggsave(filename = paste0(sub(' ', '', course), '-', sub(' ', '', semester), '-square.png'),
	   plot = sticker,
	   width = 50.8, height = 50.8, units = 'mm', bg = 'transparent', dpi = 300)

# Copy file for use on the website
file.copy(out_file,
		  'website/images/course_logo.png',
		  overwrite = TRUE)

# Copy file for use on the slides
file.copy(out_file,
		  'slides/images/hex/DATA606.png',
		  overwrite = TRUE)

# Save Website icons
# Can create site favicon here using the square output: https://favicon.io/favicon-converter/
# To create a w x h image, use this formula: w * 300 / 600
# ggsave(filename = 'website/images/apple-touch-icon.png',
# 	   plot = sticker,
# 	   width = 50.8, height = 50.8, units = 'mm', dpi = 90)
#
# ggsave(filename = 'website/images/android-chrome-192x192.png',
# 	   plot = sticker,
# 	   width = 50.8, height = 50.8, units = 'mm', dpi = 96)
#
# ggsave(filename = 'website/images/android-chrome-512x512.png',
# 	   plot = sticker,
# 	   width = 50.8, height = 50.8, units = 'mm', dpi = 256)
#
# # NOTE: The favicon.io seems to do a better job creating the small images
# ggsave(filename = 'website/images/favicon-16x16.png',
# 	   plot = sticker,
# 	   width = 50.8, height = 50.8, units = 'mm', dpi = 8)
#
# ggsave(filename = 'website/images/favicon-32x32.png',
# 	   plot = sticker,
# 	   width = 50.8, height = 50.8, units = 'mm', dpi = 16)

##### Create images for meetups ################################################
meetups <- readxl::read_excel('Schedule.xlsx', sheet = 'Meetups')
for(i in seq_len(nrow(meetups))) {
	datestr <- meetups[i,]$Date |> as.character()
	meetup_image(
		title = meetups[i,]$Topic,
		date = format(meetups[i,]$Date, '%B %d, %Y'),
		out_file = paste0('website/posts/', datestr, '-', gsub(' ', '_', meetups[i,]$Topic), '.png')
	)
}

