setwd('Slides')

# library(servr)
# library(renderthis)
# ?servr

pak::pak('rstudio/pagedown')

pagedown::find_chrome()

renderthis::to_pdf("01-Intro_to_Course.html",
				   complex_slides = TRUE,
				   partial_slides = FALSE)

pagedown::chrome_print("01-Intro_to_Course.html", "01-Intro_to_Course.pdf", timeout = 240)
pagedown::chrome_print("02-Summarizing_Data.html", "02-Summarizing_Data.pdf", timeout = 240)

setwd('/Users/jasonbryer/Dropbox (Personal)/School/Teaching/DAV5300 2025 Spring/')
