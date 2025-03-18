library(ggplot2)
data(mtcars)

lm_out <- lm(mpg ~ wt + cyl + hp, data =mtcars)
summary(lm_out)

anova(lm_out)

mtcars$mpg_predicted <- predict(lm_out)

ggplot(mtcars, aes(x = mpg, y = mpg_predicted)) +
	geom_point() +
	geom_smooth(method = 'lm', se = FALSE) +
	coord_equal()

cor(mtcars$mpg_predicted, mtcars$mpg)^2


lm(mpg ~ wt * cyl, data = mtcars) |> summary()
