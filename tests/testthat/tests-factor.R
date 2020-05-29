context("Factors")

test_that("cplot(what='prediction') works with factors inside and outside formula", {
              
    library(MASS)
    data(mtcars)
    mtcars2 <- mtcars
    mtcars2$am <- as.factor(mtcars2$am)

    mod1 <- lm(vs ~ hp + wt + factor(am), data = mtcars)
    mod2 <- lm(vs ~ hp + wt + am, data = mtcars2)

    pred1 <- cplot(mod1, x = "hp", what = 'prediction', draw = FALSE)
    pred2 <- cplot(mod2, x = "hp", what = 'prediction', draw = FALSE)

    expect_equal(pred1, pred2)

})
