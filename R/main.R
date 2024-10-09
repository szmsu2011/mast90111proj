## ---- lib
library(tidyverse)
library(np)
library(mgcv)

## ---- data
data <- ISLR::Hitters |>
  mutate(lSalary = log(Salary)) |>
  select(lSalary, Hits, Years) |>
  drop_na()
set.seed(90111)
train <- slice_sample(data, n = 200)
test <- setdiff(data, train)

## ---- normal
muhat <- mean(train$lSalary)
sigma2hat <- var(train$lSalary)
## Test coverage of 50% confidence interval of prediction
sum(abs(test$lSalary - muhat) < qnorm(.75) * sqrt(sigma2hat)) |>
  binom.test(63)

## ---- kde
kd_rot <- density(train$lSalary, bw = "nrd0") # ROT bandwidth selection
kd_cv <- density(train$lSalary, bw = "ucv") # LOOCV bandwidth selection
## Test coverage of 50% confidence interval of prediction
cdf_rot <- cumsum(kd_rot$y / sum(kd_rot$y))
cdf_cv <- cumsum(kd_cv$y / sum(kd_cv$y))
sum(between(
  test$lSalary,
  kd_rot$x[which.min(abs(cdf_rot - .25))],
  kd_rot$x[which.min(abs(cdf_rot - .75))]
)) |>
  binom.test(63)
sum(between(
  test$lSalary,
  kd_cv$x[which.min(abs(cdf_cv - .25))],
  kd_cv$x[which.min(abs(cdf_cv - .75))]
)) |>
  binom.test(63)

## ---- univar-fit-plot
bind_rows(
  tibble(
    x = kd_cv$x, model = "Normal",
    f = dnorm(x, mean(train$lSalary), sd(train$lSalary))
  ),
  tibble(x = kd_rot$x, model = "KDE (ROT)", f = kd_rot$y),
  tibble(x = kd_cv$x, model = "KDE (LSCV)", f = kd_cv$y)
) |>
  ggplot() +
  geom_line(aes(x, f, col = model)) +
  geom_point(aes(x, 0), tibble(x = test$lSalary), shape = 1) +
  annotate("text", x = 5.93, y = 0, label = "Test data", vjust = -1) +
  theme_bw() +
  theme(legend.position = "top") +
  labs(x = "log(Salary)", y = expr(hat(f)), col = "Trained curves")

## ---- univar-repeat-eval
## Repeat evaluation 100 times
t(replicate(100, {
  train <- slice_sample(data, n = 200)
  test <- setdiff(data, train)
  muhat <- mean(train$lSalary)
  sigma2hat <- var(train$lSalary)
  nc <- mean(abs(test$lSalary - muhat) < qnorm(.75) * sqrt(sigma2hat))
  kd_rot <- density(train$lSalary, bw = "nrd0")
  kd_cv <- density(train$lSalary, bw = "ucv")
  cdf_rot <- cumsum(kd_rot$y / sum(kd_rot$y))
  cdf_cv <- cumsum(kd_cv$y / sum(kd_cv$y))
  kdrc <- mean(between(
    test$lSalary,
    kd_rot$x[which.min(abs(cdf_rot - .25))],
    kd_rot$x[which.min(abs(cdf_rot - .75))]
  ))
  kdcc <- mean(between(
    test$lSalary,
    kd_cv$x[which.min(abs(cdf_cv - .25))],
    kd_cv$x[which.min(abs(cdf_cv - .75))]
  ))
  c(Normal = nc, `KDE (ROT)` = kdrc, `KDE (LSCV)` = kdcc)
})) |>
  as_tibble(.names_repair = "minimal") |>
  pivot_longer(everything(), names_to = "model", values_to = "cov") |>
  ggplot(aes(fct_inorder(model), cov)) +
  geom_boxplot(outlier.shape = 1) +
  geom_hline(yintercept = 0.5, col = 3) +
  theme_bw() +
  labs(x = "Model", y = "Test coverage of 50% CI")

## ---- bivar-reg
## Linear regression
lr <- lm(lSalary ~ Hits, train)
## Local linear regression
ll <- npreg(lSalary ~ Hits, train, regtype = "ll")
## Penalised cubic spline regression
ps <- gam(lSalary ~ s(Hits, k = 35 + 2, bs = "cr"), data = train)

## ---- bivar-reg-fit-plot
bind_rows(
  tibble(mhat = fitted(lr), model = "Linear"),
  tibble(mhat = fitted(ll), model = "Local linear"),
  tibble(mhat = fitted(ps), model = "Penalised spline")
) |>
  mutate(Hits = rep(train$Hits, 3)) |>
  ggplot() +
  geom_line(aes(Hits, mhat, col = model)) +
  geom_point(aes(Hits, lSalary), test, shape = 1) +
  annotate("text", x = 150, y = 5, label = "Test data points") +
  theme_bw() +
  theme(legend.position = "top") +
  labs(x = "Hits", y = "log(Salary)", col = "Trained curves")

## ---- bivar-reg-eval
lr_pred <- predict(lr, list(Hits = test$Hits))
ll_pred <- predict(ll, newdata = list(Hits = test$Hits))
ps_pred <- predict(ps, list(Hits = test$Hits))
bivar_err <- tibble(
  Linear = (test$lSalary - lr_pred)^2,
  `Local linear` = (test$lSalary - ll_pred)^2,
  `Penalised spline` = (test$lSalary - ps_pred)^2
) |>
  pivot_longer(everything(), names_to = "model", values_to = "err")
t(summarise(group_by(bivar_err, model), `Test MSEP` = mean(err))) |>
  kable()

## ---- bivar-repeat-eval
## Repeat evaluation 100 times
p <- t(replicate(100, {
  train <- slice_sample(data, n = 200)
  test <- setdiff(data, train)
  lr <- lm(lSalary ~ Hits, train)
  ll <- npreg(lSalary ~ Hits, train, regtype = "ll")
  ps <- gam(lSalary ~ s(Hits, k = 35 + 2, bs = "cr"), data = train)
  lr_pred <- predict(lr, test)
  ll_pred <- predict(ll, newdata = test)
  ps_pred <- predict(ps, test)
  c(
    Linear = mean((test$lSalary - lr_pred)^2),
    `Local linear` = mean((test$lSalary - ll_pred)^2),
    `Penalised spline` = mean((test$lSalary - ps_pred)^2)
  )
})) |>
  as_tibble(.names_repair = "minimal") |>
  pivot_longer(everything(), names_to = "model", values_to = "MSEP") |>
  ggplot(aes(fct_inorder(model), MSEP)) +
  geom_boxplot(outlier.shape = 1) +
  theme_bw() +
  labs(x = "Model", y = "Test MSEP")

## ---- bivar-repeat-eval-plot
p

## ---- multivar-repeat-eval
single_index <- function(X, Y) {
  si <- function(beta_tail, X, Y, opt = TRUE, L = 10, fx = FALSE) {
    beta <- c(1, beta_tail)
    beta <- beta / sqrt(sum(beta^2))
    Xb <- X %*% beta
    fit <- gam(Y ~ s(Xb, bs = "cr", k = L + 2, fx = fx), method = "ML")
    if (opt) {
      fit$gcv.ubre
    } else {
      fit$beta <- beta
      fit
    }
  }
  f1 <- nlm(si, 20, X = X, Y = Y)
  si(f1$estimate, X, Y, opt = FALSE, L = 37)
}
t(replicate(100, {
  train <- slice_sample(data, n = 200)
  test <- setdiff(data, train)
  lr <- lm(lSalary ~ Hits + Years, train)
  si <- single_index(as.matrix(train[, -1]), train$lSalary)
  pl <- gam(
    lSalary ~ Hits + s(Years, bs = "cr"),
    data = train
  )
  am <- gam(
    lSalary ~ s(Hits, k = 27, bs = "cr") + s(Years, bs = "cr"),
    data = train
  )
  lr_pred <- predict(lr, test)
  si_pred <- predict(si, list(Xb = as.matrix(test[, -1]) %*% si$beta))
  pl_pred <- predict(pl, test)
  am_pred <- predict(am, test)
  c(
    Linear = mean((test$lSalary - lr_pred)^2),
    `Single index` = mean((test$lSalary - si_pred)^2),
    `Partial linear` = mean((test$lSalary - pl_pred)^2),
    Additive = mean((test$lSalary - am_pred)^2)
  )
})) |>
  as_tibble(.names_repair = "minimal") |>
  pivot_longer(everything(), names_to = "model", values_to = "MSEP") |>
  ggplot(aes(fct_inorder(model), MSEP)) +
  geom_boxplot(outlier.shape = 1) +
  theme_bw() +
  labs(x = "Model", y = "Test MSEP")
