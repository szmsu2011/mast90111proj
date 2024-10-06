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
    x = kd_cv$x, type = "Normal",
    f = dnorm(x, mean(train$lSalary), sd(train$lSalary))
  ),
  tibble(x = kd_rot$x, type = "KDE (ROT)", f = kd_rot$y),
  tibble(x = kd_cv$x, type = "KDE (LSCV)", f = kd_cv$y)
) |>
  ggplot() +
  geom_line(aes(x, f, col = type)) +
  geom_point(aes(x, 0), tibble(x = test$lSalary), shape = 1) +
  annotate("text", x = 5.93, y = 0, label = "Test data", vjust = -1) +
  theme_bw() +
  theme(legend.position = "top") +
  labs(x = "log(Salary)", y = expr(hat(f)), col = "Trained curves")
