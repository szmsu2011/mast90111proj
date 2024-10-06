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
span <- extendrange(train$lSalary, f = 0.1)
tibble(
  x = kd_rot$x,
  Normal = dnorm(x, mean(train$lSalary), sd(train$lSalary)),
  `KDE(ROT)` = kd_rot$y,
  `KDE(CV)` = kd_cv$y
) |>
  pivot_longer(!x, names_to = "type", values_to = "f") |>
  ggplot() +
  geom_line(aes(x, f, col = type)) +
  geom_point(aes(x, 0), tibble(x = test$lSalary), shape = 1) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "log(Salary)", y = expr(hat(f)), col = "")
