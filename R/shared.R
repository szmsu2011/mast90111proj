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
