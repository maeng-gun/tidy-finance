# 0. 패키지 로드 ####

library(tidyverse)
library(tidyquant)
library(scales)

# 1. 애플 주가 티블 ####
prices <- tq_get("AAPL",
                 get = "stock.prices",
                 from = "2000-01-01",
                 to = "2021-12-31"
)

# 2. 애플 가격 선그래프 ####
prices %>% 
  ggplot(aes(x=date, y=adjusted)) +
  geom_line() +
  labs(title = "2000년 이후 애플 주가")

# 3. 애플 일간수익률 ####
returns <- prices %>% 
  arrange(date) %>% 
  mutate(ret = adjusted / lag(adjusted) -1) %>% 
  select(symbol, date, ret) %>% 
  drop_na(ret)

# 4. 애플 수익률 히스토그램  ####
quantile_0.05 <- quantile(returns %>% pull(ret), prob=.05)
returns %>% 
  ggplot(aes(x=ret))+
  geom_histogram(bins=100)+
  geom_vline(xintercept = quantile_0.05, 
             linetype="dashed")+
  scale_x_continuous(labels=percent)

# 5. 요약통계량 ####
returns %>% 
  group_by(year=year(date)) %>% 
  summarise(across(
    ret,
    list(mean=mean, sd=sd, max=max, min=min),
    .names = "{.fn}_ret"
  ))

# 6. DOW 지수 구성종목들의 수익률 티블 ####
index_prices <- 
  tq_index('DOW') %>% 
  tq_get(get = "stock.prices",
         from = "2000-01-01",
         to = "2021-12-31")

index_prices <- readRDS('data/index_prices.Rdata')

index_prices <- 
  index_prices %>% 
  group_by(symbol) %>% 
  mutate(n = n()) %>% 
  ungroup(symbol) %>% 
  filter(n == max(n)) %>% 
  select(-n)

returns <- index_prices |>
  mutate(month = floor_date(date, "month")) |>
  group_by(symbol, month) |>
  summarize(price = last(adjusted), .groups = "drop_last") |>
  mutate(ret = price / lag(price) - 1) |>
  drop_na(ret) |>
  select(-price)

# 7. 수익률 행렬(행-날짜, 열-종목, 값-수익률) ####
returns_matrix <- returns %>% 
  pivot_wider(
    names_from = symbol,
    values_from = ret
  ) %>% 
  select(-month)

# 8. 최소분산 포트폴리오(MVP) 산출 ####
sigma <- cov(returns_matrix)
mu <- colMeans(returns_matrix)
N <- ncol(returns_matrix)
iota <- rep(1, N)
sigma_inv <- solve(sigma)

mvp_weights <- sigma_inv %*% iota
mvp_weights <- mvp_weights / sum(mvp_weights)

mvp_r_v <- tibble(
  average_ret = as.numeric(t(mvp_weights) %*% mu),
  volatility = as.numeric(sqrt(t(mvp_weights) %*% sigma %*% mvp_weights))
)

# 9. 효율적 프론티어 생성 ####
benchmark_multiple <- 3
mu_bar <- benchmark_multiple * mvp_r_v$average_ret

C <- as.numeric(t(iota) %*% sigma_inv %*% iota)
D <- as.numeric(t(iota) %*% sigma_inv %*% mu)
E <- as.numeric(t(mu) %*% sigma_inv %*% mu)
lambda_tilde <- as.numeric(2 * (mu_bar - D / C) / (E - D^2 / C))
efp_weights <- mvp_weights +
  lambda_tilde / 2 * (sigma_inv %*% mu - D * mvp_weights)

length_year <- 12
a <- seq(from = -0.4, to = 1.9, by = 0.01)

res <- tibble(
  a = a,
  mu = NA,
  sd = NA
)
for (i in seq_along(a)) {
  w <- (1 - a[i]) * mvp_weights + (a[i]) * efp_weights
  res$mu[i] <- length_year * t(w) %*% mu   
  res$sd[i] <- sqrt(length_year) * sqrt(t(w) %*% sigma %*% w)
}


res |>
  ggplot(aes(x = sd, y = mu)) +
  geom_point() +
  geom_point(
    data = res |> filter(a %in% c(0, 1)),
    size = 4
  ) +
  geom_point(
    data = tibble(
      mu = length_year * mu,       
      sd = sqrt(length_year) * sqrt(diag(sigma))
    ),
    aes(y = mu, x = sd), size = 1
  ) +
  labs(
    x = "Annualized standard deviation",
    y = "Annualized expected return",
    title = "Efficient frontier for DOW index constituents"
  ) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent)
