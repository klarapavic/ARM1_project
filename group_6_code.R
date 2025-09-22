
#packages
required_packages <- c(
  "tidyverse", 
  "zoo",
  "dplyr",
  "quantmod",
  "ggplot2",
  "knitr",
  "kableExtra",
  "lubridate",
  "broom",
  "MASS"
)

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

invisible(lapply(required_packages, library, character.only = TRUE))


##### ----- Load Factor Data ----- #####
load("group_6_data.RData")
#dtv <- read.csv("dolvol.csv", stringsAsFactors = FALSE)[, c("date", "ret")]
#noa <- read.csv("noa.csv", stringsAsFactors = FALSE)[, c("date", "ret")]
#prc <- read.csv("prc.csv", stringsAsFactors = FALSE)[, c("date", "ret")]

names(dtv) <- c("date", "dtv")
names(noa) <- c("date", "noa")
names(prc) <- c("date", "prc")

dtv$date <- as.Date(dtv$date)
noa$date <- as.Date(noa$date)
prc$date <- as.Date(prc$date)

factors_tmp <- merge(dtv, noa, by = "date")
factors <- merge(factors_tmp, prc, by = "date")

###### ----- Load Fama-French Data ----- #####

#ff <- read.csv("FF.csv", stringsAsFactors = FALSE)
ff$RF <- as.numeric(ff$RF) / 100
ff$Mkt_RF <- as.numeric(ff$Mkt.RF) / 100
ff$market_total <- ff$RF + ff$Mkt_RF
# NOTE: NOA data starts from 1950s
ff$date <- seq(as.Date("1926-07-31"), by = "month", length.out = nrow(ff))

# Subset needed cols
ff_df <- ff[, c("date", "RF", "Mkt_RF", "market_total")]

# Merge Factors with Fama-French Data --> df_ff (older data incl.)

df_ff <- merge(factors, ff_df, by = "date")

###### ----- Get SP500 (SPY ETF) & Compute Excess Returns ----- #####

getSymbols("SPY", src = "yahoo", from = "1950-11-01", auto.assign = TRUE)
spy_monthly <- to.monthly(SPY, indexAt = "lastof", drop.time = TRUE)
spy_ret <- monthlyReturn(Cl(spy_monthly))  # monthly total return (not excess)

spy_df <- data.frame(date = index(spy_ret), monthly.returns = coredata(spy_ret))
spy_df$date <- as.Date(spy_df$date)

# Merge SP500 with FF RF to get excess returns
spy_excess_df <- merge(spy_df, ff_df[, c("date", "RF")], by = "date") %>%
  mutate(sp500 = monthly.returns - RF)  # excess return 

### Merge Factors + SPY + RF --> df_spy (post-1993)

df_spy <- merge(factors, spy_excess_df[, c("date", "sp500", "RF")], by = "date")

### Two Final DataFrames

# df_ff  --> factors + FF market return + RF (starts 1930s or earlier)
# df_spy --> factors + SP500 excess return + RF (starts 1993)

head(df_ff)
head(df_spy)


#### ----- Correlations and Interactions ----- ####

# Drop NA rows and keep overlapping sample
factors_clean <- factors %>%
  dplyr::select(date, dtv, noa, prc) %>%
  drop_na()

par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))  # adjust margins for clarity

# Plot 1: DTV vs NOA
plot(factors_clean$dtv, factors_clean$noa,
     main = "DTV vs NOA",
     xlab = "DTV", ylab = "NOA",
     pch = 20, col = "black")
abline(lm(noa ~ dtv, data = factors_clean), col = "grey", lwd = 2)

# Weak negative relationship between trading activity (DTV) and operational leverage (NOA)
# Suggests that firms with bloated operating assets are not necessarily more traded or visible
# May indicate investor neglect of inefficient firms, consistent with the "overlooked anomalies" narrative


# Plot 2: DTV vs PPS
plot(factors_clean$dtv, factors_clean$prc,
     main = "DTV vs PPS",
     xlab = "DTV", ylab = "PPS",
     pch = 20, col = "black")
abline(lm(prc ~ dtv, data = factors_clean), col = "grey", lwd = 2)

# Strong positive relationship between DTV and PPS
# Indicates that high-priced stocks tend to trade more in dollar terms — likely large-cap, institutionally held firms
# Reinforces the idea that price level and liquidity are intertwined via market structure and investor clientele


# Plot 3: NOA vs PPS
plot(factors_clean$noa, factors_clean$prc,
     main = "NOA vs PPS",
     xlab = "NOA", ylab = "PPS",
     pch = 20, col = "black")
abline(lm(prc ~ noa, data = factors_clean), col = "grey", lwd = 2)

# Slight negative relationship between NOA and PPS
# High NOA firms tend to have lower prices — consistent with market discounting overinvestment or inefficiency
# Suggests pricing reflects balance sheet quality, supporting the underreaction hypothesis (Hirshleifer et al. 2004)


#### ----- Rolling Window Analysis ----- #####

##### ----- Basic Summary Stats ----- ####

window <- 36 #enough data for relevant analysis, but doesn't create noise. Stretegies often evaluated at 3y window.
pub_date_dtv <- as.Date("1998-08-01")
pub_date_prc <- as.Date("2013-04-01")
pub_date_noa <- as.Date("2004-12-01")

# rolling stats function

compute_rolling_stats <- function(df, factor_col, pub_date, window = 36) {
  df <- df %>%
    arrange(date) %>%
    mutate(
      ret = .data[[factor_col]],  # Extract the right column by name
      roll_mean   = rollapply(ret, width = window, FUN = mean, align = "right", fill = NA),
      roll_sd     = rollapply(ret, width = window, FUN = sd, align = "right", fill = NA),
      roll_sharpe = roll_mean / roll_sd,
      period = if_else(date < pub_date, "Pre-Publication", "Post-Publication")
    )
  
  summary_stats <- df %>%
    group_by(period) %>%
    summarise(
      avg_return = mean(ret, na.rm = TRUE),
      volatility = sd(ret, na.rm = TRUE),
      sharpe = avg_return / volatility,
      n_obs = n(),
      .groups = "drop"
    )
  
  list(rolling = df, summary = summary_stats)
}

# Dollar Trading Volume
dtv_result <- compute_rolling_stats(df_ff, "dtv", pub_date_dtv, window)

# Net Operating Assets
noa_result <- compute_rolling_stats(df_ff, "noa", pub_date_noa, window)

# Price per Share
prc_result <- compute_rolling_stats(df_ff, "prc", pub_date_prc, window)

# print summary stats
print(dtv_result$summary)
print(noa_result$summary)
print(prc_result$summary)

# Pretty table
performance_table <- rbind(
  cbind(Factor = "DTV", dtv_result$summary),
  cbind(Factor = "NOA", noa_result$summary),
  cbind(Factor = "PPS", prc_result$summary)
)

performance_table %>%
  kable(digits = 4, caption = "Pre- vs. Post-Publication Factor Performance",
        col.names = c("Factor", "Period", "Avg. Return", "Volatility", "Sharpe", "Obs.")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, position = "center")

###### --- Interpretation of Pre/Post-Publication Factor Performance ---- ####

# DTV
# Before publication: Slightly positive returns but low Sharpe; consistent with the liquidity premium hypothesis (Datar et al., 1998).
# After publication: Factor collapses into negative territory. The negative Sharpe ratio suggests liquidity-based strategies may have been arbitraged away or crowded out by institutional investors.
# Conclusion: DTV has lost its risk-adjusted value and may no longer justify its inclusion in modern models.

# NOA
# Before publication: Strong performance, both in absolute and risk-adjusted terms. The Sharpe ratio of 0.31 is meaningful.
# After publication: Still positive performance, but Sharpe drops to 0.20 — suggesting partial decay, possibly due to increased investor awareness or exploitation.
# Conclusion: NOA remains a robust factor, but its alpha has decreased, likely due to partial arbitrage or better information dissemination.

# PPS
# Before publication: Mildly useful, perhaps reflecting retail investor sentiment and lottery stock preferences (Han & Kumar, 2013).
# After publication: Sharply negative performance. The –0.44% avg. return and –0.15 Sharpe imply that the factor now represents negative alpha, possibly due to noise trading, regulation changes, or structural market shifts.
# Conclusion: PPS is now a destructive signal. It may function as a contrarian indicator or simply reflect investor biases that no longer deliver excess returns.

# Factor Decay
# --> Both DTV and PPS exhibit substantial performance deterioration after publication.
# --> Negative Sharpe ratios post-2010 suggest erosion of return premia and/or arbitrage saturation.

# NOA Resilience
# --> NOA remains a relatively strong factor with consistently positive returns.
# --> While Sharpe has declined, the factor still provides useful risk-adjusted return.
# --> Implies slower arbitrage of accounting-based mispricing.

# Publication Effect
# --> Results support academic literature suggesting factor alpha declines after discovery.
# --> Investors should be cautious about relying on historically strong backtests.

# Practical Implication
# --> Active managers relying on DTV or PPS factors should reconsider exposure.
# --> Possible strategies: rotation into more robust factors, dynamic weighting, or hedging.

# NOTE: These interpretations align with rolling Sharpe plots, cumulative return curves, and cross-factor analysis.


# Addining another risk metric: Information Ratio
# What is the Information Ratio?
# IR = Avg. Active Return / Tracking Error

# Active Return = Factor return – Benchmark return (S&P 500 here)
# Tracking Error = Standard deviation of active return
# IR > 0: Factor outperformed the benchmark on a risk-adjusted basis
# IR < 0: Factor underperformed, adjusted for how volatile that underperformance was

### Function to compute IR stats by period

compute_ir_summary <- function(df, factor_col, benchmark_col = "sp500", pub_date = as.Date("2010-01-01"), window = 36) {
  df <- df %>%
    arrange(date) %>%
    mutate(
      factor_ret = .data[[factor_col]],
      benchmark_ret = .data[[benchmark_col]],
      active_ret = factor_ret - benchmark_ret,
      roll_active_mean = rollapply(active_ret, window, mean, fill = NA, align = "right"),
      roll_active_sd   = rollapply(active_ret, window, sd, fill = NA, align = "right"),
      roll_ir = roll_active_mean / roll_active_sd,
      period = if_else(date < pub_date, "Pre-Publication", "Post-Publication")
    )
  
  summary_df <- df %>%
    group_by(period) %>%
    summarise(
      avg_active_ret = mean(active_ret, na.rm = TRUE),
      tracking_error = sd(active_ret, na.rm = TRUE),
      information_ratio = avg_active_ret / tracking_error,
      n_obs = n(),
      .groups = "drop"
    )
  
  return(summary_df)
}

# Run for all three factors
ir_dtv <- compute_ir_summary(df_spy, "dtv")
ir_noa <- compute_ir_summary(df_spy, "noa")
ir_prc <- compute_ir_summary(df_spy, "prc")

ir_table <- rbind(
  cbind(Factor = "DTV", ir_dtv),
  cbind(Factor = "NOA", ir_noa),
  cbind(Factor = "PPS", ir_prc)
)

# Display pretty table
ir_table %>%
  kable(digits = 4, caption = "Pre- vs. Post-Publication Information Ratio (IR) by Factor",
        col.names = c("Factor", "Period", "Avg. Active Return", "Tracking Error", "Information Ratio", "Observations")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE, position = "center")

# DTV: 
# The factor’s economic value as a proxy for liquidity premium seems arbitraged away post-2010. IR confirms that the underperformance is systematic, not random.
# Pre-Publication	(-0.0546)	--> Slight underperformance vs. S&P 500, but with relatively low volatility — not a strong negative signal.
# Post-Publication	(-0.2761)	--> Clear deterioration — the factor generates strong negative alpha with moderately high tracking error.

# NOA 
# remains more resilient than DTV or PPS
# Pre-Publication  (+0.0493) --> Modest outperformance vs. S&P 500 with stable tracking error.
#                            --> Suggests persistent accounting-based anomaly with risk-adjusted benefits.
# Post-Publication (–0.1248) --> Noticeable deterioration in relative performance.
#                            --> IR still better than DTV/PPS, suggesting partial rather than full arbitrage.

# PPS
# appears highly fragile and sentiment-driven, fails to produce alpha pre- or post-publication.
# Pre-Publication  (–0.0025) --> Essentially no signal; performance indistinguishable from noise.
#                            --> May have reflected random variation or unexploited inefficiency.
# Post-Publication (–0.3314) --> Significant negative alpha with notable tracking error.
#                            --> Likely reflects structural changes (e.g. retail behavior, regulation).
#                            --> Possibly a contrarian signal or outdated anomaly.


###### ------ Final Table All Stats ------- ####

combined_table <- performance_table %>%
  inner_join(ir_table, by = c("Factor", "period")) %>%
  dplyr::select(Factor, period,
         avg_return, volatility, sharpe, 
         avg_active_ret, tracking_error, information_ratio,
         n_obs.x) %>%
  rename(
    Period = period,
    `Avg. Return` = avg_return,
    Volatility = volatility,
    `Sharpe Ratio` = sharpe,
    `Avg. Active Return` = avg_active_ret,
    `Tracking Error` = tracking_error,
    `Information Ratio` = information_ratio,
    Observations = n_obs.x
  )

# Display table
combined_table %>%
  kable(digits = 4, caption = "Risk & Performance Metrics: Avg. Returns, Volatility, Sharpe and Information Ratio by Factor and Period") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE, position = "center")

#### ---- Testing ---- #####
# Test whether Factor outperforms S&P 500 (excess return)
t.test(df_spy$dtv - df_spy$sp500)
t.test(df_spy$noa - df_spy$sp500)
t.test(df_spy$prc - df_spy$sp500)


# simple CAPM-style test "Would my factor outperform the market (benchmark = sp500)?"

factor_list <- c("dtv", "noa", "prc")

reg_results <- list()

# Loop through each factor and run regression against S&P 500
for (fac in factor_list) {
  formula <- as.formula(paste(fac, "~ sp500"))
  model <- lm(formula, data = df_spy)
  reg_results[[fac]] <- tidy(model)
}

# results
print(reg_results$dtv)
print(reg_results$noa)
print(reg_results$prc)

#### --------- Plots ---------- ####


# NOTE: NOA data start in 1950s, as we merged the data frames for efficiency we cut there
dtv_df <- dtv_result$rolling %>% filter(date >= as.Date("1955-01-01"))
noa_df <- noa_result$rolling %>% filter(date >= as.Date("1955-01-01"))
prc_df <- prc_result$rolling %>% filter(date >= as.Date("1955-01-01"))

# plotting function
plot_rolling_metric <- function(df, metric_col, title_text, y_label, color_line = "blue") {
  ggplot(df, aes(x = date, y = .data[[metric_col]])) +
    geom_line(color = color_line) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "tomato") +
    geom_vline(xintercept = as.Date("2010-01-01"), linetype = "dotted", color = "lightblue") +
    labs(title = title_text, x = "Date", y = y_label) +
    theme_minimal()
}

### NOTES

# Sharpe
# Sharpe = mean return / standard deviation
# measures risk-adjusted returns using a 3 year window
# We ask: When was investing in factor "efficient" from a risk-reward perspective?

# Avg. Returns
# Rolling average of raw (not risk-adjusted) monthly returns
# These are raw opportunities, but say nothing about the volatility required to earn them.
# Caution: A high return can still be unattractive if volatility is too high (see Sharpe above).

# Volatility
# Standard deviation of monthly returns over rolling 36-month windows
# Helps explain dips in the Sharpe ratio 
# --> even stable returns with spiking volatility reduce risk-adjusted performance.

###### ---- DTV ---- #### 

plot_rolling_metric(dtv_df, "roll_sharpe", "DTV: Rolling 36-Month Sharpe Ratio", "Sharpe Ratio")
# Sharpe ratio trends downward post-1990s, reflecting declining risk-adjusted returns.
# Sustained negative Sharpe post-2010s suggests DTV has lost its economic value.
# Earlier peaks (1970s–1990s) indicate periods where illiquidity was better rewarded.

plot_rolling_metric(dtv_df, "roll_mean",   "DTV: Rolling 36-Month Avg. Return",   "Average Return")
# Positive average returns in 1970s–1980s align with known illiquidity premium periods.
# Persistent sub-zero returns after 2010 suggest decay in the DTV return anomaly.
# Short-lived post-crisis recoveries fail to sustain alpha beyond early 2000s.

plot_rolling_metric(dtv_df, "roll_sd",     "DTV: Rolling 36-Month Volatility",    "Volatility")
# Volatility spikes in the 1970s (stagflation, energy crisis) and early 2000s (dot-com bust).
# Has structurally declined post-2008, consistent with greater liquidity and HFT growth.
# Recent increases (2020+) suggest renewed market frictions, but without return compensation.

# cumulative performance
# Cumulative growth of $1 invested in the DTV factor vs. the benchmark
df_cum <- df_spy %>%
  arrange(date) %>%
  mutate(
    cum_dtv   = cumprod(1 + dtv),
    cum_sp500 = cumprod(1 + sp500)
  )

ggplot(df_cum, aes(x = date)) +
  geom_line(aes(y = cum_dtv, color = "DTV Factor")) +
  geom_line(aes(y = cum_sp500, color = "S&P 500 (Excess Return)")) +
  scale_color_manual(name = "Series", values = c("DTV Factor" = "blue", "S&P 500 (Excess Return)" = "red")) +
  labs(
    title = "Cumulative Return: DTV Factor vs. S&P 500",
    x = "Date",
    y = "Cumulative Return (Growth of $1)"
  ) +
  theme_minimal()

# DTV factor shows flat or declining performance vs. steadily compounding S&P 500.
# Since ~2010, DTV factor appears structurally underperforming in both total and excess terms.
# Suggests strong crowding or arbitrage of the illiquidity premium in modern markets.

###### ---- NOA ---- #### 

plot_rolling_metric(noa_df, "roll_sharpe", "NOA: Rolling 36-Month Sharpe Ratio", "Sharpe Ratio")
# Sharpe ratio peaked before 2005 but declined steadily after --> signal decay post-publication.
# Since 2010, Sharpe mostly negative or near-zero --> no compensation for risk.
# PPS lacks persistent pricing power, possibly arbitraged or correlated with weakened factors.

plot_rolling_metric(noa_df, "roll_mean",   "NOA: Rolling 36-Month Avg. Return",   "Average Return")
# Average returns trend downward after early 2000s --> structural decay in raw factor performance.
# Rolling mean negative after 2010 --> even without considering risk, returns alone are unattractive.
# Likely crowded or no longer capturing relevant firm characteristics (e.g., size, liquidity).

plot_rolling_metric(noa_df, "roll_sd",     "NOA: Rolling 36-Month Volatility",    "Volatility")
# Volatility is low and stable throughout --> between ~1.5% to 3% monthly.
# No major volatility spikes --> Sharpe weakness is not risk-driven but return-driven.
# Interpretation: Factor lacks volatility “leverage” to generate excess returns --> fails to justify inclusion on risk basis.

# Cumulative performance: NOA
df_cum_noa <- df_spy %>%
  arrange(date) %>%
  mutate(
    cum_noa   = cumprod(1 + noa),
    cum_sp500 = cumprod(1 + sp500)
  )

ggplot(df_cum_noa, aes(x = date)) +
  geom_line(aes(y = cum_noa, color = "NOA Factor")) +
  geom_line(aes(y = cum_sp500, color = "S&P 500 (Excess Return)")) +
  scale_color_manual(name = "Series", values = c("NOA Factor" = "blue", "S&P 500 (Excess Return)" = "red")) +
  labs(
    title = "Cumulative Return: NOA Factor vs. S&P 500",
    x = "Date",
    y = "Cumulative Return (Growth of $1)"
  ) +
  theme_minimal()

# PPS factor fails to grow meaningfully since 2000 --> near-flat excess return profile.
# S&P 500 (excess return) shows strong compounding --> benchmark dominates.
# PPS not investable as standalone strategy --> poor performance and decay post-publication.


###### ---- PRC ---- #### 

plot_rolling_metric(prc_df, "roll_sharpe", "PPS: Rolling 36-Month Sharpe Ratio", "Sharpe Ratio")
# Sharpe ratio of PPS oscillates but remains weak or negative post-2000.
# Consistent periods of poor risk-adjusted performance undermine economic justification.
# Post-publication Sharpe < 0 indicates the market may have priced out this inefficiency.

plot_rolling_metric(prc_df, "roll_mean",   "PPS: Rolling 36-Month Avg. Return",   "Average Return")
# Rolling returns show high average returns pre-1990s, but drop into negative territory thereafter.
# Peak return periods coincide with elevated volatility — reward came at a cost.
# Post-2010 returns mostly negative, confirming performance decay.

plot_rolling_metric(prc_df, "roll_sd",     "PPS: Rolling 36-Month Volatility",    "Volatility")
# High volatility spikes in 1970s, early 2000s, and post-2020 — likely macro stress episodes.
# No structural volatility decline, unlike NOA — suggests instability rather than maturity.
# Current volatility elevated despite low returns → worsens Sharpe ratio.


# Cumulative performance: PRC
df_cum_prc <- df_spy %>%
  arrange(date) %>%
  mutate(
    cum_prc   = cumprod(1 + prc),
    cum_sp500 = cumprod(1 + sp500)
  )

ggplot(df_cum_prc, aes(x = date)) +
  geom_line(aes(y = cum_prc, color = "PPS Factor")) +
  geom_line(aes(y = cum_sp500, color = "S&P 500 (Excess Return)")) +
  scale_color_manual(name = "Series", values = c("PPS Factor" = "blue", "S&P 500 (Excess Return)" = "red")) +
  labs(
    title = "Cumulative Return: PPS Factor vs. S&P 500",
    x = "Date",
    y = "Cumulative Return (Growth of $1)"
  ) +
  theme_minimal()

# PPS factor severely underperforms the S&P 500 post-2000, indicating factor decay.
# Early relative outperformance disappears, suggesting mean-reversion or arbitrage pressure.
# By 2023, PPS has failed to compound meaningfully — strong signal that it's no longer an investable anomaly.

#combined plots for presentation

metrics <- c("roll_mean", "roll_sd", "roll_sharpe")
y_labels <- c("Average Monthly Return", "Rolling Volatility", "Sharpe Ratio")
titles <- c("Rolling 36-Month Average Returns: DTV, NOA, PRC",
            "Rolling 36-Month Volatility: DTV, NOA, PRC",
            "Rolling 36-Month Sharpe Ratios: DTV, NOA, PRC")

plot_list <- list()

for (i in seq_along(metrics)) {
  metric <- metrics[i]
  
  df_combined <- bind_rows(
    dtv_df %>% dplyr::select(date, !!metric) %>% mutate(factor = "DTV"),
    noa_df %>% dplyr::select(date, !!metric) %>% mutate(factor = "NOA"),
    prc_df %>% dplyr::select(date, !!metric) %>% mutate(factor = "PRC")
  ) %>% rename(value = !!metric)
  
  p <- ggplot(df_combined, aes(x = date, y = value, color = factor)) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "tomato") +
    geom_vline(xintercept = as.Date("2010-01-01"), linetype = "dotted", color = "lightblue") +
    labs(
      title = titles[i],
      x = "Date",
      y = y_labels[i],
      color = "Factor"
    ) +
    theme_minimal()
  
  print(p)  
  plot_list[[metric]] <- p
}

library(dplyr)

df_cum_all <- df_spy %>%
  arrange(date) %>%
  mutate(
    cum_dtv   = cumprod(1 + dtv),
    cum_noa   = cumprod(1 + noa),
    cum_prc   = cumprod(1 + prc),
    cum_sp500 = cumprod(1 + sp500)
  ) %>%
  dplyr::select(date, cum_dtv, cum_noa, cum_prc, cum_sp500) %>%
  pivot_longer(-date, names_to = "Series", values_to = "CumulativeReturn") %>%
  mutate(
    Series = recode(Series,
                    cum_dtv   = "DTV",
                    cum_noa   = "NOA",
                    cum_prc   = "PPS",
                    cum_sp500 = "S&P 500 (Excess Return)"
    )
  )

ggplot(df_cum_all, aes(x = date, y = CumulativeReturn, color = Series)) +
  geom_line() +
  labs(
    title = "Cumulative Return: DTV, NOA, PPS Factors vs. S&P 500",
    x = "Date",
    y = "Cumulative Return"
  ) +
  theme_minimal() +
  scale_color_manual(values = c(
    "DTV" = "blue",
    "NOA" = "darkgreen",
    "PPS" = "purple",
    "S&P 500 (Excess Return)" = "red"
  ))

#### ------------- (PART D) ------------------ ######

# FF3
#ff3 <- read.csv("FF.csv", stringsAsFactors = FALSE)

ff3 <- ff3 %>%
  mutate(
    date = seq(as.Date("1926-07-31"), by = "month", length.out = nrow(ff3)),
    Mkt_RF = as.numeric(Mkt.RF) / 100,
    SMB = as.numeric(SMB) / 100,
    HML = as.numeric(HML) / 100,
    RF = as.numeric(RF) / 100
  ) %>%
  dplyr::select(date, Mkt_RF, SMB, HML, RF)

# Hou et al. Q5 model
#q5 <- read.csv("q5_factors_monthly_2024.csv")  
colnames(q5)
library(lubridate)
q5 <- q5 %>%
  mutate(
    date = make_date(year, month, 1),  # builds the first day of each month
    MKT = R_MKT / 100,
    ME  = R_ME  / 100,
    IA  = R_IA  / 100,
    ROE = R_ROE / 100,
    EG  = R_EG  / 100,
    RF_Q5 = R_F / 100
  ) %>%
  dplyr::select(date, MKT, ME, IA, ROE, EG, RF_Q5)

library(lubridate)

# Option 1: Using floor_date (sets day to 01)
factors <- factors %>% mutate(date = floor_date(date, "month"))
ff3     <- ff3     %>% mutate(date = floor_date(date, "month"))
q5      <- q5      %>% mutate(date = floor_date(date, "month"))

# Full merge: your factors + FF3 + Q5
df_all <- reduce(list(factors, ff3, q5), full_join, by = "date") %>%
  drop_na()

# Use FF3 RF and drop Q5’s
df_all <- df_all %>% dplyr::select(-RF_Q5)

# Define excess returns
df_all <- df_all %>%
  mutate(
    excess_dtv = dtv,
    excess_noa = noa,
    excess_prc = prc
  )

# Formulas for CAPM, FF3, Q5
capm <- y ~ Mkt_RF
ff3  <- y ~ Mkt_RF + SMB + HML
q5   <- y ~ MKT + ME + IA + ROE + EG
df_all<-na.omit(df_all)

# loop for outputs
results <- list()
for (fac in c("excess_dtv", "excess_noa", "excess_prc")) {
  df_all <- df_all %>% mutate(y = .data[[fac]])
  
  results[[fac]] <- list(
    CAPM = summary(lm(capm, data = df_all)),
    FF3  = summary(lm(ff3, data = df_all)),
    Q5   = summary(lm(q5, data = df_all))
  )
}
results

########DTV
# No significant market exposure or alpha under CAPM and FF3
# Under FF3, strong positive exposure to:SMB(strong small-cap bias) and HML(lean to value stocks)
# Under Q5, all five factors are significant.Loadings:
#Negative MKT:inverse market exposure. 
#Positive ME: confirms small-cap tilt.
# Positive IA: links DTV to high investment firms.
# Negative ROE: indicates a bias toward unprofitable stocks (speculative trading).
# Positive EG: associated with firms that have high expected growth, consistent with speculative behavior.
# R2 is close to 0 for CAPM, 22% for FF3 and 67% for Q5. 
# 
# liquidity proxy:behaves as small, illiquid, speculative stocks.
# Supports liquidity premium theory
# No unique return generation ? offers no additional benefit over small-cap strategies.
# 
############NOA
# Under CAPM, NOA has a negative market beta:suggests potential as a market hedge.
# Positive and significant alpha
# Under FF3:
# Negative MKT and HML betas:tilt towards growth stocks. Alpha remains significant.
# Under Q5 only ROE and IA are significant. Negative ROE + positive IA betas link to low profitability, high investment firms (i.e., overinvesting).
# Alpha is smaller but still significant, meaning Q5 explains some of the return variation.
# R2 is close to 0 for CAPM, 1% for FF3 and increases to 14% for Q5. Most of the variance is unexplained by these models. 
# Assuming the source of NOA might be accounting-based mispricing we try the Stambaugh-Yuan Mispricing Model later in the exercise. 
#
# Despite exposure to inefficient growth firms, NOA continues to yield excess returns, suggests investor neglect of inefficiencies.
# 
########PPS
# Sensitive to market risk across all models with positive market betas.
# In FF3 all factors significant:
# positive SMB and HML, negative alpha ? suggests underperformance after adjusting for risk.
# In Q5 all five factors significant:
# 
# Relatively high ME suggests small-cap bias.
# Negative ROE and EG indicate focus on unprofitable, low-growth firms.
# Alpha sign change from FF3.
# R2 is 6% for CAPM, 25% for FF3 and 59% for Q5.
#
# PPS factor resembles a pro-cyclical (positive market corr), lottery-like strategy

#regressing factors against each other:
dtv_fl<-lm(dtv~noa+prc, data=df_all)
summary(dtv_fl)
noa_fl<-lm(noa~dtv+prc, data=df_all)
summary(noa_fl)
prc_fl<-lm(prc~noa+dtv, data=df_all)
summary(prc_fl)

#noa is essentially independent of the other factors
#PRC and DVT are significant as explanatory variables when regressing against each other. Around 40% of variance explained by other factor.

#Stambaugh?Yuan Mispricing Model
#includes quality management and profitability

#mdf <- read.csv("M4.csv", stringsAsFactors = FALSE)
mdf <- mdf %>%
  mutate(date = ymd(paste0(YYYYMM, "01")))
mdf <- mdf %>% dplyr::select(-YYYYMM)
df_all2 <- reduce(list(factors, mdf), full_join, by = "date") %>%
  drop_na()
m4   <- y ~ MKTRF + MGMT + PERF+SMB

results2 <- list()
for (fac in c("dtv", "noa", "prc")) {
  df_all2 <- df_all2 %>% mutate(y = .data[[fac]])
  
  results2[[fac]] <- list(
    M4 = summary(lm(m4, data = df_all2))
  )
}
results2

#alternative candidate model explains PRC and DVT, leaves no statistically significant alpha. NOA still shows unexplained returns.
#R2 is higher in all cases using the Q5 model. For NOA we try a combination of factors in an attempt in increase explained variance:

df_all3 <- reduce(list(df_all, mdf), full_join, by = "date") %>%
  drop_na()
m4   <- y ~ MKT + ROE + EG + MGMT+PERF

results3 <- list()
for (fac in c("noa")) {
  df_all3 <- df_all3 %>% mutate(y = .data[[fac]])
  
  results3[[fac]] <- list(
    M4 = summary(lm(m4, data = df_all3))
  )
}
results3

#All variables are significant and alpha is smallest out of all models. R2 increased 5 percentage points, but is still low.

