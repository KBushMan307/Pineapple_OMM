library(DBI)
library(RPostgres)
library(tidyverse)
library(pROC)

con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = "railway",
  host     = "nozomi.proxy.rlwy.net",
  port     = 11470,
  user     = "postgres",
  password = "cudxFcbrmueliIyKTzLjPYNgijILiOPw",
  sslmode = 'require'
)

MLB_Model_History <- DBI::dbGetQuery(
  con,
  "SELECT * FROM mlb_winner_history"
)

MLB_Model_History <- MLB_Model_History |>
  filter(!(spread %in% c(-2.5, 1, -2)))

MLB_Model_History$full_bet_type <- paste(MLB_Model_History$bet_type,
                                         MLB_Model_History$spread)

MLB_Model_History$full_bet_type <- ifelse(MLB_Model_History$full_bet_type == "ML NA",
                                          "ML", MLB_Model_History$full_bet_type)

MLB_Model_History$full_bet_type <- as.factor(MLB_Model_History$full_bet_type)

MLB_Model_History$vegas_prob <- round(ifelse(MLB_Model_History$model_price > 0,
                                             100 / (MLB_Model_History$model_price + 100),
                                             abs(MLB_Model_History$model_price) /
                                               (abs(MLB_Model_History$model_price) + 100)), 4)

MLB_Model_History$win_prob <- MLB_Model_History$vegas_prob +
  MLB_Model_History$model_edge

MLB_Model_History$win <- ifelse(MLB_Model_History$result == "win",
                                1, 0)

MLB_Model_History <- MLB_Model_History |>
  filter(!is.na(win))

MLB_Performance <- MLB_Model_History |>
  select(game_date, full_bet_type, win_prob, win) |>
  arrange(game_date) |>
  group_by(full_bet_type) |>
  mutate(
    game_num = seq_len(n()),
    AUC = sapply(seq_len(n()), function(i) {
      if (n_distinct(win[1:i]) < 2) return(NA_real_)
      
      as.numeric(auc(win[1:i], win_prob[1:i]))
    }),
    Brier_Error = (win - win_prob)^2,
    Brier_Score = cummean(Brier_Error),
    Current_Value = row_number() == n()
  ) |>
  ungroup() |>
  select(game_date, game_num, full_bet_type, AUC, Brier_Score, Current_Value) |>
  pivot_longer(cols = c(AUC, Brier_Score),
               values_to = "Value", names_to = "Measure") |>
  mutate(Sport = "MLB") |>
  rename(bet_type = full_bet_type)



ggplot(MLB_Performance, aes(x = game_date, y = Cumulative_AUC,
                            color = full_bet_type)) +
  geom_line()

ggplot(MLB_Performance, aes(x = game_date, y = Cumulative_Brier,
                            color = full_bet_type)) +
  geom_hline(yintercept = 0.25, linetype = "dashed", alpha = 0.5) +
  geom_line() 



NBA_Model_History <- DBI::dbGetQuery(
  con,
  "SELECT * FROM nba_game_pick_history"
)

NBA_Model_History$final_spread <- NBA_Model_History$home_score -
  NBA_Model_History$away_score

NBA_Model_History$win <- ifelse(NBA_Model_History$result == "win",
                                1, 0)

NBA_Model_History$vegas_prob <- round(ifelse(NBA_Model_History$odds_american > 0,
                                             100 / (NBA_Model_History$odds_american + 100),
                                             abs(NBA_Model_History$odds_american) /
                                               (abs(NBA_Model_History$odds_american) + 100)), 4)

NBA_ML_History <- NBA_Model_History |>
  filter(pick_type == "ML" & !is.na(win))

NBA_Spread_History <- NBA_Model_History |>
  filter(pick_type == "SPREAD" & !is.na(win))

NBA_ML_Performance <- NBA_ML_History |>
  select(game_date, model_win_prob, win) |>
  arrange(game_date) |>
  mutate(
    game_num = seq_len(n()),
    AUC = sapply(seq_len(n()), function(i) {
      if (n_distinct(win[1:i]) < 2) return(NA_real_)
      
      as.numeric(auc(win[1:i], model_win_prob[1:i]))
    }),
    Brier_Error = (win - model_win_prob)^2,
    Brier_Score = cummean(Brier_Error),
    Current_Value = row_number() == n()
  ) |>
  select(game_date, game_num, AUC, Brier_Score, Current_Value) |>
  pivot_longer(cols = c(AUC, Brier_Score),
               values_to = "Value", names_to = "Measure") |>
  mutate(Sport = "NBA", bet_type = "ML")

NBA_Spread_Performance <- NBA_Spread_History |>
  select(game_date, model_spread, final_spread) |>
  arrange(game_date) |>
  mutate(
    game_num = seq_len(n()),
    Squared_Error = (final_spread - model_spread)^2,
    RMSE = sqrt(cummean(Squared_Error)),
    Absolute_Error = abs(final_spread - model_spread),
    MAE = cummean(Absolute_Error),
    Absolute_Percent_Error = abs((final_spread - model_spread)/final_spread)*100,
    MAPE = cummean(Absolute_Percent_Error),
    Current_Value = row_number() == n()
  ) |>
  select(game_date, game_num, RMSE, MAE, MAPE, Current_Value) |>
  pivot_longer(cols = c(RMSE, MAE, MAPE),
               values_to = "Value", names_to = "Measure") |>
  mutate(Sport = "NBA", bet_type = "Spread")



ggplot(MLB_Performance, aes(x = game_date, y = Cumulative_AUC,
                            color = full_bet_type)) +
  geom_line()

ggplot(MLB_Performance, aes(x = game_date, y = Cumulative_Brier,
                            color = full_bet_type)) +
  geom_hline(yintercept = 0.25, linetype = "dashed", alpha = 0.5) +
  geom_line() 



NFL_Model_History <- DBI::dbGetQuery(
  con,
  "SELECT * FROM nfl_winner_history"
)

NFL_Model_History$final_spread <- NFL_Model_History$home_score -
  NFL_Model_History$away_score

NFL_Model_History$win <- ifelse(NFL_Model_History$result == "win",
                                1, 0)

NFL_Model_History$vegas_prob <- round(ifelse(NFL_Model_History$odds_american > 0,
                                             100 / (NFL_Model_History$odds_american + 100),
                                             abs(NFL_Model_History$odds_american) /
                                               (abs(NFL_Model_History$odds_american) + 100)), 4)

NFL_ML_History <- NFL_Model_History |>
  filter(pick_type == "ML" & !is.na(win))

NFL_Spread_History <- NFL_Model_History |>
  filter(pick_type == "SPREAD" & !is.na(win))

NFL_ML_Performance <- NFL_ML_History |>
  select(game_date, win_prob, win) |>
  arrange(game_date) |>
  mutate(
    game_num = seq_len(n()),
    AUC = sapply(seq_len(n()), function(i) {
      if (n_distinct(win[1:i]) < 2) return(NA_real_)
      
      as.numeric(auc(win[1:i], win_prob[1:i]))
    }),
    Brier_Error = (win - win_prob)^2,
    Brier_Score = cummean(Brier_Error),
    Current_Value = row_number() == n()
  ) |>
  select(game_date, game_num, AUC, Brier_Score, Current_Value) |>
  pivot_longer(cols = c(AUC, Brier_Score),
               values_to = "Value", names_to = "Measure") |>
  mutate(Sport = "NFL", bet_type = "ML")

# NFL_Spread_Performance <- NFL_Spread_History |>
#   select(game_date, model_spread, final_spread) |>
#   arrange(game_date) |>
#   mutate(
#     game_num = seq_len(n()),
#     Squared_Error = (final_spread - model_spread)^2,
#     RMSE = sqrt(cummean(Squared_Error)),
#     Absolute_Error = abs(final_spread - model_spread),
#     MAE = cummean(Absolute_Error),
#     Absolute_Percent_Error = abs((final_spread - model_spread)/final_spread)*100,
#     MAPE = cummean(Absolute_Percent_Error),
#     Current_Value = row_number() == n()
#   ) |>
#   select(game_date, game_num, RMSE, MAE, MAPE, Current_Value) |>
#   pivot_longer(cols = c(RMSE, MAE, MAPE),
#                values_to = "Value", names_to = "Measure") |>
#   mutate(Sport = "NFL", bet_type = "Spread")



NCAAF_Model_History <- DBI::dbGetQuery(
  con,
  "SELECT * FROM ncaaf_winner_history"
)

NCAAF_Model_History$final_spread <- NCAAF_Model_History$home_score -
  NCAAF_Model_History$away_score

NCAAF_Model_History$win <- ifelse(NCAAF_Model_History$result == "win",
                                  1, 0)

NCAAF_Model_History$vegas_prob <- round(ifelse(NCAAF_Model_History$odds_american > 0,
                                               100 / (NCAAF_Model_History$odds_american + 100),
                                               abs(NCAAF_Model_History$odds_american) /
                                                 (abs(NCAAF_Model_History$odds_american) + 100)), 4)

NCAAF_ML_History <- NCAAF_Model_History |>
  filter(pick_type == "ML" & !is.na(win))

NCAAF_Spread_History <- NCAAF_Model_History |>
  filter(pick_type == "SPREAD" & !is.na(win))

NCAAF_ML_Performance <- NCAAF_Spread_History |>
  select(game_date, win_prob, win) |>
  arrange(game_date) |>
  mutate(
    game_num = seq_len(n()),
    AUC = sapply(seq_len(n()), function(i) {
      if (n_distinct(win[1:i]) < 2) return(NA_real_)
      
      as.numeric(auc(win[1:i], win_prob[1:i]))
    }),
    Brier_Error = (win - win_prob)^2,
    Brier_Score = cummean(Brier_Error),
    Current_Value = row_number() == n()
  ) |>
  select(game_date, game_num, AUC, Brier_Score, Current_Value) |>
  pivot_longer(cols = c(AUC, Brier_Score),
               values_to = "Value", names_to = "Measure") |>
  mutate(Sport = "NFL", bet_type = "ML")

# NCAAF_Spread_Performance <- NCAAF_Spread_History |>
#   select(game_date, model_spread, final_spread) |>
#   arrange(game_date) |>
#   mutate(
#     game_num = seq_len(n()),
#     Squared_Error = (final_spread - model_spread)^2,
#     RMSE = sqrt(cummean(Squared_Error)),
#     Absolute_Error = abs(final_spread - model_spread),
#     MAE = cummean(Absolute_Error),
#     Absolute_Percent_Error = abs((final_spread - model_spread)/final_spread)*100,
#     MAPE = cummean(Absolute_Percent_Error),
#     Current_Value = row_number() == n()
#   ) |>
#   select(game_date, game_num, RMSE, MAE, MAPE, Current_Value) |>
#   pivot_longer(cols = c(RMSE, MAE, MAPE),
#                values_to = "Value", names_to = "Measure") |>
#   mutate(Sport = "NFL", bet_type = "Spread")



Tot_Model_Performance <- bind_rows(MLB_Performance,
                                   NBA_ML_Performance,
                                   NBA_Spread_Performance,
                                   NFL_ML_Performance,
                                   NCAAF_ML_Performance) |>
  arrange(Sport, game_date)
