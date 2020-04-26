library(tidyverse)
library(ncaahoopR)
library(data.table)
library(sqldf)
library(ggthemes)

# louisville schedule
lou_games <- get_schedule("Louisville") %>% 
  filter(!is.na(team_score)) %>% 
  mutate(win_total = gsub( "-.*$", "", record),
         win_ind = ifelse(team_score > opp_score, 1, 0), 
         win_loss = ifelse(team_score > opp_score, "W", "L"))

# vector of louisville game IDs
game_ids <- lou_games$game_id

# get all play-by-play data
pbp_all <- get_pbp("Louisville")

# halftime scores
halftime <- pbp_all %>% 
  filter(half == 2 & description == 'PLAY') %>% 
  select(game_id, date, home, away, home_score, away_score, score_diff,
         win_prob, naive_win_prob) %>% 
  data.table() %>% 
  .[, `:=` (cards_score = home_score, opp_score = away_score)] %>% 
  .[away == 'Louisville', 
    `:=` (cards_score = away_score, opp_score = home_score)] %>% 
  mutate(h_score_diff = cards_score - opp_score)

# under 16 
u16 <- pbp_all %>% 
  filter(half == 2 & description == 'Official TV Timeout' & secs_remaining >= 750) %>% 
  mutate(unique_play = paste(game_id, play_id, sep = "_")) %>% 
  ## remove bad Miami row
  filter(unique_play != '401168156_202') %>% 
  ## add WKU in
  rbind(pbp_all %>% filter(game_id == 401168228 & play_id == 168) %>% 
          mutate(unique_play = paste(game_id, play_id, sep = "_"))) %>% 
  data.table() %>% 
  ## fix scores
  .[, `:=` (cards_score = home_score, opp_score = away_score)] %>% 
  .[away == 'Louisville', 
    `:=` (cards_score = away_score, opp_score = home_score)] %>% 
  mutate(u16_score_diff = cards_score - opp_score)

## data check: if WKU is the last row, data is correct
if(u16[nrow(u16), 'home'] == "Western Kentucky") {
  'Data is correct.'
} else {
  'Incorrect data.'
}

## join data & calculate 2H first segment differentials 
score_diffs <- sqldf("select u.u16_score_diff, h.*, lg.win_total, lg.win_ind, lg.win_loss
             from halftime h 
             left join u16 u on h.game_id = u.game_id
              left join lou_games lg on h.game_id = lg.game_id") %>% 
  select(game_id, home, away, h_score_diff, u16_score_diff, win_total, win_ind, win_loss) %>% 
  mutate(first4_diff = u16_score_diff - h_score_diff)

## avg 2H first segment differential
mean(score_diffs$first4_diff)

## plot trend 
## add shape for win/loss (ATS)
score_diffs %>% 
  mutate(game_row = 1:n(), 
         pos_diff_ind = as.numeric(first4_diff > 0), 
         diff_type = ifelse(first4_diff > 0, 'positive', 
                            ifelse(first4_diff == 0, 'even', 'negative'))) %>% 
  ggplot() + 
  # geom_point(aes(game_row, first4_diff)) + 
  geom_text(aes(game_row, first4_diff, label = win_loss, col = win_loss), size = 4) + 
  geom_line(aes(game_row, first4_diff)) + 
  scale_color_manual(values = c('darkred', 'black'),
                     aesthetics = c("colour")) +
  theme(legend.position = 'none') + 
  theme_fivethirtyeight() + 
  theme(legend.position = 'none')


pbp_all %>%
  filter(game_id == 401168482) %>% 
  select(play_id, win_prob, naive_win_prob) %>% 
  ggplot() + 
  geom_line(aes(play_id, win_prob), col = 'darkred') + 
  geom_line(aes(play_id, naive_win_prob), col = 'black') + 
  theme(legend.position = 'none')

pbp_all %>%
  filter(game_id == 401168482) %>% summary(win_prob)

court <- ncaahoopR::court

pbp_all %>% 
  filter(game_id == 401168482 & !is.na(shot_outcome) & shot_team == 'Louisville') %>% 
  ggplot() + 
  geom_point(aes(shot_x, shot_y, shape = shot_outcome, col = shot_team), size = 2.5, alpha = .5) +
  geom_polygon(data = court, aes(x=x, y=y, group = group), col = 'grey') + 
  scale_color_manual(values = c('darkred', 'navy')) + 
  # theme_fivethirtyeight() +
  theme(legend.position = 'none')
