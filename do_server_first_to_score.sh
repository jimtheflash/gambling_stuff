echo "Running first to score R scripts"
git checkout main
git pull

echo "Current season opening tip"
Rscript first_basket/'00_current_season_opening tip.R'
echo "Current season player aggregates"
Rscript first_basket/'00_current_season_player_aggregates.R'
echo "Current season take first shot"
Rscript first_basket/'00_current_season_take_first_shot.R'
echo "Jump ball rankings"
Rscript first_basket/'00_jump_ball_rankings_player_info.R'
echo "First to score bets"
Rscript first_basket/'01_first_to_score_bets.R'

git add data/02_curated/nba_first_to_score/*.csv.gz
git commit -m "Updating first to score"
git push