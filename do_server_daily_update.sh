echo "**** Running daily update ****"
git checkout main
git pull

echo "-- Sync season game data --"
. .venv/bin/activate && python nba_api_scripts/01_sync_season_data_to_local.py
echo "-- Sync team roster data --"
. .venv/bin/activate && python nba_api_scripts/team_rosters.py
echo "-- Starting R scripts for first to score --"
echo "---- Current season opening tip ----"
Rscript first_basket/'00_current_season_opening tip.R'
echo "---- Current season player aggregates ----"
Rscript first_basket/'00_current_season_player_aggregates.R'
echo "---- Current season take first shot ----"
Rscript first_basket/'00_current_season_take_first_shot.R'
echo "---- Jump ball rankings ----"
Rscript first_basket/'00_jump_ball_rankings_player_info.R'
git add data/*/*.csv
git add data/*/*.csv.gz
git add data/02_curated/nba_first_to_score/*.csv.gz
git add data/02_curated/nba_first_to_score/*/*/*/*.csv.gz
git commit -m "Daily file update"
git push