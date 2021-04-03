echo "Lineups update"
git checkout main
git pull

echo "Rotowire lineups"
. .venv/bin/activate && python nba_api_scripts/rotowire_lineups.py
echo "First to score bets"
Rscript first_basket/'01_first_to_score_bets.R'
git add data/01_raw/nba_lineups/*/*/*/*/*.html
git add data/01_raw/nba_lineups/*.html
git add data/02_curated/nba_lineups/*/*/*/*/*.csv
git add data/02_curated/nba_lineups/*.csv
git add data/02_curated/nba_first_to_score/*.csv.gz
git add data/02_curated/nba_first_to_score/*/*/*/*.csv.gz
git commit -m "Lineups file update"
git push