echo "Running daily update"
git checkout main
git pull

. .venv/bin/activate && python nba_api_scripts/rotowire_lineups.py
git add data/01_raw/nba_lineups/*/*/*/*/*.html
git add data/01_raw/nba_lineups/*.html
git add data/02_curated/nba_lineups/*/*/*/*/*.csv
git add data/02_curated/nba_lineups/*.csv
git commit -m "Lineups file update"
git push