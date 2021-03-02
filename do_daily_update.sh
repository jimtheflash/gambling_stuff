echo "Running daily update"
git checkout main
git pull

source .venv/bin/activate

python -m nba_api_scripts/01_sync_season_data_to_local.py

git add .

git commit -m "Latest file update"

git push