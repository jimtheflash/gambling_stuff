echo "Running daily update"
git checkout main
git pull

echo "Sync season game data"
. .venv/bin/activate && python nba_api_scripts/01_sync_season_data_to_local.py
echo "Sync team roster data"
. .venv/bin/activate && python nba_api_scripts/team_rosters.py
COMMIT_DTTM=`date +"%D %T"`
echo "Commit DTTM: $COMMIT_DTTM"
git add data/*/*.csv
git add data/*/*.csv.gz
git commit -m "Latest file update: $COMMIT_DTTM"
git push