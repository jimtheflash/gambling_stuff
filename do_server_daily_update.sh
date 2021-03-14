echo "Running daily update"
git checkout main
git pull

. .venv/bin/activate && python nba_api_scripts/01_sync_season_data_to_local.py
COMMIT_DTTM=`date +"%D %T"`
echo "Commit DTTM: $COMMIT_DTTM"
git add data/*/*.csv
git commit -m "Latest file update: $COMMIT_DTTM"
git push