echo "Running daily update"
git checkout main
git pull

source $1 && python nba_api_scripts/01_sync_season_data_to_local.py
COMMIT_DTTM=`date +"%D %T"`
echo "Commit DTTM: $COMMIT_DTTM"
git add *.csv
git commit -m "Latest file update: $COMMIT_DTTM"
git push