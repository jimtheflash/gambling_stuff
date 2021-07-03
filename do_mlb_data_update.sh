echo "**** Running MLB daily update ****"
git checkout main
git pull

echo "-- Sync team roster data --"
. .venv/bin/activate && python mlb_api_scripts/team_rosters.py
git add data/01_raw/mlb/rosters/*
git add data/01_raw/mlb/teams/*
git add data/02_curated/mlb/*
git commit -m "MLB rosters update"
git push