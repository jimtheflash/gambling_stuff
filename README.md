# gambling_stuff

This is where some random stuff for betting on sports will live, until it lives somewhere else.

# NBA

## Setup

- Install a version of Python at least 3.7.X
- (Recommended) Create a local virtual environment
    - `venv`: `python -m venv .venv`
- Activate venv and install project requirements
    - MacOS: `source .venv/bin/activate & pip install -r nba_api-scripts/requirements.txt`

## Sync local data files
This process will sync local data files for play-by-play and boxscore API's for a single season

- Activate virtual environment
- Example invocation:
    - `python nba_api_scripts/01_sync_season_data_to_local.py`