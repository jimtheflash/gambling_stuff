from datetime import date, timedelta
import os
import time
from glob import glob
from random import uniform

import numpy as np

from args import get_cli_args
from game import get_game_pbp, get_game_boxscore
from get_logger import get_logger
from player import get_player_info
from schedule import get_day_schedule
from season import format_season_string, get_current_season, get_season_gamelog


logger = get_logger(__name__)

GAMELOG_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.realpath(__file__))),
    "data",
    "nba_gamelogs",
)
PBP_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.realpath(__file__))),
    "data",
    "nba_pbp",
)
BS_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.realpath(__file__))),
    "data",
    "nba_boxscores",
)
SCHEDULE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.realpath(__file__))),
    "data",
    "nba_schedules",
)
PI_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.realpath(__file__))),
    "data",
    "nba_player_info",
)
SYNC_CATEGORIES = [
    {
        "category": "play_by_play",
        "base_path": PBP_PATH,
        "column_name": "GAME_ID",
        "function": get_game_pbp,
    },
    {
        "category": "boxscore",
        "base_path": BS_PATH,
        "column_name": "GAME_ID",
        "function": get_game_boxscore,
    },
    {
        "category": "player_info",
        "base_path": PI_PATH,
        "column_name": "PLAYER_ID",
        "function": get_player_info,
    },
]


def main(args: dict):
    season_year = args.get("season") or get_current_season()
    season = format_season_string(season_year)

    gamelog_df = get_season_gamelog(season)
    source_file_name = os.path.join(GAMELOG_PATH, f"nba_gamelogs_{season}.csv")
    gamelog_df.to_csv(source_file_name, index=False)
    today_dt = date.today()
    schedule_days = [today_dt, today_dt + timedelta(days=1)]
    for schedule in schedule_days:
        schedule_df = get_day_schedule(schedule)
        if len(schedule_df) > 0:
            schedule_file_name = os.path.join(
                SCHEDULE_PATH, f"{str(schedule).replace('-', '')}.csv"
            )
            schedule_df.to_csv(schedule_file_name, index=False)
            time.sleep(uniform(1.6, 2.5))

    for sync in SYNC_CATEGORIES:
        category = sync["category"]
        base_path = sync["base_path"]
        col = sync["column_name"]
        func = sync["function"]

        sync_ids = list(gamelog_df[gamelog_df["PLAYER_NAME"].notnull()][col].unique())
        logger.info(f"Syncing season {category} files to local")
        sync_files = glob(os.path.join(base_path, "*.csv"))
        local_ids = [os.path.basename(x).split(".")[0] for x in sync_files]
        missing_ids = list(np.setdiff1d(sync_ids, local_ids))
        logger.info(
            f"There are {len(missing_ids)} missing {category} files to retrieve"
        )
        if not args.get("dryrun"):
            logger.info(f"Downloading {category} files...")
            for idx, sync_id in enumerate(missing_ids):
                try:
                    if idx % 10 == 0:
                        logger.info(f"Downloading {idx}/{len(missing_ids)} files")
                    sync_df = func(sync_id)
                    sync_file_name = os.path.join(base_path, f"{sync_id}.csv")
                    sync_df.to_csv(sync_file_name, index=False)
                    time.sleep(uniform(1.6, 2.5))
                except:
                    logger.warning("Hit unexpected exception. Skipping...")
                    time.sleep(uniform(1.6, 2.5))
                    continue


if __name__ == "__main__":
    args = get_cli_args()
    main(args)
