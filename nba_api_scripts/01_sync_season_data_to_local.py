import os
import logging
import time
import sys
from glob import glob

import numpy as np

from args import get_cli_args
from game import get_game_pbp, get_game_boxscore
from get_logger import get_logger
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
SYNC_CATEGORIES = [
    {"category": "play_by_play", "base_path": PBP_PATH, "function": get_game_pbp},
    {"category": "boxscore", "base_path": BS_PATH, "function": get_game_boxscore},
]


def main(args: dict):
    season_year = args.get("season") or get_current_season()
    season = format_season_string(season_year)

    gamelog_df = get_season_gamelog(season)
    source_file_name = os.path.join(GAMELOG_PATH, f"nba_gamelogs_{season}.csv")
    gamelog_df.to_csv(source_file_name, index=False)
    game_ids = list(gamelog_df.GAME_ID.unique())

    for sync in SYNC_CATEGORIES:
        category = sync["category"]
        base_path = sync["base_path"]
        func = sync["function"]

        logger.info(f"Syncing season {category} files to local")
        sync_files = glob(os.path.join(base_path, "*.csv"))
        local_game_ids = [os.path.basename(x).split(".")[0] for x in sync_files]
        missing_game_ids = list(np.setdiff1d(game_ids, local_game_ids))
        logger.info(f"There are {len(missing_game_ids)} missing games to retrieve")
        if not args.get("dryrun"):
            for game_id in missing_game_ids:
                pbp_df = func(game_id)
                pbp_file_name = os.path.join(PBP_PATH, f"{game_id}.csv")
                pbp_df.to_csv(pbp_file_name, index=False)
                time.sleep(2)


if __name__ == "__main__":
    args = get_cli_args()
    main(args)
