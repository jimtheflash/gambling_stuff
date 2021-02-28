import pandas as pd
import nba_api.stats.endpoints.boxscoretraditionalv2 as bs2
import nba_api.stats.endpoints.playbyplayv2 as pbp

from get_logger import get_logger


logger = get_logger(__name__)


def get_game_pbp(game_id: int) -> pd.DataFrame:
    logger.info(f"Getting PBP for game: {game_id}")
    game_id = str(game_id)
    playbyplay = pbp.PlayByPlayV2(game_id=game_id, start_period=0, end_period=0)
    pbp_df = playbyplay.get_data_frames()[0]

    return pbp_df


def get_game_boxscore(game_id: int) -> pd.DataFrame:
    logger.info(f"Getting box score for game: {game_id}")
    game_id = str(game_id)
    boxscore = bs2.BoxScoreTraditionalV2(game_id=game_id)
    boxscore_df = boxscore.get_data_frames()[0]

    return boxscore_df