from datetime import datetime
import logging

import nba_api.stats.endpoints.commonteamyears as ct
import nba_api.stats.endpoints.playergamelogs as pg
import pandas as pd
from dateutil.relativedelta import relativedelta

from get_logger import get_logger


logger = get_logger(__name__)


def get_current_season() -> str:
    """Uses CommonTeamYears to get the latest season year"""
    logger.info("Getting current season year")
    team_years = ct.CommonTeamYears().get_data_frames()[0]
    max_year = team_years["MAX_YEAR"].unique().max()

    return max_year


def format_season_string(year: str) -> str:
    """Uses season year to generate a season year bounded string"""
    year_dttm = datetime.strptime(year, "%Y")
    season_end = ((year_dttm + relativedelta(years=1)).year) % 100
    season_string = f"{year}-{season_end}"

    return season_string


def get_season_gamelog(season_string: str) -> pd.DataFrame:
    """
    Args:
        season_string (str): formatted season year string
        season_type (str): one of
    Returns:
        dataframe from NBA API with DFS points appended
    """
    logger.info(f"Getting season game log for season: {season_string}")
    gl_df_list = []
    for season_type in ["Pre Season", "Regular Season", "PlayIn", "Playoffs"]:
        seas_gamelog = pg.PlayerGameLogs(
            season_nullable=season_string, season_type_nullable=season_type
        )
        tmp_gl_df = seas_gamelog.get_data_frames()
        tmp_gl_df = tmp_gl_df[0]
        tmp_gl_df["SEASON_TYPE"] = season_type
        gl_df_list.append(tmp_gl_df)
    gl_df = pd.concat(gl_df_list)
    gl_df["fd"] = (
        gl_df.PTS
        + (1.2 * gl_df.REB)
        + (1.5 * gl_df.AST)
        + (3 * gl_df.BLK)
        + (3 * gl_df.STL)
        + (-1 * gl_df.TOV)
    )
    gl_df["dk"] = (
        gl_df.PTS
        + (0.5 * gl_df.FG3M)
        + (1.25 * gl_df.REB)
        + (1.5 * gl_df.AST)
        + (2 * gl_df.BLK)
        + (2 * gl_df.STL)
        + (-0.5 * gl_df.TOV)
        + (1.5 * gl_df.DD2)
        + (3 * gl_df.TD3)
    )
    gl_df = gl_df.sort_values(by=["GAME_ID", "PLAYER_ID"])

    return gl_df
