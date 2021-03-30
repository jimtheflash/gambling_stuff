from pathlib import Path

import pandas as pd

from nba_api.stats.endpoints.commonteamroster import CommonTeamRoster
from nba_api.stats.endpoints.commonteamyears import CommonTeamYears


def get_teams() -> pd.DataFrame:
    teams = CommonTeamYears().get_data_frames()[0]
    return teams


def get_team_season_roster(season_string: str, team_id: str) -> pd.DataFrame:
    dfs = CommonTeamRoster(season=season_string, team_id=team_id)
    roster = dfs.get_data_frames()[0]
    return roster
