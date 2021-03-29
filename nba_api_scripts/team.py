from pandas import DataFrame

from nba_api.stats.endpoints.commonteamroster import CommonTeamRoster
from nba_api.stats.endpoints.commonteamyears import CommonTeamYears

from season import get_current_season, format_season_string


def get_active_teams() -> DataFrame:
    teams = CommonTeamYears().get_data_frames()[0]
    return teams


def get_team_season_roster(season_string: str, team_id: str) -> DataFrame:
    dfs = CommonTeamRoster(season=season_string, team_id=team_id)
    roster = dfs.get_data_frames()[0]
    return roster


def get_clean_active_roster(raw_roster_df: DataFrame):
    pass