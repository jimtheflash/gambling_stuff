from pathlib import Path

import pandas as pd

from nba_api.stats.endpoints.commonteamroster import CommonTeamRoster
from nba_api.stats.endpoints.commonteamyears import CommonTeamYears


def get_active_teams() -> pd.DataFrame:
    teams = CommonTeamYears().get_data_frames()[0]
    return teams


def get_team_season_roster(season_string: str, team_id: str) -> pd.DataFrame:
    dfs = CommonTeamRoster(season=season_string, team_id=team_id)
    roster = dfs.get_data_frames()[0]
    return roster


def get_clean_active_roster(raw_roster_df: pd.DataFrame):
    base_data_path = Path(__file__).parent.parent.joinpath("data")
    teams_df = pd.read_csv(
        Path(base_data_path).joinpath("01_raw", "nba_teams", "current.csv")
    )[["TEAM_ID", "ABBREVIATION"]].rename({"ABBREVIATION": "TEAM_ABBREVIATION"})
    clean_df = raw_roster_df.join(teams_df, on="PLAYER_ID")
    player_info_path = Path(base_data_path).joinpath("nba_player_info")
    # team_players_files = [x for x in ]
    final_cols = [
        "TEAM_ID",
        "TEAM_ABBREVIATION",
        "TEAM_NAME",
        "PLAYER_ID",
        "PLAYER_NAME",
        "PLAYER_SLUG",
        "HEIGHT",
        "WEIGHT",
        "AGE",
    ]
    # get additional player info
    # get team abbreviations

    pass