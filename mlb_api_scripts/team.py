from pathlib import Path


import pandas as pd
import requests as req

from .common import BASE_URL, CURRENT_SEASON


def get_teams(season: int = CURRENT_SEASON) -> dict:
    get_teams_url = (
        f"{BASE_URL}/named.team_all_season.bam?sport_code='mlb'&season='{season}'"
    )

    teams_resp = req.get(get_teams_url)
    teams = teams_resp.json()["team_all_season"]["queryResults"]["row"]
    return teams


def get_team_current_roster(team_id: int) -> dict:
    get_roster_url = f"{BASE_URL}/named.roster_40.bam?team_id='{team_id}'"
    roster_resp = req.get(get_roster_url)
    roster = roster_resp.json()
    return roster


def get_team_season_roster(team_id: int) -> dict:
    # dfs = CommonTeamRoster(season=season_string, team_id=team_id)
    # roster = dfs.get_data_frames()[0]
    # return roster
    pass
