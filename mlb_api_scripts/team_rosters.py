import json
from datetime import date
from pathlib import Path

import pandas as pd
import requests as req
from statsapi import lookup_team, roster

from common import BASE_URL
from get_logger import get_logger


BASE_DATA_PATH = Path(__file__).parent.parent.joinpath("data")
ARCHIVE = str(date.today()).split("-")


def get_teams(season: int = date.today().year) -> dict:
    teams_response = req.get(f"{BASE_URL}/teams?season={season}&sportIds=1")
    teams = teams_response.json()
    return teams


def parse_teams(teams_json: dict) -> pd.DataFrame:
    data_rows = []
    for team in teams_json["teams"]:
        row = (
            team["id"],
            team["name"],
            team["teamCode"],
            team["abbreviation"],
            team["league"]["name"],
            team["division"]["name"],
        )
        data_rows.append(row)
    teams_df_cols = ["team_id", "team_name", "code", "abbrev", "league", "division"]
    teams_df = pd.DataFrame(data_rows, columns=teams_df_cols)
    return teams_df


def get_team_season_roster(team_id: int, season: int = date.today().year) -> dict:
    roster_response = req.get(f"{BASE_URL}/teams/{team_id}/roster?&season={season}")
    team_roster = roster_response.json()
    return team_roster


def parse_team_season_roster(team_roster_json: list) -> pd.DataFrame:
    roster_data_rows = []
    for player in team_roster_json["roster"]:
        player_row = (
            player["person"]["id"],
            player["person"]["fullName"],
            player["position"]["abbreviation"],
            player["parentTeamId"],
        )
        roster_data_rows.append(player_row)
    roster_df_cols = ["player_id", "name", "position", "team_id"]
    roster_df = pd.DataFrame(roster_data_rows, columns=roster_df_cols)
    return roster_df


def main(season: int = date.today().year) -> None:
    teams_json = get_teams(season=season)
    with open(
        Path(BASE_DATA_PATH).joinpath("01_raw", "mlb", "teams", "current.json"), "w"
    ) as fout:
        json.dump(teams_json, fout, indent=4)
    teams_df = parse_teams(teams_json=teams_json)
    teams_df.to_csv(
        Path(BASE_DATA_PATH).joinpath("02_curated", "mlb", "teams", "current.csv"),
        index=False,
    )
    team_ids = [_["id"] for _ in teams_json["teams"]]
    roster_dfs = []
    for team_id in team_ids:
        roster_json = get_team_season_roster(team_id=team_id, season=season)
        with open(
            Path(BASE_DATA_PATH).joinpath(
                "01_raw", "mlb", "rosters", f"{team_id}.json"
            ),
            "w",
        ) as fout:
            json.dump(roster_json, fout, indent=4)
        roster_df = parse_team_season_roster(team_roster_json=roster_json)
        roster_dfs.append(roster_df)
    all_roster_dfs = pd.concat(roster_dfs).sort_values(["team_id", "player_id"])
    all_roster_dfs.to_csv(
        Path(BASE_DATA_PATH).joinpath("02_curated", "mlb", "rosters", "current.csv"),
        index=False,
    )


if __name__ == "__main__":
    main()
