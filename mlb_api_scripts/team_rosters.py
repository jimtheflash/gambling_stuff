import json
from datetime import date, datetime
from glob import glob
from pathlib import Path

import pandas as pd
import requests as req
from statsapi import lookup_team, roster

from args import get_cli_args
from common import BASE_URL
from get_logger import get_logger

logger = get_logger(__name__)

CURRENT_SEASON = date.today().year
NOW_EPOCH = int(datetime.now().timestamp())
BASE_PATH = Path(__file__).parent.parent
RAW_PATH = Path(BASE_PATH).joinpath("data", "01_raw", "mlb")
CURATED_PATH = Path(BASE_PATH).joinpath("data", "02_curated", "mlb")


def get_teams(season: int) -> dict:
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


def get_team_season_roster(team_id: int, season: int, date_as_mdy: str) -> dict:
    roster_response = req.get(
        f"{BASE_URL}/teams/{team_id}/roster?&season={season}&date={date_as_mdy}"
    )
    team_roster = roster_response.json()
    return team_roster


def parse_team_season_roster(team_roster_json: dict) -> pd.DataFrame:
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


def get_raw_team_data(logical_date: str = str(date.today())) -> list:
    ymd = logical_date.split("-")
    mdy = f"{ymd[1]}/{ymd[2]}/{ymd[0]}"
    season = ymd[0]
    write_files = []
    teams_json = get_teams(season=season)
    write_files.append(
        [Path(RAW_PATH).joinpath("teams", *ymd).with_suffix(".json"), teams_json]
    )

    team_ids = [_["id"] for _ in teams_json["teams"]]
    roster_jsons = []
    for team_id in team_ids:
        team_roster_path = Path(RAW_PATH).joinpath(
            "rosters", *ymd, f"teamid_{team_id}.json"
        )
        roster_json = get_team_season_roster(
            team_id=team_id, season=season, date_as_mdy=mdy
        )
        write_files.append([team_roster_path, roster_json])
        roster_jsons.append(roster_json)

    for write_file in write_files:
        write_file[0].parent.mkdir(parents=True, exist_ok=True)
        with open(write_file[0], "w") as fout:
            json.dump(write_file[1], fout, indent=4)

    return [teams_json, roster_jsons]


def main(logical_date: str = str(date.today())) -> None:
    ymd = logical_date.split("-")
    season = int(ymd[0])
    teams_json, roster_jsons = get_raw_team_data(logical_date)
    write_files = []
    teams_df = parse_teams(teams_json)
    write_files.append([Path(CURATED_PATH).joinpath(*ymd, "teams.csv"), teams_df])
    roster_dfs = []
    for roster_json in roster_jsons:
        roster_df = parse_team_season_roster(roster_json)
        roster_dfs.append(roster_df)
    all_roster_dfs = pd.concat(roster_dfs).sort_values(["team_id", "player_id"])
    write_files.append(
        [Path(CURATED_PATH).joinpath(*ymd, "rosters.csv"), all_roster_dfs]
    )
    if season == CURRENT_SEASON:
        logger.info("Updating current season files")
        write_files.append([Path(CURATED_PATH).joinpath("teams.csv"), teams_df])
        write_files.append([Path(CURATED_PATH).joinpath("rosters.csv"), all_roster_dfs])
    for write_file in write_files:
        write_file[0].parent.mkdir(parents=True, exist_ok=True)
        write_file[1].to_csv(write_file[0], index=False)


if __name__ == "__main__":
    args = get_cli_args()
    main(args["logical_date"])
