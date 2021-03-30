from pathlib import Path
from time import sleep
from datetime import date
from random import uniform
import pandas as pd
from season import get_current_season, format_season_string
from team import get_teams, get_team_season_roster


def main() -> bool:
    teams_df = get_teams()
    sleep(uniform(1.5, 2.5))
    season = get_current_season()
    season_str = format_season_string(year=season)
    sleep(uniform(1.5, 2.5))
    team_ids = list(teams_df.query(f'MAX_YEAR == "{season}"')["TEAM_ID"])

    for team_id in team_ids:
        raw_roster = get_team_season_roster(season_str, team_id)
        raw_roster_path = Path(__file__).parent.parent.joinpath(
            "data", "01_raw", "nba_rosters", f"{team_id}.csv.gz"
        )
        today = str(date.today()).split("-")
        raw_archive_roster_path = Path(__file__).parent.parent.joinpath(
            "data", "01_raw", "nba_rosters", *today, f"{team_id}.csv.gz"
        )
        write_paths = [raw_roster_path, raw_archive_roster_path]
        for write_path in write_paths:
            Path(write_path).parent.mkdir(parents=True, exist_ok=True)
            raw_roster.to_csv(
                write_path,
                index=False,
                compression="gzip",
            )
        sleep(uniform(1.5, 2.5))


# def get_clean_active_roster(raw_roster_df: pd.DataFrame):
#     base_data_path = Path(__file__).parent.parent.joinpath("data")
#     teams_df = pd.read_csv(
#         Path(base_data_path).joinpath("01_raw", "nba_teams", "current.csv")
#     )[["TEAM_ID", "ABBREVIATION"]].rename({"ABBREVIATION": "TEAM_ABBREVIATION"})
#     clean_df = raw_roster_df.join(teams_df, on="TEAM_ID")
#     final_cols = [
#         "TEAM_ID",
#         "TEAM_ABBREVIATION",
#         "TEAM_NAME",
#         "PLAYER_ID",
#         "PLAYER_NAME",
#         "PLAYER_SLUG",
#         "HEIGHT",
#         "WEIGHT",
#         "AGE",
#     ]
#     # get additional player info
#     # get team abbreviations

#     pass


if __name__ == "__main__":
    main()
