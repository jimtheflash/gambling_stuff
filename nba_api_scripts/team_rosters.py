from pathlib import Path
from time import sleep
from glob import glob
from datetime import date
from random import uniform
import pandas as pd
from season import get_current_season, format_season_string
from team import get_teams, get_team_season_roster

BASE_DATA_PATH = Path(__file__).parent.parent.joinpath("data")
ARCHIVE = str(date.today()).split("-")


def get_raw_rosters() -> bool:
    teams_df = get_teams()
    sleep(uniform(1.5, 2.5))
    season = get_current_season()
    season_str = format_season_string(year=season)
    sleep(uniform(1.5, 2.5))
    team_ids = list(teams_df.query(f'MAX_YEAR == "{season}"')["TEAM_ID"])

    for team_id in team_ids:
        raw_roster = get_team_season_roster(season_str, team_id)
        raw_roster_path = Path(BASE_DATA_PATH).joinpath(
            "01_raw", "nba_rosters", f"{team_id}.csv.gz"
        )
        raw_archive_roster_path = Path(BASE_DATA_PATH).joinpath(
            "01_raw", "nba_rosters", *ARCHIVE, f"{team_id}.csv.gz"
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
    return True


def main() -> bool:
    raw_success = get_raw_rosters()
    teams_df = pd.read_csv(
        Path(BASE_DATA_PATH).joinpath("01_raw", "nba_teams", "current.csv")
    )
    teams = teams_df[["TEAM_ID", "ABBREVIATION"]].rename(
        columns={"ABBREVIATION": "TEAM_ABBREVIATION"}
    )
    raw_roster_files = glob(
        str(Path(BASE_DATA_PATH).joinpath("01_raw", "nba_rosters", "*.csv.gz"))
    )
    rosters_df = pd.concat([pd.read_csv(x) for x in raw_roster_files])
    rosters = rosters_df.rename(
        columns={"TeamID": "TEAM_ID", "PLAYER": "PLAYER_NAME", "EXP": "EXPERIENCE"}
    )
    clean_df = pd.merge(rosters, teams, on="TEAM_ID")
    final_cols = [
        "TEAM_ID",
        "TEAM_ABBREVIATION",
        "PLAYER_ID",
        "PLAYER_NAME",
        "PLAYER_SLUG",
        "POSITION",
        "HEIGHT",
        "WEIGHT",
        "AGE",
        "EXPERIENCE",
    ]
    curated_path = Path(BASE_DATA_PATH).joinpath(
        "02_curated", "nba_rosters", "current.csv.gz"
    )
    curated_archive_path = Path(BASE_DATA_PATH).joinpath(
        "02_curated", "nba_rosters", ARCHIVE[0], ARCHIVE[1], f"{ARCHIVE[2]}.csv.gz"
    )
    for path in [curated_path, curated_archive_path]:
        Path(path).parent.mkdir(parents=True, exist_ok=True)
        clean_df[final_cols].to_csv(path, index=False, compression="gzip")


if __name__ == "__main__":
    main()
