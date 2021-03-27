# builtin imports
import re
from datetime import datetime
from pathlib import Path

# third party imports
import pandas as pd
import requests as req
from bs4 import BeautifulSoup

# first party
from normalize_name import *


def lineups_from_matchups_html(matchups_html):
    lineup_players_list = []
    for matchup_html in matchups_html:
        matchup_teams_html = matchup_html.find("div", attrs={"class": "lineup__teams"})
        matchup = "".join([x.text for x in matchup_teams_html.find_all("div")])
        matchup_teams = [
            {
                "TEAM_ABBREVIATION": x.find("div").text,
                "HOME_AWAY": x.get("class")[-1],
                "MATCHUP": matchup,
            }
            for x in matchup_teams_html.find_all("a")
        ]
        for team in matchup_teams:
            abbrev = normalize_name(team.get("TEAM_ABBREVIATION"))
            home_away = team.get("HOME_AWAY")
            lineup_raw = [
                x for x in matchup_html.find_all("ul") if home_away in str(x)
            ][0]
            players_raw = [
                x for x in lineup_raw.find_all("li") if "lineup__player" in str(x)
            ]
            lineup_status = lineup_raw.find(
                "li", attrs={"class": re.compile("lineup__status")}
            ).get("class")[-1]
            for idx, player in enumerate(players_raw):
                player_dict = {
                    "TEAM_ABBREVIATION": abbrev,
                    "HOME_AWAY": home_away,
                    "MATCHUP": matchup,
                    "LINEUP_DESC": lineup_status if idx <= 4 else None,
                    "TO_PLAY_DESC": player.get("title"),
                    "PLAYER_NAME": player.find("a").get("title"),
                    "NAME_SHORT": player.find("a").text,
                    "STARTING_POSITION": player.find(
                        "div", attrs={"class", "lineup__pos"}
                    ).text,
                }
                lineup_players_list.append(player_dict)
    return lineup_players_list


def main():
    file_create = str(datetime.now())
    file_date = datetime.now().strftime(r"%Y-%m-%d-%H").split("-")
    base_path = Path(__file__).parent.parent.absolute()
    raw_current_filename = Path.joinpath(
        base_path, "data", "01_raw", "nba_lineups", "rotowire.html"
    )
    curated_current_filename = Path.joinpath(
        base_path, "data", "02_curated", "nba_lineups", "rotowire.csv"
    )
    raw_filename = Path.joinpath(
        base_path, "data", "01_raw", "nba_lineups", *file_date, "rotowire.html"
    )
    curated_filename = Path.joinpath(
        base_path, "data", "02_curated", "nba_lineups", *file_date, "rotowire.csv"
    )
    base_url = "https://www.rotowire.com/basketball/nba-lineups.php"
    response = req.get(base_url)
    soup = BeautifulSoup(response.text, features="html.parser")
    lineups_html = soup.find("div", attrs={"class": "lineups"})
    Path(raw_filename).parent.mkdir(parents=True, exist_ok=True)
    with open(raw_filename, "w", encoding="utf-8") as fp:
        fp.write(str(lineups_html))
    with open(raw_current_filename, "w", encoding="utf-8") as fp:
        fp.write(str(lineups_html))

    matchups_html = [
        x
        for x in lineups_html.find_all("div", attrs={"class": "lineup__box"})
        if "lineup__teams" in str(x)
    ]
    lineup_players_list = lineups_from_matchups_html(matchups_html)
    lineup_df = pd.DataFrame(lineup_players_list).drop_duplicates()
    lineup_df["CREATE_DTTM"] = file_create
    Path(curated_filename).parent.mkdir(parents=True, exist_ok=True)
    lineup_df.to_csv(curated_filename, index=False)
    lineup_df.to_csv(curated_current_filename, index=False)


if __name__ == "__main__":
    main()
