# builtin imports
from datetime import date
from pathlib import Path

# third party imports
import pandas as pd
import requests as req
from bs4 import BeautifulSoup


base_url = "https://www.rotowire.com/basketball/nba-lineups.php"

response = req.get(base_url)

soup = BeautifulSoup(response.text, features="html.parser")

lineups_html = soup.find("div", attrs={"class": "lineups"})

file_date = str(date.today()).split("-")

base_path = Path(__file__).parent.parent.absolute()

raw_filename = Path.joinpath(
    base_path, "data", "01_raw", "nba_lineups", *file_date, "rotowire.html"
)

curated_filename = Path.joinpath(
    base_path, "data", "02_curated", "nba_lineups", *file_date, "rotowire.csv"
)

Path(raw_filename).parent.mkdir(parents=True, exist_ok=True)
with open(raw_filename, "w", encoding="utf-8") as fp:
    fp.write(str(lineups_html))

matchups_html = [
    x
    for x in lineups_html.find_all("div", attrs={"class": "lineup__box"})
    if "lineup__teams" in str(x)
]

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
        abbrev = team.get("TEAM_ABBREVIATION")
        home_away = team.get("HOME_AWAY")
        lineup_raw = [x for x in matchup_html.find_all("ul") if home_away in str(x)][0]
        players_raw = [
            x for x in lineup_raw.find_all("li") if "lineup__player" in str(x)
        ]
        for player in players_raw:
            player_dict = {
                "TEAM_ABBREVIATION": abbrev,
                "HOME_AWAY": home_away,
                "MATCHUP": matchup,
                "TO_PLAY_DESC": player.get("title"),
                "PLAYER_NAME": player.find("a").get("title"),
                "NAME_SHORT": player.find("a").text,
                "STARTING_POSITION": player.find(
                    "div", attrs={"class", "lineup__pos"}
                ).text,
            }
            lineup_players_list.append(player_dict)

lineup_df = pd.DataFrame(lineup_players_list).drop_duplicates()
Path(curated_filename).parent.mkdir(parents=True, exist_ok=True)
lineup_df.to_csv(curated_filename, index=False)


def main():
    base_url = "https://www.rotowire.com/basketball/nba-lineups.php"
    response = req.get(base_url)
    soup = BeautifulSoup(response.text, features="html.parser")
    lineups_html = soup.find("div", attrs={"class": "lineups"})
    file_date = str(date.today()).split("-")
    base_path = Path(__file__).parent.parent.absolute()
    raw_filename = Path.joinpath(
        base_path, "data", "01_raw", "nba_lineups", *file_date, "rotowire.html"
    )
    curated_filename = Path.joinpath(
        base_path, "data", "02_curated", "nba_lineups", *file_date, "rotowire.csv"
    )
    Path(raw_filename).parent.mkdir(parents=True, exist_ok=True)
    with open(raw_filename, "w", encoding="utf-8") as fp:
        fp.write(str(lineups_html))

    matchups_html = [
        x
        for x in lineups_html.find_all("div", attrs={"class": "lineup__box"})
        if "lineup__teams" in str(x)
    ]


if __name__ == "__main__":
    main()