import json
from pathlib import Path

NAME_LU_FILE = Path(__file__).parent.parent.joinpath("data", "lu", "nba_team_key.json")


def normalize_name(name: str) -> str:
    with open(NAME_LU_FILE, "r") as fp:
        name_lu = json.load(fp)
    for name_dict in name_lu:
        if name in name_dict["aliases"]:
            name_clean = name_dict.get("name")
        else:
            continue
    return name_clean