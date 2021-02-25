import nba_api.stats.endpoints.boxscoretraditionalv2 as bs2
import numpy as np
import pandas as pd
import time
import os

gamelog_path = os.path.join(
    os.path.dirname(os.path.dirname(os.path.realpath(__file__))),
    "data",
    "nba_gamelogs",
)
bs_path = os.path.join(
    os.path.dirname(os.path.dirname(os.path.realpath(__file__))), "data", "nba_boxscores"
)

gamelogs = [f for f in os.listdir(gamelog_path) if not f.startswith(".")]
bss = [
    os.path.splitext(x)[0]
    for x in [y for y in os.listdir(bs_path) if not y.startswith(".")]
]

for gl in gamelogs:
    print(gl)
    csv_path = os.path.join(gamelog_path, gl)
    gl_df = pd.read_csv(csv_path, dtype="str")
    game_ids = list(gl_df.GAME_ID.unique())
    game_ids = list(np.setdiff1d(game_ids, bss))

    if len(game_ids) == 0:
        print("No gamelogs to update for this season!")
        continue
    else:
        print("Updating boxscores for " + str(len(game_ids)) + " games...")

    for g in game_ids:
        time.sleep(2)
        print(g)
        boxscore = bs2.BoxScoreTraditionalV2(game_id=str(g))
        bs_df = boxscore.get_data_frames()
        bs_df = bs_df[0]
        csv_string = os.path.join(bs_path, g + ".csv")
        bs_df.to_csv(csv_string)
