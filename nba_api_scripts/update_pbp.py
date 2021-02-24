import nba_api.stats.endpoints.playbyplayv2 as pbp
import pandas as pd
import numpy as np
import time
import os

gamelog_path = os.path.join(
    os.path.dirname(os.path.dirname(os.path.realpath(__file__))),
    "data",
    "nba_gamelogs",
)
pbp_path = os.path.join(
    os.path.dirname(os.path.dirname(os.path.realpath(__file__))), "data", "nba_pbp"
)

gamelogs = [f for f in os.listdir(gamelog_path) if not f.startswith(".")]
pbps = [
    os.path.splitext(x)[0]
    for x in [y for y in os.listdir(pbp_path) if not y.startswith(".")]
]

for gl in gamelogs:
    print(gl)
    csv_path = gamelog_path + gl
    gl_df = pd.read_csv(csv_path, dtype="str")
    game_ids = list(gl_df.GAME_ID.unique())
    game_ids = list(np.setdiff1d(game_ids, pbps))

    if len(game_ids) == 0:
        print("No gamelogs to update for this season!")
        continue
    else:
        print("Updating pbp for " + str(len(game_ids)) + " games...")

    for g in game_ids:
        time.sleep(2)
        print(g)
        playbyplay = pbp.PlayByPlayV2(game_id=str(g), start_period=0, end_period=0)
        pbp_df = playbyplay.get_data_frames()
        pbp_df = pbp_df[0]
        csv_string = pbp_path + g + ".csv"
        pbp_df.to_csv(csv_string)
