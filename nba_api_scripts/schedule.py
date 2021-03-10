import nba_api.stats.endpoints.scoreboardv2 as sb2
import pandas as pd
from datetime import date
import os


def get_day_schedule(game_date: date) -> pd.DataFrame:
    game_date = str(game_date)
    schedule = sb2.ScoreboardV2(game_date=game_date)
    schedule_df = schedule.get_data_frames()[0]

    return schedule_df
