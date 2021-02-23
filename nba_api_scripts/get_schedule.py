import nba_api.stats.endpoints.scoreboardv2 as sb2
import pandas as pd
from datetime import date

date_string = str(date.today())
schedule_path = './data/nba_schedules/'

scoreboard = sb2.ScoreboardV2(game_date = date_string)
all_dfs = scoreboard.get_data_frames()
schedule_df = all_dfs[0]

# stripping the dashes out of the date for easier sorting and tidyness
output_string = schedule_path + date_string.replace('-', '') + '.csv'
schedule_df.to_csv(output_string)
