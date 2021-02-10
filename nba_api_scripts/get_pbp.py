import nba_api.stats.endpoints.playergamelogs as pg
import nba_api.stats.endpoints.playbyplayv2 as pbp
import pandas as pd
import time

path_string = '/Users/jim/Desktop/'
seas = '2020-21'
seas_gamelog = pg.PlayerGameLogs(season_nullable=seas)
gl_df = seas_gamelog.get_data_frames()
gl_df = gl_df[0]

gl_path = path_string + 'nba_gamelogs_' + str(seas) + '.csv'
gl_df.to_csv()

gl_df['fd'] = gl_df.PTS + (1.2*gl_df.REB) + (1.5*gl_df.AST) + (3*gl_df.BLK) + (3*gl_df.STL) + (-1*gl_df.TOV)
gl_df['dk'] = gl_df.PTS + (0.5*gl_df.FG3M) + (1.25*gl_df.REB) + (1.5*gl_df.AST) + (2*gl_df.BLK) + (2*gl_df.STL) + (-0.5*gl_df.TOV) + (1.5*gl_df.DD2) + (3*gl_df.TD3)

game_ids = list(gl_df.GAME_ID.unique())

for g in game_ids:
  time.sleep(2)
  print(g)
  playbyplay = pbp.PlayByPlayV2(game_id=g, start_period=1, end_period=1)
  pbp_df = playbyplay.get_data_frames()
  pbp_df = pbp_df[0]
  csv_string = path_string + str(g) + '.csv'
  pbp_df.to_csv(csv_string)
