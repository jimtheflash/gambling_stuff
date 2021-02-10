import nba_api.stats.endpoints.playergamelogs as pg
import pandas as pd
import time

season_starts = range(15,21) 

for s in season_starts:
  print(s)
  time.sleep(3)
  season_end = s+1
  season_string = str('20') + str(s) + '-' + str(season_end)
  seas_gamelog = pg.PlayerGameLogs(season_nullable=season_string)
  gl_df = seas_gamelog.get_data_frames()
  gl_df = gl_df[0]
  gl_df['fd'] = gl_df.PTS + (1.2*gl_df.REB) + (1.5*gl_df.AST) + (3*gl_df.BLK) + (3*gl_df.STL) + (-1*gl_df.TOV)
  gl_df['dk'] = gl_df.PTS + (0.5*gl_df.FG3M) + (1.25*gl_df.REB) + (1.5*gl_df.AST) + (2*gl_df.BLK) + (2*gl_df.STL) + (-0.5*gl_df.TOV) + (1.5*gl_df.DD2) + (3*gl_df.TD3)
  csv_string = './data/nba_gamelogs/nba_gamelogs_' + season_string + '.csv'
  gl_df.to_csv(csv_string, index=False)
