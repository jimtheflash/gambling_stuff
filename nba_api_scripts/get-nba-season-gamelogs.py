import nba_api.stats.endpoints.playergamelogs as pg
import pandas as pd

seas_gamelog = pg.PlayerGameLogs(season_nullable="2020-21")
gl_df = seas_gamelog.get_data_frames()
gl_df = gl_df[0]

gl_df['fd'] = gl_df.PTS + (1.2*gl_df.REB) + (1.5*gl_df.AST) + (3*gl_df.BLK) + (3*gl_df.STL) + (-1*gl_df.TOV)
gl_df['dk'] = gl_df.PTS + (0.5*gl_df.FG3M) + (1.25*gl_df.REB) + (1.5*gl_df.AST) + (2*gl_df.BLK) + (2*gl_df.STL) + (-0.5*gl_df.TOV) + (1.5*gl_df.DD2) + (3*gl_df.TD3)

gl_df.to_csv('/Users/jim/Desktop/nba_2020-21.csv')
