import pandas as pd
import nba_api.stats.endpoints.commonplayerinfo as cpi
from json.decoder import JSONDecodeError

from get_logger import get_logger


logger = get_logger(__name__)


def get_player_info(player_id: int) -> pd.DataFrame:
    try:
        player_id = str(player_id)
        playerinfo = cpi.CommonPlayerInfo(player_id=player_id)
        player_df = playerinfo.get_data_frames()[0]

        return player_df
    except JSONDecodeError:
        logger.warning(f"Could not find playerID: {player_id}")
