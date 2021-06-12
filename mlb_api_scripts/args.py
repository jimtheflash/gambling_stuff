import argparse
from datetime import date
from logging import getLevelName

from get_logger import get_logger


logger = get_logger(__name__)


def get_cli_args():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--dryrun",
        action="store_true",
        help="Omit this argument to actually extract API files",
    )
    parser.add_argument("--logical_date", help="Date for which to get data", default=str(date.today()))
    args = vars(parser.parse_args())
    logger.debug(args)
    return args
