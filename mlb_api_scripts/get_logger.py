import logging
import sys


def get_logger(name=__name__):
    logger = logging.getLogger(name)
    logger.setLevel(logging.DEBUG)
    stream = logging.StreamHandler(sys.stdout)
    logger.addHandler(stream)
    # create formatter and add it to the handlers
    formatter = logging.Formatter("%(levelname)s @ %(asctime)s - %(message)s")
    stream.setFormatter(formatter)

    return logger
