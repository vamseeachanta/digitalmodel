from loguru import logger as logging


class Mooring:
    def __init__(self):
        pass

    def router(self, cfg_base):
        logging.info("mooring, application ... START")
        if "mooring" in cfg_base and cfg_base["mooring"]["flag"]:
            cfg_base = mooring(cfg_base)
