# Standard library imports
import logging

from digitalmodel.hydrodynamics.aqwa.aqwa_lis_files import AqwaLISFiles
from digitalmodel.hydrodynamics.aqwa.aqwa_reader import AqwaReader

lis_files = AqwaLISFiles()
aqr = AqwaReader()


class AqwaPostProcess:
    def __init__(self):
        pass

    def post_process_router(self, cfg):

        if cfg["type"]["results"]:
            if cfg["result_method"] == "lis":
                cfg = lis_files.router(cfg)
            elif cfg["result_method"] == "aqwareader":
                aqr.router(cfg)

        else:
            logging.info("No option specified ... End Run.")

        return cfg

    def get_visualizations(self, cfg):
        ov = orcaflex_visualizations()
        ov.get_visualizations(cfg)
