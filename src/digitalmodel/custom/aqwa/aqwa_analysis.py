import os
import math
import pandas as pd

import logging
from digitalmodel.custom.aqwa.aqwa_analysis_ef_server import AqwaEFServer

aq_ef = AqwaEFServer()

class AqwaAnalysis:

    def __init__(self):
        pass

    def analysis_router(self, cfg):

        if cfg['analysis_settings']['method'] == 'ef_server':
            aq_ef.ef_server_router(cfg)

        return cfg

