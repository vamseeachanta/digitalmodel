import logging

# Reader imports
from digitalmodel.custom.aqwa.aqwa_analysis_ef_server import AqwaEFServer
from digitalmodel.custom.aqwa.aqwa_analysis_raos import AqwaRAOs

aq_ef = AqwaEFServer()
aq_raos = AqwaRAOs()

class AqwaAnalysis:

    def __init__(self):
        pass

    def analysis_router(self, cfg):

        if 'analysis_settings' in cfg:
            if cfg['analysis_settings']['method'] == 'ef_server':
                aq_ef.ef_server_router(cfg)
            elif cfg['analysis_settings']['method'] == 'raos':
                cfg = aq_raos.rao_router(cfg)
        else:
            logging.info('Analysis method not specified in the configuration file.')
        
        return cfg

