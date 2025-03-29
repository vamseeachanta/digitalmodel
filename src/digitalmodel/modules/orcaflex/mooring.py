
class Mooring():
    def __init__(self):
        pass

    def router(self, cfg):
        groups = cfg['analysis']['mooring']['groups']
        for group in groups:
            if group['flag']:
                self.pretension_analysis(group)

    def pretension_analysis(self, group):
        # check yml load
        # utilize model to add moorings
        # utilize winch commands to add pretensions
        
        
        
        pass