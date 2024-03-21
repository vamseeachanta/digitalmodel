class UmbilicalAnalysis():

    def __init__(self, cfg):
        self.cfg = cfg
        self.cfg_array = []

    def perform_analysis(self):
        if self.cfg['default']['analysis']['end'] == 'first':
            self.first_end_analysis()
        elif self.cfg['default']['analysis']['end'] == 'second':
            self.second_end_analysis()
        elif self.cfg['default']['analysis']['end'] == 'second':
            self.installation_analysis()

        return self.cfg

    def first_end_analysis(self):
        pass

    def second_end_analysis(self):
        pass

    def installation_analysis(self):
        pass

