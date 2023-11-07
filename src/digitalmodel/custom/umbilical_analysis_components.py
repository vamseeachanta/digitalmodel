class UmbilicalAnalysis():

    def __init__(self, cfg):
        self.cfg = cfg
        self.cfg_array = []

    def perform_analysis(self):
        if self.cfg['default']['analysis']['end'] == 'first':
            self.first_end_analysis()
        if self.cfg['default']['analysis']['end'] == 'second':
            self.second_end_analysis()

        return self.cfg

    def first_end_analysis(self):
        pass

    def second_end_analysis(self):
        pass
