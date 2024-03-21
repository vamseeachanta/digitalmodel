class UmbilicalAnalysis():

    def __init__(self):
        pass

    def perform_analysis(self, cfg):
        if cfg['installation_phases']:
            self.installation_phases(cfg)
        # if cfg['default']['analysis']['end'] == 'first':
        #     self.first_end_analysis()
        # elif cfg['default']['analysis']['end'] == 'second':
        #     self.second_end_analysis()
        # elif cfg['default']['analysis']['end'] == 'second':
        #     self.installation_analysis()
        else:
            raise NotImplementedError("Analysis not implemented.")

        return cfg

    def first_end_analysis(self):
        pass

    def second_end_analysis(self):
        pass

    def installation_phases(self, cfg):
        for phase in cfg['installation']['phase']:
            self.installation_phase(cfg, phase)

        return cfg

    def installation_phase(self, cfg, phase):
        for step in phase['step']:
            self.installation_step(cfg, step)

        return cfg

    def installation_step(self, cfg, step):
        for target_settings in step['target']:
            target_value = self.get_target_result(cfg, target_settings)

        # Write step file

    def get_target_result(self, cfg, target_settings):
        pass