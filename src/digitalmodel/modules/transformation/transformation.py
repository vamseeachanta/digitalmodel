class Transformation:
    def __init__(self):
        pass

    def router(self, cfg=None):
        self.cfg = cfg
        self.get_transformation_groups()
        return self.cfg
