from digitalmodel.custom.aqwa_post_process import AqwaPostProcess

app = AqwaPostProcess()


class Aqwa:

    def __init__(self):
        pass
        

    def router(self, cfg):

        app.post_process_router(cfg)

        return cfg

