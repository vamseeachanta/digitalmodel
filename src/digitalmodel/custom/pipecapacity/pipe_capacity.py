from datetime import datetime
from pipecapacity.common.pipe_components import PipeComponents

def pipe_capacity(cfg):
    t_start = datetime.now()

    pc = PipeComponents(cfg)

    pc.evaluate_pipe_capacity()

    t_end = datetime.now()
    print("Time taken to process: {0} seconds".format((t_end - t_start).seconds))
    return cfg
