
# Reader imports
from digitalmodel.modules.pipe_capacity.common.pipe_components import PipeComponents

pipe_comps = PipeComponents()

class PipeCapacity:

    def __init__(self) -> None:
        pass

    def router(self, cfg: dict) -> dict:

        pipe_comps.evaluate_pipe_capacity(cfg)

        return cfg
