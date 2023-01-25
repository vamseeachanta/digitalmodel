from common.ApplicationManager import applicationTimer, setupApplicationRuns
from common.FEAComponents import FEAComponents


@setupApplicationRuns
@applicationTimer
def fea_model(cfg):

    fea = FEAComponents(cfg)

    fea.get_raw_data()
    fea.prepare_FEAModel()
    fea.save_output()
    fea.load_variations()
    # fea.prepare_visualizations()

    return cfg


if __name__ == '__main__':
    cfg_with_results = fea_model(cfg=None)
