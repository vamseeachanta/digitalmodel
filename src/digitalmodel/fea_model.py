from common.ApplicationManager import applicationTimer, setupApplicationRuns


@setupApplicationRuns
@applicationTimer
def fea_model(cfg):

    from common.FEAComponents import FEAComponents
    fea = FEAComponents(cfg)

    fea.get_raw_data()
    fea.prepare_FEAModel()
    fea.save_output()
    fea.load_variations()
    # fea.prepare_visualizations()

    return cfg


if __name__ == '__main__':
    cfg_with_results = fea_model(cfg=None)
