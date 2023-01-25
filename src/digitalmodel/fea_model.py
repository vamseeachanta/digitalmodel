from digitalmodel.common.FEAComponents import FEAComponents


def fea_model(cfg):

    fea = FEAComponents(cfg)

    fea.get_raw_data()
    fea.prepare_FEAModel()
    fea.save_output()
    fea.load_variations()
    # fea.prepare_visualizations()

    return cfg

