from pyintegrity.common.API579_components import API579_components


def API579(cfg):
    from datetime import datetime
    t_start = datetime.now()

    api579_components = API579_components(cfg)

    if cfg['default']['Analysis']['GML']['Circumference']:
        cfg = api579_components.gml()

    if cfg['default']['Analysis']['LML']:
        cfg = api579_components.lml()

    if cfg['default']['Analysis']['B31G']:
        cfg = api579_components.b31g()

    t_end = datetime.now()
    print("Time taken to process: {0} seconds".format(
        (t_end - t_start).seconds))

    return cfg