# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Model entry point; the analytical axial-bar route uses consistent force/length units."""


def fea_model(cfg):
    """Build a model and return cfg; axial_bar inputs use N, mm and MPa, for example.

    ``axial_bar`` selects a fixed-left chain of prismatic elastic elements with
    a signed end force. Its result contains stiffnesses in N/mm, displacements
    in mm and reaction in N for the example units. No external solver is used.
    Other configurations retain the existing external-program model writer.
    """
    if "axial_bar" in cfg:
        from .axial_bar import build_axial_bar_model

        inputs = cfg["axial_bar"]
        inputs["result"] = build_axial_bar_model(
            elements=inputs["elements"],
            tip_load=inputs["tip_load"],
        )
        return cfg

    from digitalmodel.infrastructure.base_solvers.structural.fea.FEAComponents import (
        FEAComponents,
    )

    fea = FEAComponents(cfg)

    fea.get_raw_data()
    fea.prepare_FEAModel()
    fea.save_output()
    fea.load_variations()
    # fea.prepare_visualizations()

    return cfg
