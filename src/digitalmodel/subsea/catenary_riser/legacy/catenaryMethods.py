# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Legacy calculation methods for catenary riser analysis."""

import math
import pandas as pd
import warnings

from ..models import _range


def catenaryEquation(data):
    """
    DEPRECATED: This legacy dict-based catenary API is deprecated.

    Please use the modern API from digitalmodel.marine_ops.marine_analysis.catenary instead:
        from digitalmodel.marine_ops.marine_analysis.catenary import CatenarySolver, CatenaryInput

    For backward compatibility, use the adapter:
        from digitalmodel.marine_ops.marine_analysis.catenary import catenaryEquation

    This function will be removed in a future version.
    """
    warnings.warn(
        "catenaryEquation is deprecated. "
        "Use marine_engineering.catenary.CatenarySolver for new code, or "
        "marine_engineering.catenary.catenaryEquation for backward compatibility.",
        DeprecationWarning,
        stacklevel=2
    )

    if data.get("F") is not None:
        # Share the corrected E1-E2 force route, with w already in N/m.
        from .catenary_equation import CatenaryCalculator
        CatenaryCalculator().calculate(data)
        data["W"] = data["w"] * data["S"]

        return data

    elif data.get("q") is not None:
        from .catenary_equation import CatenaryCalculator
        return CatenaryCalculator().calculate(data)

    # The X-driven branch is unimplemented; keep it LAST so a re-invocation on a
    # dict whose "X" was populated by the q-branch above still resolves via q.
    elif data.get("X") is not None:
        raise NotImplementedError("Not implemented yet")
    raise ValueError("provide force, weight and depth, or q and depth")


def catenaryForces(data):
    """
    DEPRECATED: This legacy dict-based catenary forces API is deprecated.

    Please use the modern API from digitalmodel.marine_ops.marine_analysis.catenary instead.
    For backward compatibility, use:
        from digitalmodel.marine_ops.marine_analysis.catenary import catenaryForces
    """
    warnings.warn(
        "catenaryForces is deprecated. "
        "Use marine_engineering.catenary.catenaryForces for backward compatibility.",
        DeprecationWarning,
        stacklevel=2
    )

    #  Vertical load on vessel
    Fv = data["weightPerUnitLength"] * data["S"]
    #  Total force along catenary
    F = Fv / (math.sin(math.radians(90 - data["q"])))
    #  Horizontal force along catenary
    Fh = F * math.cos(math.radians(90 - data["q"]))

    data.update({"Fv": Fv, "F": F, "Fh": Fh})

    return data


def sagHogEquation(data):
    # Sag to Buoyancy Configuration
    BendRadius = data["HangOff"]["BendRadius"]
    d = (
        (data["HogBendAboveSeabed"] - data["SagBendElevationAboveSeabed"])
        * abs(data["WeightPerUnitLengthWithBuoyancy"])
        / (
            abs(data["WeightPerUnitLengthWithBuoyancy"])
            + abs(data["WeightPerUnitLengthWithOutBuoyancy"])
        )
    )
    X = BendRadius * math.acosh(d / BendRadius + 1)
    S = BendRadius * math.sinh(X / BendRadius)
    data["SagToBuoyancy"] = {"d": d, "S": S, "X": X, "BendRadius": BendRadius}

    # Buoy to Hog Configuration
    BendRadius = (
        BendRadius
        * data["WeightPerUnitLengthWithOutBuoyancy"]
        / (abs(data["WeightPerUnitLengthWithBuoyancy"]))
    )
    d = (
        (data["HogBendAboveSeabed"] - data["SagBendElevationAboveSeabed"])
        * abs(data["WeightPerUnitLengthWithOutBuoyancy"])
        / (
            abs(data["WeightPerUnitLengthWithBuoyancy"])
            + abs(data["WeightPerUnitLengthWithOutBuoyancy"])
        )
    )
    X = BendRadius * math.acosh(d / BendRadius + 1)
    S = BendRadius * math.sinh(X / BendRadius)
    data["BuoyancyToHog"] = {"d": d, "S": S, "X": X, "BendRadius": BendRadius}

    # Hog to Buoyancy Configuration
    d = (
        data["HogBendAboveSeabed"]
        * abs(data["WeightPerUnitLengthWithOutBuoyancy"])
        / (
            abs(data["WeightPerUnitLengthWithBuoyancy"])
            + abs(data["WeightPerUnitLengthWithOutBuoyancy"])
        )
    )
    X = BendRadius * math.acosh(d / BendRadius + 1)
    S = BendRadius * math.sinh(X / BendRadius)
    data["HogToBuoyancy"] = {"d": d, "S": S, "X": X, "BendRadius": BendRadius}

    # Buoyancy to TouchDown Configuration
    BendRadius = data["HangOff"]["BendRadius"]
    d = (
        data["HogBendAboveSeabed"]
        * abs(data["WeightPerUnitLengthWithBuoyancy"])
        / (
            abs(data["WeightPerUnitLengthWithBuoyancy"])
            + abs(data["WeightPerUnitLengthWithOutBuoyancy"])
        )
    )
    X = BendRadius * math.acosh(d / BendRadius + 1)
    S = BendRadius * math.sinh(X / BendRadius)
    data["BuoyancyToTouchDown"] = {"d": d, "S": S, "X": X, "BendRadius": BendRadius}

    return data


def lazyWaveCatenaryEquation(data):

    #  Hang-off to Sag Section
    catenaryResult = catenaryEquation(data["HangOff"])
    data["HangOff"].update(catenaryResult)

    data = sagHogEquation(data)
    # Summarize the lazy wave catenary
    Fh = data["HangOff"]["BendRadius"] * data["WeightPerUnitLengthWithOutBuoyancy"]
    Fv = Fh + data["WeightPerUnitLengthWithOutBuoyancy"] * data["HangOff"]["S"]

    HangOffToBuoyancy = {
        "S": data["HangOff"]["S"] + data["SagToBuoyancy"]["S"],
        "X": data["HangOff"]["X"] + data["SagToBuoyancy"]["X"],
    }
    Buoyancy = {
        "S": data["BuoyancyToHog"]["S"] + data["HogToBuoyancy"]["S"],
        "X": data["BuoyancyToHog"]["X"] + data["HogToBuoyancy"]["X"],
    }
    BuoyancyToTouchDown = {
        "S": data["BuoyancyToTouchDown"]["S"],
        "X": data["BuoyancyToTouchDown"]["X"],
    }
    HangoffToTDP = {
        "S": HangOffToBuoyancy["S"] + Buoyancy["S"] + BuoyancyToTouchDown["S"],
        "X": HangOffToBuoyancy["X"] + Buoyancy["X"] + BuoyancyToTouchDown["X"],
    }

    data["Summary"] = {
        "HangOffToBuoyancy": HangOffToBuoyancy,
        "Buoyancy": Buoyancy,
        "BuoyancyToTouchDown": BuoyancyToTouchDown,
        "HangoffToTDP": HangoffToTDP,
        "Fh": Fh,
        "Fv": Fv,
    }
    # data['Summary'] = { "HangOffToBuoyancy": HangOffToBuoyancy}

    return data


def lazyWavePlot(data, spacing=10):
    # HangoffToBuoyancy Section
    X = []
    Y = []
    BendRadius = data["lazyWaveCatenaryResult"]["HangOff"]["BendRadius"]
    HorizontalDistance = data["lazyWaveCatenaryResult"]["HangOff"]["X"]
    XRange = data["lazyWaveCatenaryResult"]["Summary"]["HangOffToBuoyancy"]["X"]
    XCoordinateAdjustment = 0
    YCoordinateAdjustment = (
        BendRadius
        + data["lazyWaveCatenaryResult"]["HangOff"]["d"]
        + data["LazyWaveCatenaryDefinition"]["HangoffBelowMeanSeaLevel"]
    )
    for i in range(0, spacing + 1, 1):
        X.append(XCoordinateAdjustment + i * XRange / spacing)
        Y.append(
            BendRadius * (math.cosh((X[i] - HorizontalDistance) / BendRadius))
            - YCoordinateAdjustment
        )

    data["lazyWaveCatenaryResult"]["Summary"]["HangOffToBuoyancy"]["PlotData"] = {
        "X": X,
        "Y": Y,
    }
    PlotData = {"X": X, "Y": Y, "label": "Hang-off to Buoyancy", "SaveAndShow": False}
    genericPlot(PlotData)

    # Buoyancy Section
    X = []
    Y = []
    BendRadius = data["lazyWaveCatenaryResult"]["BuoyancyToHog"]["BendRadius"]
    HorizontalDistance = (
        data["lazyWaveCatenaryResult"]["HangOff"]["X"]
        + data["lazyWaveCatenaryResult"]["SagToBuoyancy"]["X"]
        + data["lazyWaveCatenaryResult"]["BuoyancyToHog"]["X"]
    )
    XCoordinateAdjustment = (
        data["lazyWaveCatenaryResult"]["HangOff"]["X"]
        + data["lazyWaveCatenaryResult"]["SagToBuoyancy"]["X"]
    )

    XRange = data["lazyWaveCatenaryResult"]["Summary"]["Buoyancy"]["X"]
    YCoordinateAdjustment = (
        -BendRadius
        + data["lazyWaveCatenaryResult"]["HangOff"]["d"]
        - (
            data["LazyWaveCatenaryDefinition"]["HogBendAboveSeabed"]
            - data["LazyWaveCatenaryDefinition"]["SagBendElevationAboveSeabed"]
        )
        + data["LazyWaveCatenaryDefinition"]["HangoffBelowMeanSeaLevel"]
    )
    for i in range(0, spacing + 1, 1):
        X.append(XCoordinateAdjustment + XRange * i / spacing)
        Y.append(
            -BendRadius * (math.cosh((-X[i] + HorizontalDistance) / BendRadius))
            - YCoordinateAdjustment
        )

    data["lazyWaveCatenaryResult"]["Summary"]["Buoyancy"]["PlotData"] = {"X": X, "Y": Y}
    PlotData = {"X": X, "Y": Y}
    PlotData = {"X": X, "Y": Y, "label": "Buoyancy", "SaveAndShow": False}
    genericPlot(PlotData)

    # BuoyancyToTouchDown Section
    X = []
    Y = []
    BendRadius = data["lazyWaveCatenaryResult"]["BuoyancyToTouchDown"]["BendRadius"]
    HorizontalDistance = data["lazyWaveCatenaryResult"]["Summary"]["HangoffToTDP"]["X"]
    XCoordinateAdjustment = (
        data["lazyWaveCatenaryResult"]["Summary"]["HangoffToTDP"]["X"]
        - data["lazyWaveCatenaryResult"]["BuoyancyToTouchDown"]["X"]
    )
    XRange = data["lazyWaveCatenaryResult"]["BuoyancyToTouchDown"]["X"]
    YCoordinateAdjustment = (
        BendRadius
        + data["LazyWaveCatenaryDefinition"]["HogBendAboveSeabed"]
        + data["LazyWaveCatenaryDefinition"]["HangoffBelowMeanSeaLevel"]
    )
    for i in range(0, spacing + 1, 1):
        X.append(XCoordinateAdjustment + XRange * i / spacing)

        Y.append(
            BendRadius * math.cosh((X[i] - HorizontalDistance) / BendRadius)
            - BendRadius
            - data["LazyWaveCatenaryDefinition"]["VerticalDistance"]
            - data["LazyWaveCatenaryDefinition"]["HangoffBelowMeanSeaLevel"]
        )

    data["lazyWaveCatenaryResult"]["Summary"]["BuoyancyToTouchDown"]["PlotData"] = {
        "X": X,
        "Y": Y,
    }
    # PlotData = {"X": X, "Y": Y, "label": "Buoyancy to TDP", "SaveAndShow": True, "fileName": "results\\" + "SLWR_" + data['FileName']}
    PlotData = {
        "X": X,
        "Y": Y,
        "label": "Buoyancy to TDP",
        "SaveAndShow": True,
        "fileName": data["Analysis"]["result_folder"]
        + "SLWR_"
        + data["Analysis"]["file_name"],
    }
    genericPlot(PlotData)

    return data


def simple_catenary_plot(data, spacing=10):
    """Plot angle-route geometry with BendRadius relative to mean sea level."""
    # HangoffToTouchDown Section
    X = []
    Y = []
    BendRadius = data["catenaryResult"]["BendRadius"]
    HorizontalDistance = data["catenaryResult"]["X"]
    XRange = data["catenaryResult"]["X"]
    XCoordinateAdjustment = 0
    YCoordinateAdjustment = (
        BendRadius
        + data["catenaryResult"]["d"]
        + data["commonDefinition"]["HangoffBelowMeanSeaLevel"]
    )
    for i in range(0, spacing + 1, 1):
        X.append(XCoordinateAdjustment + i * XRange / spacing)
        Y.append(
            BendRadius * (math.cosh((X[i] - HorizontalDistance) / BendRadius))
            - YCoordinateAdjustment
        )

    data["catenaryResult"]["PlotData"] = {"X": X, "Y": Y}
    PlotData = {
        "X": X,
        "Y": Y,
        "label": "Catenary",
        "ylabel": "Elevation relative to mean sea level [m]",
        "SaveAndShow": True,
        "fileName": data["Analysis"]["result_folder"]
        + "SCR_"
        + data["Analysis"]["file_name"],
    }
    genericPlot(PlotData)

    return data


def genericPlot(data):
    import matplotlib.pyplot as plt
    if data["label"] is None:
        plt.plot(data["X"], data["Y"])
    else:
        plt.plot(data["X"], data["Y"], label=data["label"])
    # it creates plot X-axis name, fontsize and colour
    plt.xlabel("Horizontal distance[m]", fontsize=12, fontweight="bold", color="black")
    # it creates plot Y-axis name, fontsize and colour
    plt.ylabel(
        data.get("ylabel", "Distance from seabed [m]"),
        fontsize=12, fontweight="bold", color="black"
    )
    # it creates plot tittle, color and size
    plt.title(
        "Catenary Mooring Line Shape", fontsize=14, fontweight="bold", color="black"
    )
    # Gridlines
    plt.grid()
    if data["SaveAndShow"]:
        # plt.savefig(data["fileName"].replace(".", ""), dpi=800)
        plt.savefig(data["fileName"], dpi=800)
        plt.legend(
            loc="upper center",
            bbox_to_anchor=(0.5, -0.05),
            fancybox=True,
            shadow=True,
            ncol=5,
        )


def buoyancyProperties(cfg, WeightPerUnitLengthWithOutBuoyancy):
    """Size a concentric annulus with configured gravity, not an empirical factor.

    Basis: OpenStax University Physics Vol. 1 (2016), section 14.4,
    Archimedes principle F_B = weight of displaced fluid; engineering-basis.md E4.
    https://openstax.org/books/university-physics-volume-1/pages/14-4-archimedes-principle-and-buoyancy
    Uplift per length = (rho_water-rho_foam)*g*pi/4*(D_o**2-D_i**2).
    Inch conversion: NIST SP 811 (2008), Appendix B.8, length table.

    Geometry is in inches; densities kg/m3, weights N/m and acceleration m/s2.
    Supply exactly one of thickness >= 0 or buoyancy factor >= 0. The factor
    specifies uplift divided by the positive reference-fluid bare weight.
    WeightPerUnitLengthWithOutBuoyancy is that reference weight, as supplied by
    the legacy caller; cfg['catenaryResult']['FluidFilled'] supplies actual weight.
    Actual-fluid bare weight may differ; the same uplift is subtracted from it.
    Discrete coverage is percent in (0,100], preserving mean annular volume.
    Strake effects, end caps and module attachments are outside this annular model.
    """
    from .pipeProperties import INCH_TO_METRE

    uniform = cfg["commonDefinition"]["UniformBuoyancy"]
    discrete = cfg["commonDefinition"]["DiscreteBuoyancy"]
    thickness, factor = uniform["Thickness"], uniform["BuoyancyFactor"]
    if (thickness is None) == (factor is None):
        raise ValueError("supply exactly one Thickness or BuoyancyFactor")
    gravity = cfg["default"]["Constants"]["g"]
    _range("g", gravity, positive=True)
    inner = cfg["InsulationSection"]["OD"]
    _range("OD", inner, positive=True)
    water = cfg["Material"]["SeaWater"]["Rho"]
    foam = cfg["Material"]["Buoyancy"]["Rho"]
    _range("SeaWater Rho", water, positive=True)
    _range("Buoyancy Rho", foam)
    if water <= foam:
        raise ValueError("buoyancy material density must be less than seawater density")
    coverage = discrete["BuoyancyCoverage"]
    _range("BuoyancyCoverage", coverage, positive=True)
    if coverage > 100:
        raise ValueError("BuoyancyCoverage must not exceed 100 percent")
    reference_weight = WeightPerUnitLengthWithOutBuoyancy
    _range("reference weightPerUnitLength", reference_weight, positive=True)
    actual_weight = cfg["catenaryResult"]["FluidFilled"]["weightPerUnitLength"]
    _range("actual weightPerUnitLength", actual_weight)
    force_per_inch_area = (water - foam) * gravity * INCH_TO_METRE**2
    if thickness is not None:
        _range("Thickness", thickness)
        outer = inner + 2 * thickness
        uplift = force_per_inch_area * math.pi / 4 * (outer**2 - inner**2)
        factor = uplift / reference_weight
    else:
        _range("BuoyancyFactor", factor)
        uplift = factor * reference_weight
        outer = math.sqrt(inner**2 + 4 * uplift / (math.pi * force_per_inch_area))
        thickness = (outer - inner) / 2
    discrete_outer = math.sqrt(inner**2 + 100 / coverage * (outer**2 - inner**2))
    for name, value in (("BuoyancyOD", outer), ("discrete BuoyancyOD", discrete_outer),
                        ("uplift", uplift)):
        _range(name, value)

    # All checks precede updates; invalid inputs leave the caller unchanged.
    uniform.update(Thickness=thickness, BuoyancyFactor=factor, BuoyancyOD=outer)
    discrete.update(BuoyancyOD=discrete_outer, Thickness=(discrete_outer-inner)/2)
    cfg["lazyWaveCatenaryResult"] = {
        "WeightPerUnitLengthWithOutBuoyancy": actual_weight,
        "WeightPerUnitLengthWithBuoyancy": actual_weight-uplift,
    }
    return cfg


def gethorizontalDistanceHangofftoTDPrange(cfg):
    """Sample positive declinations at 0.01 degree spacing; preserve fractions.

    The integer conversion selects an exclusive upper grid bound, not a
    rounded input angle. E1/E3 in engineering-basis.md govern each sample.
    Returned coordinates retain the legacy three-decimal reporting precision;
    the generated mesh remains unqualified for native execution.
    """
    layback_df = pd.DataFrame(
        columns=["q", "X", "S", "BendRadius", "TotalLength", "MeshSections"]
    )
    steps_in_degrees = 100
    if (
        "range" in cfg["simpleCatenaryDefinition"]["horizontalDistanceHangofftoTDP"]
        and len(
            cfg["simpleCatenaryDefinition"]["horizontalDistanceHangofftoTDP"]["range"]
        )
        > 0
    ):

        layback_range = cfg["simpleCatenaryDefinition"][
            "horizontalDistanceHangofftoTDP"
        ]["range"]

        if cfg["catenaryResult"]["X"] > layback_range[-1]:
            q_max = int(cfg["catenaryResult"]["q"]) + 1
        else:
            q_max = 45

        for q_index in range(1, q_max * steps_in_degrees):
            q = q_index / steps_in_degrees
            catenaryEquation_cfg = {
                "d": cfg["simpleCatenaryDefinition"]["verticalDistance"],
                "F": cfg["simpleCatenaryDefinition"]["axialLineForce"],
                "q": q,
                "X": None,
            }
            catenaryResult = catenaryEquation(catenaryEquation_cfg)
            total_length = (
                catenaryResult["S"]
                + cfg["commonDefinition"]["TDPToAnchor"]["SCR"]
                + cfg["commonDefinition"]["LengthBeforeHangoff"]
            )
            mesh_sections = [
                {
                    "Length": cfg["commonDefinition"]["LengthBeforeHangoff"] + 25,
                    "TargetSegmentLength": 0.2,
                },
                {"Length": 5, "TargetSegmentLength": 0.5},
                {"Length": 10, "TargetSegmentLength": 1.0},
                {
                    "Length": round(catenaryResult["S"] - 40 - 60, 1),
                    "TargetSegmentLength": 2.0,
                },
                {"Length": 10, "TargetSegmentLength": 1.0},
                {"Length": 180, "TargetSegmentLength": 0.5},
                {"Length": 20, "TargetSegmentLength": 1.0},
                {
                    "Length": cfg["commonDefinition"]["TDPToAnchor"]["SCR"] - 150,
                    "TargetSegmentLength": 2.0,
                },
            ]
            layback_df.loc[q_index] = [
                catenaryResult["q"],
                catenaryResult["X"],
                catenaryResult["S"],
                catenaryResult["BendRadius"],
                total_length,
                mesh_sections,
            ]

            if catenaryResult["X"] > layback_range[-1]:
                break

        result_index_range = []
        for layback_value in layback_range:
            result_index_range.append(layback_df["X"].sub(layback_value).abs().idxmin())

        layback_df = layback_df.round(3)
        rangeresult = layback_df.loc[result_index_range].to_dict("records")

        return rangeresult
