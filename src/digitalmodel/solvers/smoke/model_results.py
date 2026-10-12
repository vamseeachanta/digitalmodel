"""Required mooring-buoy outputs; numeric fidelity is not engineering accuracy."""
import math


def numbers(values):
    result = [float(value) for value in values]
    if not result or not all(math.isfinite(value) for value in result):
        raise ValueError("required numeric results are empty or nonfinite")
    return result


def scalar(value):
    return numbers([value])[0]


def static_results(model, api):
    line, buoy = model["Mooring"], model["Metocean buoy"]
    position, arclength = {}, None
    for axis in ("X", "Y", "Z"):
        graph = line.RangeGraph(axis, api.Period(api.pnStaticState))
        current = numbers(graph.X)
        if arclength is not None and current != arclength:
            raise ValueError("static range graphs have different arc-length grids")
        arclength = current
        position[axis] = numbers(graph.Mean)
        if len(position[axis]) != len(arclength):
            raise ValueError("static position and arc-length counts differ")
    return {
        "line_arclength": arclength, "line_position": position,
        "buoy_position": {axis: scalar(buoy.StaticResult(axis, api.oeBuoy(0, 0, 0)))
                          for axis in ("X", "Y", "Z")},
        "end_tensions": {"A": scalar(line.StaticResult("Effective Tension", api.oeEndA)),
                         "B": scalar(line.StaticResult("Effective Tension", api.oeEndB))},
    }


def histories(model, api, contract):
    period = api.Period(1)
    times = numbers(model.SampleTimes(period))
    grid = contract["period"]
    if grid != {"start": 0, "end": 100, "interval": 0.1, "grid_atol": 1e-9}:
        raise ValueError("unsupported main-stage time-grid contract")
    expected = [i / 10 for i in range(1001)]
    if len(times) != len(expected) or any(abs(a-b) > 1e-9 for a, b in zip(times, expected)):
        raise ValueError("incomplete or changed main-stage sample grid")
    line, buoy = model["Mooring"], model["Metocean buoy"]
    selectors = [
        ("tension_end_a", line, "Effective Tension", api.oeEndA),
        ("tension_end_b", line, "Effective Tension", api.oeEndB),
        ("buoy_x", buoy, "X", api.oeBuoy(0, 0, 0)),
        ("buoy_z", buoy, "Z", api.oeBuoy(0, 0, 0)),
        ("buoy_rotation_2", buoy, "Rotation 2", api.oeBuoy(0, 0, 0)),
        ("sea_surface_z", buoy, "Sea surface Z", api.oeBuoy(0, 0, 0)),
    ]
    results = {}
    for name, obj, variable, extra in selectors:
        values = numbers(obj.TimeHistory(variable, period, extra))
        if len(values) != len(times):
            raise ValueError(f"incomplete history: {name}")
        results[name] = values
    return times, results


def extract_results(model, api, contract, static):
    times, dynamic = histories(model, api, contract)
    return {
        "static": static, "times": times, "histories": dynamic,
        "units": {"times": "s", "positions": "m", "line_arclength": "m",
                  "tensions": "kN", "buoy_rotation_2": "degrees"},
        "frames": {"positions": "global Cartesian; buoy origin",
                   "buoy_rotation_2": "global-axis orientation, not dynamic local pitch",
                   "sea_surface_z": "global elevation at buoy origin",
                   "line_arclength": "from End A"},
    }
