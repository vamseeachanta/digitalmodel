"""Offline expansion of high-level OrcaFlex post-process flags into concrete config.

The ``orcaflex-strength-post`` template (and similar generic templates) supplies
only the high-level form::

    orcaflex:
      postprocess:
        summary:
          flag: True
        RangeGraph:
          flag: True
    parameters:
      VarNames:
        Line: [Effective tension, Wall tension, Bend moment, Curvature, Bend radius]

but the post-processors consume an expanded config that *nothing* derives from the
above:

  * ``cfg["summary_settings"]["groups"]``  (read in ``opp_summary.get_summary_for_file``)
  * ``cfg["RangeGraph"]``  as a list of per-variable specs (read in
    ``opp_range_graph.postProcessRange``)

This module bridges that gap.  :func:`expand_high_level_postprocess` is a *pure*
function: given the high-level cfg plus the list of Line object names (which in a
real run come from ``OrcFxAPI`` via the loaded model, but are just strings here)
it emits the concrete per-Line ``summary_settings`` / ``RangeGraph`` blocks.

It is intentionally backward compatible: if the explicit blocks are already
present, or the high-level flags are off, the cfg is returned unchanged so the
detailed strength inputs keep working exactly as before.
"""

# Standard library imports
import copy
import logging

# AdditionalData names consumed by opp_range_graph.postProcessRange when building
# the per-variable RangeGraph DataFrame. "Max" is the canonical strength stat.
DEFAULT_RANGE_GRAPH_ADDITIONAL_DATA = ["Max"]
DEFAULT_SIMULATION_PERIOD = "WholeSimulation"
DEFAULT_STATISTIC_TYPE = "Max"


def _high_level_flag(cfg, name):
    return (
        cfg.get("orcaflex", {})
        .get("postprocess", {})
        .get(name, {})
        .get("flag", False)
    )


def _var_names_for(cfg, object_type="Line"):
    return (
        cfg.get("parameters", {})
        .get("VarNames", {})
        .get(object_type, [])
    )


def build_summary_settings(var_names, object_names):
    """Build a ``summary_settings`` dict from variable + object names.

    One group per variable (e.g. "Effective tension"), each group holding one
    per-object ``Column`` running a Range Graph max over the whole simulation.
    Mirrors the shape consumed by ``opp_summary.get_summary_for_file``.
    """
    groups = []
    for var_name in var_names:
        columns = []
        for object_name in object_names:
            columns.append(
                {
                    "Command": "Range Graph",
                    "ObjectName": object_name,
                    "SimulationPeriod": DEFAULT_SIMULATION_PERIOD,
                    "ArcLength": [],
                    "Variable": var_name,
                    "Statistic_Type": DEFAULT_STATISTIC_TYPE,
                    "Label": object_name,
                }
            )
        groups.append({"Label": var_name, "Columns": columns})
    return {"groups": groups}


def build_range_graph(var_names, object_names):
    """Build a ``RangeGraph`` list (per object x variable).

    Mirrors the shape consumed by ``opp_range_graph.postProcessRange``:
    each entry carries ObjectName / SimulationPeriod / Variable / ArcLength /
    AdditionalData.
    """
    range_graph = []
    for object_name in object_names:
        for var_name in var_names:
            range_graph.append(
                {
                    "ObjectName": object_name,
                    "SimulationPeriod": DEFAULT_SIMULATION_PERIOD,
                    "Variable": var_name,
                    "ArcLength": [],
                    "AdditionalData": list(DEFAULT_RANGE_GRAPH_ADDITIONAL_DATA),
                }
            )
    return range_graph


def expand_high_level_postprocess(cfg, object_names, object_type="Line"):
    """Expand high-level summary/RangeGraph flags into concrete per-object config.

    Pure / offline: ``object_names`` is a plain list of object (Line) names; in a
    real run they are read from the loaded OrcFxAPI model, but the expansion
    itself needs no license.

    Backward compatible:
      * the high-level flag must be on, AND
      * the concrete block must be absent (or empty),
    otherwise that block is left untouched.

    Returns the (mutated) cfg for convenience.
    """
    var_names = _var_names_for(cfg, object_type)

    # ---- summary -------------------------------------------------------
    if _high_level_flag(cfg, "summary"):
        existing = cfg.get("summary_settings")
        already_expanded = bool(existing) and bool(existing.get("groups"))
        if not already_expanded:
            if var_names and object_names:
                cfg["summary_settings"] = build_summary_settings(
                    var_names, object_names
                )
            else:
                logging.warning(
                    "summary.flag is True but cannot expand summary_settings: "
                    "var_names=%s object_names=%s",
                    var_names,
                    object_names,
                )

    # ---- RangeGraph ----------------------------------------------------
    if _high_level_flag(cfg, "RangeGraph"):
        existing = cfg.get("RangeGraph")
        already_expanded = isinstance(existing, list) and len(existing) > 0
        if not already_expanded:
            if var_names and object_names:
                cfg["RangeGraph"] = build_range_graph(var_names, object_names)
            else:
                logging.warning(
                    "RangeGraph.flag is True but cannot expand RangeGraph: "
                    "var_names=%s object_names=%s",
                    var_names,
                    object_names,
                )

    return cfg


def get_object_names_from_model(model, of_objects, object_type="Line"):
    """Return the names of all objects of ``object_type`` in a loaded model.

    Thin, license-dependent wrapper: enumerates the model objects via the
    existing :class:`OrcaFlexObjects` helper and filters by type name.  Returns
    an empty list if the model is ``None`` or enumeration fails, so callers stay
    robust offline.
    """
    if model is None:
        return []
    try:
        object_df = of_objects.get_model_objects(model)["object_df"]
    except Exception as e:  # pragma: no cover - defensive
        logging.warning("Could not enumerate model objects: %s", e)
        return []
    if object_df is None or object_df.empty:
        return []
    matched = object_df[object_df["ObjectTypeName"] == object_type]
    return list(matched["ObjectName"])
