from __future__ import annotations

import importlib.util
import sys
from pathlib import Path

import yaml


def _load_validate_module():
    module_path = (
        Path(__file__).resolve().parents[3]
        / "scripts"
        / "benchmark"
        / "validate_owd_vs_spec.py"
    )
    spec = importlib.util.spec_from_file_location(
        "validate_owd_vs_spec_test_module",
        module_path,
    )
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    sys.modules.setdefault(spec.name, module)
    spec.loader.exec_module(module)
    return module


def _write_yaml(path: Path, data: dict) -> None:
    path.write_text(yaml.safe_dump(data, sort_keys=False), encoding="utf-8")


def test_compare_orcawave_ymls_emits_taxonomy_categories(tmp_path: Path):
    module = _load_validate_module()
    owd = tmp_path / "owd.yml"
    spec = tmp_path / "spec.yml"

    _write_yaml(
        owd,
        {
            "SolveType": "Potential",
            "WavesReferredToBy": "frequency (rad/s)",
            "OutputPanelPressures": "Yes",
            "ComputationStrategy": "Standard",
            "Bodies": [{"BodyName": "Hull_v1", "DivideNonPlanarPanels": "Yes"}],
        },
    )
    _write_yaml(
        spec,
        {
            "SolveType": "Full QTF",
            "WavesReferredToBy": "period (s)",
            "OutputPanelPressures": "No",
            "ComputationStrategy": "Robust",
            "Bodies": [{"BodyName": "Hull_auto", "DivideNonPlanarPanels": "No"}],
        },
    )

    result = module._compare_orcawave_ymls(owd, spec)
    categories = {diff["key"]: diff["category"] for diff in result["diffs"]}

    assert categories["SolveType"] == "solver_mode_significant"
    assert categories["WavesReferredToBy"] == "representation_normalization_only"
    assert categories["OutputPanelPressures"] == "output_only"
    assert categories["ComputationStrategy"] == "internal_default_only"
    assert categories["Bodies[0].BodyName"] == "gui_only"
    assert categories["Bodies[0].DivideNonPlanarPanels"] == "known_non_configurable_in_spec"


def test_compare_orcawave_ymls_returns_taxonomy_counts(tmp_path: Path):
    module = _load_validate_module()
    owd = tmp_path / "owd.yml"
    spec = tmp_path / "spec.yml"

    _write_yaml(
        owd,
        {
            "SolveType": "Potential",
            "WavesReferredToBy": "frequency (rad/s)",
            "OutputPanelPressures": "Yes",
            "Bodies": [{"BodyName": "Hull_v1"}],
        },
    )
    _write_yaml(
        spec,
        {
            "SolveType": "Full QTF",
            "WavesReferredToBy": "period (s)",
            "OutputPanelPressures": "No",
            "Bodies": [{"BodyName": "Hull_auto"}],
        },
    )

    result = module._compare_orcawave_ymls(owd, spec)

    assert result["taxonomy_counts"]["solver_mode_significant"] == 1
    assert result["taxonomy_counts"]["representation_normalization_only"] == 1
    assert result["taxonomy_counts"]["output_only"] == 1
    assert result["taxonomy_counts"]["gui_only"] == 1


def test_coupling_wrapper_and_index_keep_refusal(tmp_path):
    """Missing axes cannot yield correlation or a favourable coupling badge."""
    import numpy as np
    module = _load_validate_module()
    values = np.ones((5, 6, 6))
    correlations = module._compute_matrix_correlation(values, values)
    assert all(value is None for row in correlations for value in row)
    quality = [["INVALID_ABSCISSA"] * 6 for _ in range(6)]
    evidence = {"quality": quality, "sample_count": None, "comparison_status": "REFUSED"}
    coupling = {"(0, 1)": {
        "added_mass_correlation": correlations, "damping_correlation": correlations,
        "added_mass_evidence": evidence, "damping_evidence": evidence,
    }}
    page = module._build_multibody_index_html(
        tmp_path, "synthetic", {"description": "synthetic diagnostic"}, [], {}, coupling,
    )
    text = page.read_text()
    assert "REFUSED" in text
    assert text.count('class="cell-refused"') == 72
    assert "min r=" not in text
