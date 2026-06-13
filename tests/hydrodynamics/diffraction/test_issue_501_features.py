"""QTF options, field points, irregular-frequency method (#501).

Test names follow the approved r2 plan's TDD list. Byte-identity for the
L00/L02/L03 corpus is enforced separately by
tests/hydrodynamics/diffraction/benchmarks/.
"""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml

from digitalmodel.hydrodynamics.diffraction.input_schemas import (
    DiffractionSpec,
    FieldPointSpec,
    IrregularFrequencyMethod,
    QTFOptions,
)
from digitalmodel.hydrodynamics.diffraction.orcawave_backend import (
    OrcaWaveBackend,
)

FIXTURES_DIR = Path(__file__).parent / "fixtures"


def _spec_dict(**solver_options) -> dict:
    base = yaml.safe_load((FIXTURES_DIR / "spec_ship_raos.yml").read_text())
    base.setdefault("solver_options", {})
    # The fixture sets legacy flat fields; clear them so each test controls
    # exactly which fields are "set".
    base["solver_options"] = solver_options
    return base


def _render(spec: DiffractionSpec, tmp_path: Path) -> dict:
    generated = OrcaWaveBackend().generate_single(spec, tmp_path)
    documents = [d for d in yaml.safe_load_all(generated.read_text()) if d]
    return documents[0]


def _render_text(spec: DiffractionSpec, tmp_path: Path) -> str:
    return OrcaWaveBackend().generate_single(spec, tmp_path).read_text()


# ---------------------------------------------------------------------------
# Sub-task 1 — irregular frequency method
# ---------------------------------------------------------------------------


class TestIrregularFrequencyMethod:
    def test_irregular_frequency_method_none(self, tmp_path):
        spec = DiffractionSpec.model_validate(
            _spec_dict(irregular_frequency_method="none")
        )
        text = _render_text(spec, tmp_path)
        assert "BodyAddInteriorSurfacePanels: No" in text
        assert "BodyInteriorSurfacePanelMethod" not in text

    def test_irregular_frequency_method_interior_panels(self, tmp_path):
        spec = DiffractionSpec.model_validate(
            _spec_dict(irregular_frequency_method="interior_panels")
        )
        text = _render_text(spec, tmp_path)
        assert "BodyAddInteriorSurfacePanels: Yes" in text
        assert "BodyInteriorSurfacePanelMethod: Triangulation method" in text

    def test_irregular_frequency_method_control_surface(self, tmp_path):
        data = _spec_dict(irregular_frequency_method="control_surface")
        data["vessel"]["control_surface"] = {
            "type": "mesh",
            "mesh_file": "cs.gdf",
        }
        spec = DiffractionSpec.model_validate(data)
        text = _render_text(spec, tmp_path)
        assert "BodyAddInteriorSurfacePanels: No" in text
        assert "BodyControlSurfaceMeshFileName: cs.gdf" in text

    def test_control_surface_method_without_mesh_fails(self):
        with pytest.raises(ValueError, match="control surface on every body"):
            DiffractionSpec.model_validate(
                _spec_dict(irregular_frequency_method="control_surface")
            )

    def test_remove_irregular_frequencies_legacy_true(self):
        with pytest.warns(DeprecationWarning, match="remove_irregular"):
            spec = DiffractionSpec.model_validate(
                _spec_dict(remove_irregular_frequencies=True)
            )
        assert (
            spec.solver_options.irregular_frequency_method
            is IrregularFrequencyMethod.INTERIOR_PANELS
        )

    def test_remove_irregular_frequencies_legacy_false(self):
        with pytest.warns(DeprecationWarning):
            spec = DiffractionSpec.model_validate(
                _spec_dict(remove_irregular_frequencies=False)
            )
        assert (
            spec.solver_options.irregular_frequency_method
            is IrregularFrequencyMethod.NONE
        )
        # normalized for legacy readers (AQWA backend etc.)
        assert spec.solver_options.remove_irregular_frequencies is False

    def test_remove_irregular_frequencies_legacy_unset(self, recwarn):
        spec = DiffractionSpec.model_validate(_spec_dict())
        assert (
            spec.solver_options.irregular_frequency_method
            is IrregularFrequencyMethod.INTERIOR_PANELS
        )
        assert spec.solver_options.remove_irregular_frequencies is True
        deprecations = [
            w for w in recwarn.list if issubclass(w.category, DeprecationWarning)
        ]
        assert deprecations == []

    def test_irregular_frequency_mutual_exclusion(self):
        with pytest.raises(ValueError, match="not both"):
            DiffractionSpec.model_validate(
                _spec_dict(
                    remove_irregular_frequencies=True,
                    irregular_frequency_method="interior_panels",
                )
            )

    def test_remove_irregular_frequencies_plus_control_surface_combo_rejected(self):
        with pytest.raises(ValueError, match="not both"):
            DiffractionSpec.model_validate(
                _spec_dict(
                    remove_irregular_frequencies=True,
                    irregular_frequency_method="control_surface",
                )
            )


# ---------------------------------------------------------------------------
# Sub-task 2 — QTF options
# ---------------------------------------------------------------------------


class TestQTFOptions:
    def test_qtf_crossing_angle_override(self, tmp_path):
        spec = DiffractionSpec.model_validate(
            _spec_dict(
                solve_type="diagonal_qtf",
                qtf={"enabled": True, "min_crossing_angle": 30,
                     "max_crossing_angle": 150},
            )
        )
        text = _render_text(spec, tmp_path)
        assert "QTFMinCrossingAngle: 30" in text
        assert "QTFMaxCrossingAngle: 150" in text
        assert "30.0" not in text.split("QTFMinCrossingAngle")[1][:6]

    def test_qtf_enabled_raises_when_solve_type_nonqtf(self):
        with pytest.raises(ValueError, match="requires solve_type"):
            DiffractionSpec.model_validate(
                _spec_dict(
                    solve_type="potential_and_source",
                    qtf={"enabled": True},
                )
            )

    def test_qtf_crossing_angle_not_emitted_when_qtf_disabled(self, tmp_path):
        spec = DiffractionSpec.model_validate(
            _spec_dict(solve_type="potential_and_source")
        )
        text = _render_text(spec, tmp_path)
        assert "QTFMinCrossingAngle" not in text
        assert "QTFMaxCrossingAngle" not in text

    def test_qtf_legacy_flat_with_nonqtf_solve_type_still_supported(self, tmp_path):
        """Plan deviation, documented: legacy flat qtf_calculation=true with a
        non-QTF solve type is load-bearing (the L03 ship benchmark uses it),
        so the strict C1 gate binds only to the nested qtf model."""
        with pytest.warns(DeprecationWarning):
            spec = DiffractionSpec.model_validate(
                _spec_dict(
                    solve_type="potential_and_source", qtf_calculation=True
                )
            )
        text = _render_text(spec, tmp_path)
        assert "QTFMinCrossingAngle: 0" in text

    @pytest.mark.parametrize(
        ("method", "expected_method", "expected_preferred"),
        [
            ("Direct", "Direct", "Direct method"),
            ("Indirect", "Indirect", "Indirect method"),
            ("Both", "Both", "Direct method"),
        ],
    )
    def test_qtf_load_calc_method(
        self, tmp_path, method, expected_method, expected_preferred
    ):
        spec = DiffractionSpec.model_validate(
            _spec_dict(
                solve_type="full_qtf",
                qtf={"enabled": True, "load_calculation_method": method},
            )
        )
        text = _render_text(spec, tmp_path)
        assert f"QTFCalculationMethod: {expected_method}" in text
        assert f"PreferredQTFCalculationMethod: {expected_preferred}" in text

    def test_qtf_legacy_flat_alias(self):
        with pytest.warns(DeprecationWarning, match="Flat QTF fields"):
            spec = DiffractionSpec.model_validate(
                _spec_dict(
                    solve_type="diagonal_qtf",
                    qtf_calculation=True,
                    qtf_min_frequency=0.2,
                    qtf_max_frequency=1.4,
                )
            )
        resolved = spec.solver_options.resolved_qtf()
        assert resolved.enabled is True
        assert resolved.min_frequency == 0.2
        assert resolved.max_frequency == 1.4

    def test_qtf_mutual_exclusion(self):
        with pytest.raises(ValueError, match="not both"):
            DiffractionSpec.model_validate(
                _spec_dict(
                    solve_type="diagonal_qtf",
                    qtf_calculation=True,
                    qtf={"enabled": True},
                )
            )

    def test_build_general_section_unchanged_under_flat_compat(self, tmp_path):
        with pytest.warns(DeprecationWarning):
            legacy = DiffractionSpec.model_validate(
                _spec_dict(qtf_calculation=True)
            )
        rendered = _render(legacy, tmp_path)
        assert rendered["QuadraticLoadPressureIntegration"] is True  # "Yes"
        assert rendered["PreferredQuadraticLoadCalculationMethod"] == (
            "Control surface"
        )


# ---------------------------------------------------------------------------
# Sub-task 3 — field points
# ---------------------------------------------------------------------------


class TestFieldPoints:
    def test_field_points_emit_combined_key(self, tmp_path):
        data = _spec_dict()
        data["outputs"] = {
            "field_points": [
                {"name": "deck", "points": [[1, 2, 3], [4, 5, 6]]}
            ]
        }
        spec = DiffractionSpec.model_validate(data)
        rendered = _render(spec, tmp_path)
        assert rendered["FieldPointX, FieldPointY, FieldPointZ"] == [
            [1.0, 4.0],
            [2.0, 5.0],
            [3.0, 6.0],
        ]

    def test_detect_field_points_inside_bodies_default_preserves_yes(
        self, tmp_path
    ):
        spec = DiffractionSpec.model_validate(_spec_dict())
        text = _render_text(spec, tmp_path)
        assert "DetectAndSkipFieldPointsInsideBodies: Yes" in text

    def test_detect_field_points_inside_bodies_false_renders_no(self, tmp_path):
        data = _spec_dict()
        data["outputs"] = {"detect_field_points_inside_bodies": False}
        spec = DiffractionSpec.model_validate(data)
        text = _render_text(spec, tmp_path)
        assert "DetectAndSkipFieldPointsInsideBodies: No" in text

    def test_field_points_empty_unchanged(self, tmp_path):
        spec = DiffractionSpec.model_validate(_spec_dict())
        text = _render_text(spec, tmp_path)
        assert "FieldPointX" not in text

    def test_field_points_modular_mode(self, tmp_path):
        data = _spec_dict()
        data["outputs"] = {
            "field_points": [{"name": "airgap", "points": [[10, 0, 5]]}]
        }
        spec = DiffractionSpec.model_validate(data)
        OrcaWaveBackend().generate_modular(spec, tmp_path)
        outputs_yml = (tmp_path / "08_outputs.yml").read_text()
        assert "FieldPointX, FieldPointY, FieldPointZ" in outputs_yml

    def test_field_point_spec_requires_points(self):
        with pytest.raises(ValueError):
            FieldPointSpec(name="empty", points=[])


class TestExports:
    def test_new_symbols_exported(self):
        from digitalmodel.hydrodynamics.diffraction import input_schemas

        for symbol in (
            "IrregularFrequencyMethod",
            "QTFOptions",
            "FieldPointSpec",
        ):
            assert symbol in input_schemas.__all__
