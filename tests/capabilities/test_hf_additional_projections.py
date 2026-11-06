"""Hand projections of synthetic fixtures test export fidelity, not model physics."""

import json
from pathlib import Path

import pytest

from scripts.hf import build_capability_datasets as builder
from scripts.hf import capability_bundle as bundles


def test_casing_profiles_preserve_depth_alignment_and_every_pressure():
    """Two depth points give two rows; each pressure retains its original index.

    Arbitrary inputs are a hand-worked transposition, with no physical constants.
    Units and names follow build_casing_design_explorer.py::build_profiles.
    """
    source = {"profiles": {"depth_ft": [3, 7], "external_psi": [11, 13]}}
    assert builder.casing_profile_rows(source) == [
        {"depth_ft": 3, "external_psi": 11},
        {"depth_ft": 7, "external_psi": 13},
    ]
    source["profiles"]["external_psi"].pop()
    with pytest.raises(ValueError, match="length"):
        builder.casing_profile_rows(source)


def _product():
    """Arbitrary product fixture, not a catalog rating or acceptance threshold."""
    return {
        "label": "synthetic",
        "grade": "test",
        "od_in": 3,
        "weight_ppf": 7,
        "wall_in": 2,
        "id_in": 1,
        "d_over_t": 9,
        "ratings": {
            "burst_psi": 11,
            "collapse_psi": 13,
            "body_yield_lbf": 17,
            "collapse_regime": "test",
        },
        "passes_all": False,
        "max_frac_surface_pressure_psi": 19,
        "checks": {
            "tension": {
                "mode": "tension",
                "load_case": "test",
                "rating": 17,
                "max_load": 23,
                "min_design_factor": 2,
                "governing_depth_ft": 7,
                "required_design_factor": 3,
                "passes": False,
            },
        },
    }


def test_casing_products_and_checks_keep_ratings_load_units_and_false():
    """Hand flatten: one product with one tension check gives one row per config.

    Tension uses lbf, pressure/stress checks use psi, per casing_design.py's
    check_tension, check_burst, check_collapse and check_triaxial return fields.
    No design factor is computed or reclassified by the projection.
    """
    product = _product()
    assert builder.casing_product_rows({"products": [product]}) == [
        {
            "label": "synthetic",
            "grade": "test",
            "od_in": 3,
            "weight_ppf": 7,
            "wall_in": 2,
            "id_in": 1,
            "d_over_t": 9,
            "burst_psi": 11,
            "collapse_psi": 13,
            "body_yield_lbf": 17,
            "collapse_regime": "test",
            "passes_all": False,
            "max_frac_surface_pressure_psi": 19,
        }
    ]
    assert builder.casing_check_rows({"products": [product]}) == [
        {
            "product": "synthetic",
            "mode": "tension",
            "load_case": "test",
            "rating": 17,
            "max_load": 23,
            "load_unit": "lbf",
            "min_design_factor": 2,
            "governing_depth_ft": 7,
            "required_design_factor": 3,
            "passes": False,
        }
    ]
    product["checks"] = {"unknown": product["checks"]["tension"]}
    with pytest.raises(ValueError, match="unit"):
        builder.casing_check_rows({"products": [product]})


def test_sloshing_keeps_ratio_definitions_and_size_basis_separate():
    """Hand unpivot: two curve points and one lookup yield two and one rows.

    Arbitrary inputs are copied literally. Definitions follow the generator's
    SHAPES, _norm_curve and _lookup_table; no dispersion equation is evaluated.
    CFD metadata must not become a table of analytical observations.
    """
    source = {
        "shapes": [
            {
                "key": "elliptical",
                "ratio": "source label",
                "lc": "W (width)",
                "default": {"span": 7, "span_label": "Width W (m)"},
                "curve": [[2, 3], [5, 11]],
                "lookup": [{"f": 2, "T": 13}],
            }
        ],
        "cfd": {"points": ["excluded"]},
    }
    assert builder.sloshing_curve_rows(source) == [
        {
            "shape": "elliptical",
            "characteristic_length_definition": "W (width)",
            "depth_over_characteristic_length": 2,
            "dimensionless_frequency": 3,
        },
        {
            "shape": "elliptical",
            "characteristic_length_definition": "W (width)",
            "depth_over_characteristic_length": 5,
            "dimensionless_frequency": 11,
        },
    ]
    assert builder.sloshing_lookup_rows(source) == [
        {
            "shape": "elliptical",
            "span_m": 7,
            "span_definition": "Width W (m)",
            "depth_ratio": 2,
            "depth_ratio_definition": "depth / height",
            "period_s": 13,
        }
    ]
    files = bundles.bundle_files("digitalmodel-hydro", license_id="mit")
    manifest = json.loads(files["manifest.json"])
    columns = manifest["tables"]["sloshing_curves"]["columns"]
    assert "characteristic_length_definition" in columns
    assert "characteristic_length" not in columns
    assert "characteristic_length_definition" in files["README.md"].decode("utf-8")


def test_dynacard_preserves_diagnosis_metrics_and_ordered_differential():
    """One case and two candidates give one summary and two differential rows.

    Fixture numbers are arbitrary. Fields/units follow
    build_dynacard_troubleshooting.py::build_cases and main's sidecar writer.
    Probabilities are retained, never normalised; missing curves stay absent.
    """
    source = {
        "cases": [
            {
                "mode": "synthetic",
                "seed": 7,
                "classification": "candidate-a",
                "confidence": 0.7,
                "severity": "test",
                "model_version": "fixture",
                "n_points": 3,
                "metrics": {
                    "fluid_load_lbs": 11,
                    "peak_load_lbs": 13,
                    "min_load_lbs": 2,
                    "card_area_inlb": 17,
                    "area_vs_healthy_pct": 19,
                },
                "differential": [
                    {"mode": "candidate-a", "probability": 0.7},
                    {"mode": "candidate-b", "probability": 0.3},
                ],
            }
        ]
    }
    assert builder.dynacard_case_rows(source) == [
        {
            "mode": "synthetic",
            "seed": 7,
            "classification": "candidate-a",
            "confidence": 0.7,
            "severity": "test",
            "model_version": "fixture",
            "n_points": 3,
            "fluid_load_lbf": 11,
            "peak_load_lbf": 13,
            "min_load_lbf": 2,
            "card_area_in_lbf": 17,
            "area_vs_healthy_pct": 19,
        }
    ]
    assert builder.dynacard_differential_rows(source) == [
        {
            "case_mode": "synthetic",
            "candidate_rank": 1,
            "candidate_mode": "candidate-a",
            "probability": 0.7,
        },
        {
            "case_mode": "synthetic",
            "candidate_rank": 2,
            "candidate_mode": "candidate-b",
            "probability": 0.3,
        },
    ]


def test_sloshing_retained_sources_exclude_solver_sections():
    """Source identity covers the whole sidecar; retained bytes cover analytic data.

    Independent SHA-256 calls bind both representations without claiming the
    filtered source digest identifies the original sidecar bytes.
    """
    import hashlib

    files = bundles.bundle_files("digitalmodel-hydro", license_id="mit")
    manifest = json.loads(files["manifest.json"])
    for name in ("sloshing_curves", "sloshing_lookup"):
        retained = files[f"sources/{name}.json"]
        assert set(json.loads(retained)) == {"meta", "shapes"}
        metadata = manifest["tables"][name]
        source = (
            (bundles.REPO / metadata["source"]).read_bytes().replace(b"\r\n", b"\n")
        )
        assert metadata["source_sha256"] == hashlib.sha256(source).hexdigest()
        assert (
            metadata["retained_source_sha256"] == hashlib.sha256(retained).hexdigest()
        )
        assert metadata["retained_source_sections"] == ["meta", "shapes"]


@pytest.mark.parametrize("name", ["dynacard_cases", "dynacard_differential"])
def test_dynacard_retained_sources_contain_only_projected_inputs(name):
    """Retained inputs reproduce rows without settings, library data or advice."""
    import hashlib

    files = bundles.bundle_files("digitalmodel-artificial-lift", license_id="mit")
    manifest = json.loads(files["manifest.json"])
    retained_bytes = files[f"sources/{name}.json"]
    retained = json.loads(retained_bytes)
    assert set(retained) == {"cases"}
    fields = (
        {
            "mode",
            "seed",
            "classification",
            "confidence",
            "severity",
            "model_version",
            "n_points",
            "metrics",
        }
        if name == "dynacard_cases"
        else {"mode", "differential"}
    )
    for case in retained["cases"]:
        assert set(case) == fields
        if name == "dynacard_cases":
            assert set(case["metrics"]) == {
                "fluid_load_lbs",
                "peak_load_lbs",
                "min_load_lbs",
                "card_area_inlb",
                "area_vs_healthy_pct",
            }
        else:
            assert all(
                set(item) == {"mode", "probability"} for item in case["differential"]
            )
    table = manifest["tables"][name]
    source_bytes = (bundles.REPO / table["source"]).read_bytes()
    source_bytes = source_bytes.replace(b"\r\n", b"\n")
    original = json.loads(source_bytes)
    project = builder.DATASETS["digitalmodel-artificial-lift"][name]
    rows = [json.loads(line) for line in files[f"{name}/train.jsonl"].splitlines()]
    assert rows == project(original) == project(retained)
    assert table["source_sha256"] == hashlib.sha256(source_bytes).hexdigest()
    assert table["retained_source_sha256"] == hashlib.sha256(retained_bytes).hexdigest()
    assert table["retained_source_sections"] == ["cases"]


def test_unselected_sloshing_metadata_is_never_retained_or_projected(monkeypatch):
    """An excluded section may contain private locators; it never enters a bundle."""
    read = bundles._text_bytes

    def with_unselected_metadata(path):
        data = read(path)
        if path.name == "sloshing-explorer.json":
            doc = json.loads(data)
            doc["cfd"] = {"private_locator": str(path)}
            return json.dumps(doc).encode()
        return data

    expected = bundles.bundle_files("digitalmodel-hydro", license_id="mit")
    monkeypatch.setattr(bundles, "_text_bytes", with_unselected_metadata)
    actual = bundles.bundle_files("digitalmodel-hydro", license_id="mit")
    for name in ("sloshing_curves", "sloshing_lookup"):
        assert actual[f"sources/{name}.json"] == expected[f"sources/{name}.json"]
        assert actual[f"{name}/train.jsonl"] == expected[f"{name}/train.jsonl"]
    assert actual["manifest.json"] != expected["manifest.json"]


def test_dynacard_excluded_nested_content_never_enters_the_bundle(
    tmp_path, monkeypatch
):
    """Extra keys at every selected nesting level are excluded before locator checks."""
    read = bundles._text_bytes
    restricted = (Path("/") / "home" / tmp_path.name / "data").as_posix()

    def with_excluded_content(path):
        data = read(path)
        if path.name == "dynacard-troubleshooting.json":
            doc = json.loads(data)
            doc["well"] = doc["library_summary"] = {"locator": restricted}
            for case in doc["cases"]:
                case["actions"] = case["card_coordinates"] = [restricted]
                case["metrics"]["extra"] = restricted
                for candidate in case["differential"]:
                    candidate["actions"] = [restricted]
            return json.dumps(doc).encode("utf-8")
        return data

    expected = bundles.bundle_files("digitalmodel-artificial-lift", license_id="mit")
    monkeypatch.setattr(bundles, "_text_bytes", with_excluded_content)
    actual = bundles.bundle_files("digitalmodel-artificial-lift", license_id="mit")
    for name in ("dynacard_cases", "dynacard_differential"):
        for path in (f"sources/{name}.json", f"{name}/train.jsonl"):
            assert actual[path] == expected[path]
    assert actual["manifest.json"] != expected["manifest.json"]


@pytest.mark.parametrize("missing", ["confidence", "metric", "probability"])
def test_missing_dynacard_inputs_fail_before_creating_output(
    tmp_path, monkeypatch, missing
):
    """Required selected values cannot be silently dropped or filled with defaults."""
    read = bundles._text_bytes

    def with_missing_input(path):
        data = read(path)
        if path.name == "dynacard-troubleshooting.json":
            doc = json.loads(data)
            case = doc["cases"][0]
            if missing == "metric":
                del case["metrics"]["peak_load_lbs"]
            elif missing == "probability":
                del case["differential"][0]["probability"]
            else:
                del case[missing]
            return json.dumps(doc).encode("utf-8")
        return data

    monkeypatch.setattr(bundles, "_text_bytes", with_missing_input)
    target = tmp_path / "bundle"
    with pytest.raises(KeyError):
        bundles.write_bundle("digitalmodel-artificial-lift", target, license_id="mit")
    assert not target.exists()


def test_exporter_uses_explicit_utf8_and_lf(tmp_path, monkeypatch):
    """Output bytes retain UTF-8 and LF when platform text defaults differ."""
    write = Path.write_text

    def alternate_defaults(path, text, *, encoding=None, newline=None):
        return write(
            path, text, encoding=encoding or "ascii", newline=newline or "\r\n"
        )

    monkeypatch.setattr(Path, "write_text", alternate_defaults)
    monkeypatch.setattr(
        builder, "DATASETS", {"synthetic": {"table": lambda: [{"label": "µ"}]}}
    )
    monkeypatch.setattr("sys.argv", ["exporter", "--out", str(tmp_path)])
    builder.main()
    data = (tmp_path / "synthetic/table.json").read_bytes()
    assert b"\r" not in data
    assert json.loads(data.decode("utf-8")) == [{"label": "µ"}]


@pytest.mark.parametrize(
    "config,expected",
    [
        ("casing_profiles", 61),
        ("casing_products", 8),
        ("casing_checks", 8 * 4),
        ("sloshing_curves", 30 + 30 + 15 + 15),
        ("sloshing_lookup", 4 * 7),
        ("dynacard_cases", 8),
        ("dynacard_differential", 8 * 3),
    ],
)
def test_new_committed_config_counts(config, expected):
    """Independent counts from committed generator grids, not exporter output.

    casing: build_casing_design_explorer.py, 61 depths, 8 products, 4 modes.
    sloshing: build_sloshing_explorer.py, _norm_curve produces 30/30/15/15
    points with its early exit; SHAPES has four entries, seven lookup fills each.
    dynacard: build_dynacard_troubleshooting.py, eight USE_CASES and top three
    differential diagnoses per case. These are coverage counts only.
    """
    functions = {
        name: fn for tables in builder.DATASETS.values() for name, fn in tables.items()
    }
    assert len(functions[config]()) == expected


@pytest.mark.parametrize(
    "dataset",
    [
        "digitalmodel-well",
        "digitalmodel-hydro",
        "digitalmodel-artificial-lift",
    ],
)
def test_extended_bundles_declare_and_verify_every_new_table(tmp_path, dataset):
    """Two builds agree byte-for-byte; each JSONL column set matches its manifest."""
    target = tmp_path / "bundle"
    bundles.write_bundle(dataset, target, license_id="mit")
    bundles.verify_bundle(dataset, target, license_id="mit")
    manifest = json.loads((target / "manifest.json").read_text(encoding="utf-8"))
    assert set(manifest["tables"]) == set(builder.DATASETS[dataset])
    for name, table in manifest["tables"].items():
        assert table["data_limits"]
        rows = [
            json.loads(line)
            for line in (target / name / "train.jsonl")
            .read_text(encoding="utf-8")
            .splitlines()
        ]
        assert all(set(row) == set(table["columns"]) for row in rows)
        assert table["row_count"] == len(rows)
        assert table["engineering_readiness"] == "NOT_EVALUATED"


def test_shared_sidecar_is_read_once_for_all_configs(monkeypatch):
    """Three casing tables must describe one snapshot even if the source changes.

    A second read is refused independently of the projection code, then source
    digests are compared across the product, check and pressure-profile configs.
    """
    read = bundles._text_bytes
    reads = []

    def read_once(path):
        if path.name == "casing-design-explorer.json":
            reads.append(path)
            assert len(reads) == 1, "shared source was reopened"
        return read(path)

    monkeypatch.setattr(bundles, "_text_bytes", read_once)
    files = bundles.bundle_files("digitalmodel-well", license_id="mit")
    manifest = json.loads(files["manifest.json"])
    names = ("casing_profiles", "casing_products", "casing_checks")
    assert len(reads) == 1
    assert len({manifest["tables"][name]["source_sha256"] for name in names}) == 1
    assert len({files[f"sources/{name}.json"] for name in names}) == 1
