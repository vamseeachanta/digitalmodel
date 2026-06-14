from copy import deepcopy
from pathlib import Path

import pandas as pd
import pytest
import yaml


def _write_base_input(path: Path) -> None:
    path.write_text(
        yaml.safe_dump(
            {
                "basename": "template",
                "pipeline": {
                    "span_length": [10],
                    "crossection": [{"Design_WT": 0.375}],
                },
            },
            sort_keys=False,
        )
    )


def _run_router(tmp_path, monkeypatch, variants):
    from digitalmodel import engine as engine_module
    from digitalmodel.workflows.parametric_run import router

    base_input = tmp_path / "base.yml"
    output_dir = tmp_path / "results"
    manifest = output_dir / "cases.csv"
    _write_base_input(base_input)

    calls = []

    def fake_engine(*, cfg, **kwargs):
        calls.append(deepcopy(cfg))
        return cfg

    monkeypatch.setattr(engine_module, "engine", fake_engine)

    cfg = {
        "basename": "parametric_run",
        "_config_dir_path": str(tmp_path),
        "parametric_run": {
            "base_input": "base.yml",
            "target_basename": "dummy_target",
            "variants": variants,
            "render": {
                "output_dir": str(output_dir),
                "output_pattern": "case_{index}",
            },
            "run": {"engine": "python", "mode": "serial"},
            "collect": {"manifest": str(manifest)},
        },
    }

    result = router(cfg)
    return result, calls, manifest, output_dir


def test_parametric_run_expands_factorial_cases_with_dotted_mapping(
    tmp_path, monkeypatch
):
    variants = {
        "source": "factorial",
        "parameters": [
            {"name": "span", "values": [40, 60]},
            {"name": "wall", "values": [0.5, 0.625]},
        ],
        "mapping": {
            "span": "pipeline.span_length.0",
            "wall": "pipeline.crossection.0.Design_WT",
        },
    }

    result, calls, manifest, output_dir = _run_router(tmp_path, monkeypatch, variants)

    assert len(calls) == 4
    assert calls[0]["basename"] == "dummy_target"
    assert calls[0]["pipeline"]["span_length"] == [40]
    assert calls[0]["pipeline"]["crossection"][0]["Design_WT"] == 0.5

    generated = output_dir / "case_0.yml"
    assert generated.exists()
    generated_cfg = yaml.safe_load(generated.read_text())
    assert generated_cfg["pipeline"]["span_length"] == [40]
    assert generated_cfg["pipeline"]["crossection"][0]["Design_WT"] == 0.5

    rows = pd.read_csv(manifest)
    assert rows["status"].tolist() == ["completed"] * 4
    assert rows["input_file"].tolist()[0] == str(generated)
    assert result["parametric_run"]["outputs"]["manifest"] == str(manifest)


@pytest.mark.parametrize(
    ("source", "variants", "expected"),
    [
        (
            "range",
            {
                "source": "range",
                "param": "span",
                "from": 1,
                "to": 3,
                "mapping": {"span": "pipeline.span_length.0"},
            },
            [1, 2, 3],
        ),
        (
            "csv",
            {
                "source": "csv",
                "file": "matrix.csv",
                "mapping": {"span": "pipeline.span_length.0"},
            },
            [20, 30],
        ),
        (
            "yaml_matrix",
            {
                "source": "yaml_matrix",
                "list": [{"span": 70}, {"span": 80}],
                "mapping": {"span": "pipeline.span_length.0"},
            },
            [70, 80],
        ),
    ],
)
def test_parametric_run_expands_non_factorial_sources(
    tmp_path, monkeypatch, source, variants, expected
):
    if source == "csv":
        (tmp_path / "matrix.csv").write_text("span\n20\n30\n")

    _, calls, manifest, _ = _run_router(tmp_path, monkeypatch, variants)

    assert [call["pipeline"]["span_length"][0] for call in calls] == expected
    assert pd.read_csv(manifest)["span"].tolist() == expected
