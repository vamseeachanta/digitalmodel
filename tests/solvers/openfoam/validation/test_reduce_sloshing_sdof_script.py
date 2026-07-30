"""CLI contract tests for the single-tank SDOF reducer (#1911)."""

from __future__ import annotations

import importlib.util
import json
from pathlib import Path


SCRIPT = (
    Path(__file__).resolve().parents[4]
    / "scripts"
    / "cfd"
    / "reduce_sloshing_sdof.py"
)


def test_reducer_can_write_to_nondefault_output(tmp_path, monkeypatch) -> None:
    spec = importlib.util.spec_from_file_location("reduce_sloshing_sdof", SCRIPT)
    assert spec and spec.loader
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)

    forced_path = tmp_path / "forced.json"
    forced_path.write_text(
        json.dumps(
            {
                "tank": {"breadth_m": 0.9},
                "fills": [
                    {
                        "h_over_L": 0.5,
                        "T1_analytical_s": 1.0,
                        "forced": [
                            {
                                "status": "completed",
                                "period_ratio": 1.0,
                                "drive_period_s": 1.0,
                                "runup_amp_m": 0.2,
                                "quad_coeff": 1.0,
                            }
                        ],
                    }
                ],
            }
        )
    )
    monkeypatch.setattr(module, "_FORCED", forced_path)
    output_path = tmp_path / "reduced.json"
    default_output_before = module._OUT.read_bytes()

    assert (
        module.main(
            [
                "--work-dir",
                str(tmp_path / "no-raw-cases"),
                "--output",
                str(output_path),
            ]
        )
        == 0
    )
    assert output_path.exists()
    assert module._OUT.read_bytes() == default_output_before
