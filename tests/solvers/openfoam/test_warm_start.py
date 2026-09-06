from pathlib import Path

import yaml

from digitalmodel.solvers.openfoam.warm_start.decision import decide
from digitalmodel.solvers.openfoam.warm_start.fields import clean_restart, rewrite_speed_fields
from digitalmodel.solvers.openfoam.warm_start.record import RecordStore


FIELD = """FoamFile { format ascii; class volVectorField; object U; }
internalField uniform (-1 0 0);
boundaryField
{
 inlet { type fixedValue; value uniform (-1 0 0); }
 outlet { type outletPhaseMeanVelocity; Umean 1; value uniform (-1 0 0); }
}
"""


def test_beta_prior_refuses_first_geometry_hop_and_calibration_allows_it():
    cold = decide("geometry", 5000, 400, [], margin_fraction=.1)
    assert cold.decision == "COLD_BY_EV" and cold.ev == 200
    warm = decide("geometry", 5000, 400, [], calibrate=True)
    assert warm.decision == "WARM_CALIBRATION"


def test_copy_cleanup_and_ascii_boundary_rewrite(tmp_path: Path):
    source, target = tmp_path / "source", tmp_path / "target"
    for name in ("alpha.water", "U", "p_rgh", "k", "omega", "nut", "phi", "p"):
        (source / "20").mkdir(parents=True, exist_ok=True)
        (source / "20" / name).write_text(FIELD)
    (source / "20" / "uniform").mkdir()
    (target / "0.orig").mkdir(parents=True)
    for name in ("alpha.water", "U", "p_rgh", "k", "omega", "nut"):
        (target / "0.orig" / name).write_text(FIELD.replace("-1", "-2").replace("Umean 1", "Umean 2"))
    clean_restart(source / "20", target)
    rewrite_speed_fields(target, dry_run=True)
    assert "uniform (-2 0 0)" in (target / "0" / "U").read_text()
    assert "Umean 2" in (target / "0" / "U").read_text()
    assert not (target / "0" / "phi").exists()
    assert not (target / "0" / "uniform").exists()


def test_record_update(tmp_path: Path):
    store = RecordStore(tmp_path, "speed", "L1", 5000)
    store.append({"id": "x", "decision": "WARM", "outcome": "WARM_OK", "iterations": 2000})
    data = yaml.safe_load(store.path.read_text())
    assert data["summary"]["successes"] == 1
    assert data["summary"]["p_posterior"] == .6
