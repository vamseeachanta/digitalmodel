"""Hull-keyed reporting from already extracted solver results."""
import pytest

from digitalmodel.hydrodynamics.diffraction.report_generator import generate_diffraction_report
from digitalmodel.hydrodynamics.hull_library.catalog import HullCatalog
from digitalmodel.hydrodynamics.hull_library.profile_schema import HullProfile, HullStation, HullType


@pytest.fixture
def catalog():
    result = HullCatalog()
    result.register_hull(HullProfile(
        name="test-box", hull_type=HullType.BARGE, source="synthetic test",
        length_bp=100, beam=20, draft=5, depth=10,
        stations=[HullStation(x_position=0, waterline_offsets=[(0, 10), (10, 10)]),
                  HullStation(x_position=100, waterline_offsets=[(0, 10), (10, 10)])],
    ))
    return result


def test_hull_metadata_is_rendered(tmp_path, catalog, mock_diffraction_results):
    path = generate_diffraction_report(
        output_path=tmp_path / "report.html", hull_id="test-box", catalog=catalog,
        solver_results={"test": mock_diffraction_results},
    )
    content = path.read_text(encoding="utf-8")
    for expected in ("test-box", "Hull catalogue", "Length between perpendiculars (m)",
                     "100.0", "synthetic test", "Revision history"):
        assert expected in content
    assert mock_diffraction_results.vessel_name != "test-box"


def test_unknown_hull_is_rejected(tmp_path, catalog, mock_diffraction_results):
    with pytest.raises(KeyError):
        generate_diffraction_report(output_path=tmp_path / "report.html", hull_id="missing",
                                    catalog=catalog, solver_results={"test": mock_diffraction_results})
    assert not (tmp_path / "report.html").exists()


def test_empty_results_are_rejected(tmp_path, catalog):
    with pytest.raises(ValueError, match="solver_results"):
        generate_diffraction_report(output_path=tmp_path / "report.html", hull_id="test-box",
                                    catalog=catalog, solver_results={})
