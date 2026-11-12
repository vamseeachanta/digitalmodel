"""Keep optional slope installation independent of the seepage mesher."""

from pathlib import Path
import tomllib

from packaging.requirements import Requirement


def test_slope_extras_preserve_optional_solver_boundaries():
    """Use the benchmarked release without requiring Gmsh for stability.

    Source: xslope v1.0.3 pyproject.toml declares Gmsh in its fem extra;
    the adapter's benchmark pin is documented in the slope screening guide.
    """
    metadata = tomllib.loads(
        (Path(__file__).parents[2] / "pyproject.toml").read_text()
    )["project"]
    extras = metadata["optional-dependencies"]
    slope = {item.name: item for item in map(Requirement, extras["slope"])}
    seepage = {item.name: item for item in map(Requirement, extras["slope-seepage"])}
    assert str(slope["xslope"].specifier) == "==1.0.3"
    assert slope["xslope"] == seepage["xslope"]
    assert "gmsh" not in slope
    assert "4.15.2" in seepage["gmsh"].specifier
    assert "4.14.0" not in seepage["gmsh"].specifier
    assert not slope["xslope"].extras
    assert "xslope" not in {Requirement(item).name for item in metadata["dependencies"]}
