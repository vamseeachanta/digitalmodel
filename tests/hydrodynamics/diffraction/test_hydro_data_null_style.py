"""The emitted null style must be pinned, not inherited from PyYAML.

The #1633 closeout on the Windows host (acma-hou-rds02) failed
test_unit_box_docs_hydro_data_matches_fresh_run with 177 line differences,
every one of the form:

    'refusal_reason: ~'  !=  'refusal_reason: null'

`~` and `null` are the SAME value in YAML -- both are None. No hydrodynamic
number moved. The difference is purely how that host's PyYAML chose to
serialise None, and the golden test compares LINES, so the committed evidence
could never reproduce there.

Owner decision 2026-08-04: pin the emit style rather than relax the comparison
to parsed-YAML equality. Byte-identity also catches formatting drift, and
that guarantee is worth keeping; portability is the cheaper thing to fix.
"""

from __future__ import annotations

import yaml

from digitalmodel.hydrodynamics.diffraction.benchmark_runner import (
    PinnedNullDumper,
)


def _represent_none_as_tilde(dumper, _data):
    return dumper.represent_scalar("tag:yaml.org,2002:null", "~")


class TestNullStyleIsPinned:
    def test_none_emits_null_not_tilde(self) -> None:
        text = yaml.dump({"refusal_reason": None}, Dumper=PinnedNullDumper)

        assert text == "refusal_reason: null\n"

    def test_pin_overrides_a_host_that_prefers_tilde(self) -> None:
        """The discriminating case: prove the pin WINS, not that it agrees.

        A test that only dumps on this host proves nothing -- this host
        already emits `null`. Registering the tilde representer on the base
        Dumper simulates the Windows host, so a dumper that merely inherited
        the ambient style would fail here.
        """
        yaml.add_representer(type(None), _represent_none_as_tilde, Dumper=yaml.Dumper)
        try:
            inherited = yaml.dump({"refusal_reason": None}, Dumper=yaml.Dumper)
            pinned = yaml.dump({"refusal_reason": None}, Dumper=PinnedNullDumper)
        finally:
            yaml.add_representer(
                type(None), yaml.representer.SafeRepresenter.represent_none,
                Dumper=yaml.Dumper,
            )

        assert inherited == "refusal_reason: ~\n"
        assert pinned == "refusal_reason: null\n"

    def test_nested_and_collection_nulls_are_pinned(self) -> None:
        """The committed evidence carries nulls inside nested matrix maps."""
        doc = {"pair": {"added_mass_correlations": {"1,2": None, "1,4": None}}}

        text = yaml.dump(doc, Dumper=PinnedNullDumper, default_flow_style=False)

        assert "1,2: null" in text
        assert "1,4: null" in text
        assert "~" not in text
