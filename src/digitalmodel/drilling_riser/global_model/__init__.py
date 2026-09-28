"""Drilling-riser global OrcaFlex model: text spec, generator and qualification hand checks.

* :mod:`.spec` - the riser global model specification (SI units, generic, no project data).
* :mod:`.build` - spec -> modular-generator ``generic`` spec -> OrcaFlex text YAML model.
* :mod:`.hand_checks` - submerged weight, effective tension chain, tensioned-beam periods.
* :mod:`.qualification` - gate evaluation (PASS / FAIL / NOT_EVALUATED).
* :mod:`.orcaflex_run` - statics, end tensions, modal periods and as-analysed section data
  through OrcFxAPI (configure the version pin with ``orcaflex_api.configure`` first).
"""

from .spec import LineSection, RiserGlobalModelSpec, tube_section

__all__ = ["LineSection", "RiserGlobalModelSpec", "tube_section"]
