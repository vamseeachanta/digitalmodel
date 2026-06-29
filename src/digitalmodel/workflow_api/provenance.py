# ABOUTME: stamp_provenance -- the canonical reusable provenance assembler
# ABOUTME: codifying the #3282 provenance SHAPE (workspace-hub#3283).
"""Canonical reusable provenance assembler (workspace-hub#3283).

``stamp_provenance`` is the named, reusable entry-point each adopting repo calls
to stamp an envelope's ``provenance`` block. It DELEGATES to the #3282-owned
:func:`assetutilities.workflow_api.envelope.make_provenance` so the emitted dict
SHAPE can never drift from the one the runner already produces -- #3283 owns the
reusable named assembler, #3282 owns the field set + the parameterized
``code_version``. No second hashing function and no re-derivation of the
``input_hash`` (it is consumed verbatim from #3282).

``code_version`` is PARAMETERIZED by ``package_name`` (workspace-hub#3287): an
``assethold`` or ``digitalmodel`` envelope must report ITS OWN package version,
never a hardcoded ``assetutilities`` one.
"""

from __future__ import annotations

from assetutilities.workflow_api.envelope import make_provenance


def stamp_provenance(
    input_hash: str | None,
    *,
    package_name: str = "digitalmodel",
    standard_revisions: list | None = None,
    data_as_of: str | None = None,
) -> dict:
    """Assemble the canonical ``provenance`` block for a ResultEnvelope.

    Returns ``{code_version: {package_version, git_sha}, standard_revisions: [...],
    data_as_of, input_hash}``.

    Parameters
    ----------
    input_hash:
        The #3282-owned ``input_hash`` value. Reused verbatim -- never recomputed
        here. ``None`` is allowed (error envelopes carry no input hash).
    package_name:
        The package whose version + git sha are stamped under ``code_version``.
        Parameterized so each adopter stamps its OWN version.
    standard_revisions:
        Optional list of citation dicts (``code_id`` / ``publisher`` / ``revision``)
        lifted from a calc's DNV/API citation sidecar.
    data_as_of:
        Optional data-vintage stamp; ``None`` for pure-calc workflows whose
        determinism does not depend on a refreshable dataset.
    """
    return make_provenance(
        input_hash,
        package_name=package_name,
        standard_revisions=standard_revisions,
        data_as_of=data_as_of,
    )
