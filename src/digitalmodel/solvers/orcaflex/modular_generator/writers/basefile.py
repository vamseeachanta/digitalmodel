"""Emit an OrcaFlex variation model composed with ``BaseFile:``.

OrcaFlex composes a variation model two ways, and the semantics differ:

``BaseFile:``
    Clears **all** existing model data, then loads the named file.  Accepts a
    binary ``.dat`` or a text ``.yml``.

``IncludeFile:``
    Merges incrementally from whatever state the model is already in.  Text
    only.

:mod:`digitalmodel.solvers.orcaflex.modular_generator` emits the second form, as
a flat ``- includefile:`` list.  This module emits the first.

Two constraints on the emitted document are OrcaFlex behaviour rather than
stylistic preference:

**Reference before use.**  A referenced object must appear before any reference
to it — line types before lines, and vessels and 3D/6D buoys before lines,
links, winches and shapes.  The order comes from
:func:`object_section_order`, which reads it off ``BuilderRegistry``, the
dependency order the generator already emits include files in.
``post_validator._OBJECT_SECTIONS`` is a membership set and not that order: it
places ``6DBuoys`` after ``Lines``, so ordering by it would emit a line
referencing a buoy the document has not yet declared.

**No YAML anchors.**  OrcFxAPI's YAML parser rejects ``&id001`` / ``*id001``.
The package already carries :class:`.._NoAliasDumper` for exactly this; it is
reused here rather than duplicated.

The comparator for the ``BaseFile`` reference emitted here is
``template_generator.TemplateGenerator._generate_reference``, an independent
in-tree implementation, captured as goldens under
``tests/solvers/orcaflex/modular_generator/goldens/basefile/``.  The relative-path
resolution below reproduces that function's two branches deliberately.  The
override sections have no comparator; see the goldens' ``PROVENANCE.md``.
"""

from __future__ import annotations

import os
from copy import deepcopy
from pathlib import Path
from typing import Any, Mapping

import yaml

from .. import _NoAliasDumper
from ..builders import BuilderRegistry  # noqa: F401  (import registers builders)
from ..builders.generic_builder import _SECTION_ORDER
from ..post_validator import _OBJECT_SECTIONS

__all__ = ["build_variation_document", "object_section_order", "write_variation_model"]


def object_section_order() -> tuple[str, ...]:
    """The order object sections are emitted in, derived from the generator.

    OrcaFlex requires a referenced object to appear before any reference to it:
    vessels and 3D/6D buoys before lines, links, winches and shapes; type
    definitions before the instances that use them.

    The order is assembled from three in-tree authorities, strongest first, and
    is total over :data:`_OBJECT_SECTIONS`:

    1. :class:`BuilderRegistry`, the order the generator already emits include
       files in.  Each builder declares the object sections it emits as
       ``_sections``, so the registry's ``order`` argument yields a section
       order directly.  ``BuoysBuilder`` is registered at 80 and
       ``LinesBuilder`` at 90, which is why buoys precede lines here and in a
       generated ``master.yml``.
    2. ``generic_builder._SECTION_ORDER``, which places the sections no builder
       claims.  That list is documented as derived from a monolithic model's
       own ``SaveData()`` output order, and ``GenericModelBuilder`` — the last
       object-emitting builder in the registry, at order 200 — already uses it
       to order a single document holding every section, which is the shape
       emitted here.  An unclaimed section inherits the last registry anchor
       ahead of it in that list, so ``WingTypes`` sorts with the other type
       definitions rather than after ``Lines``.
    3. :data:`_OBJECT_SECTIONS` for anything the first two do not place, so the
       result stays deterministic when a new section is added.

    :data:`_OBJECT_SECTIONS` is deliberately NOT the ordering authority.  It is
    documented in ``post_validator`` as the SET of sections that define named
    objects, and it lists ``6DBuoys``, ``3DBuoys``, ``Links``, ``Winches`` and
    ``Constraints`` after ``Lines``.  Ordering by it emits a line that
    references a buoy the document has not yet declared.  It is used here only
    for membership.
    """
    known = set(_OBJECT_SECTIONS)

    # Anchors: the registered order of the earliest builder claiming a section.
    anchors: dict[str, int] = {}
    for _output_file, builder_cls in BuilderRegistry.get_ordered_builders():
        for section in getattr(builder_cls, "_sections", ()):
            if section in known and section not in anchors:
                anchors[section] = builder_cls._order

    # Rank every section `_SECTION_ORDER` knows about.  An anchored section
    # takes its registered order.  An unanchored one inherits the last anchor
    # ahead of it and keeps its `_SECTION_ORDER` position after it, so a type
    # definition no builder emits still lands among the type definitions rather
    # than at the end of the document.
    ranks: dict[str, tuple[int, int, int]] = {}
    last_anchor = -1
    offset = 0
    for index, section in enumerate(_SECTION_ORDER):
        if section not in known:
            continue
        if section in anchors:
            last_anchor = anchors[section]
            offset = 0
        else:
            offset += 1
        ranks[section] = (last_anchor, offset, index)

    # A section neither list places sorts last, in `_OBJECT_SECTIONS` order, so
    # the result is total and deterministic when a new section appears.
    tail = max((rank[0] for rank in ranks.values()), default=0) + 1
    for index, section in enumerate(_OBJECT_SECTIONS):
        if section not in ranks:
            ranks[section] = (tail, 0, index)

    return tuple(sorted(ranks, key=lambda section: ranks[section]))


def _relative_base_reference(base_path: Path, out_dir: Path) -> str:
    """Resolve ``base_path`` relative to ``out_dir``, as OrcaFlex reads it.

    Reproduces ``template_generator._generate_reference``
    (``template_generator.py:397-406``): a lexical ``Path.relative_to`` first,
    falling back to ``os.path.relpath`` on the resolved paths because
    ``relative_to`` cannot produce ``..`` traversal.  The separator is
    normalised to ``/`` on every platform.
    """
    try:
        relative = base_path.relative_to(out_dir)
    except ValueError:
        relative = Path(os.path.relpath(base_path.resolve(), out_dir.resolve()))
    return str(relative).replace("\\", "/")


def build_variation_document(
    base_path: str | Path,
    overrides: Mapping[str, Any] | None,
    out_path: str | Path,
) -> dict[str, Any]:
    """Build the variation-model document without writing it.

    Args:
        base_path: The model the variation is built on.  Either a binary
            ``.dat`` or a text ``.yml``; the extension is emitted unchanged,
            because ``BaseFile`` accepts both.  The file need not exist — only
            its path is used.
        overrides: Sections to emit after the base reference.  Object sections
            are ordered by :data:`_OBJECT_SECTIONS`; any other section is
            emitted ahead of them, in the order supplied.
        out_path: Where the document is destined.  The base reference is
            relative to this file's directory, so the path matters even when
            the file is never written.

    Returns:
        The document, key-ordered as it will be emitted.
    """
    base_path = Path(base_path)
    out_dir = Path(out_path).parent

    document: dict[str, Any] = {
        "BaseFile": _relative_base_reference(base_path, out_dir),
    }
    if not overrides:
        return document

    object_sections = set(_OBJECT_SECTIONS)

    # Non-object sections (General, Environment, VariableData, …) declare no
    # named objects, so reference-before-use does not order them.  They are
    # emitted first, in the order supplied, rather than dropped: iterating the
    # object sections alone would silently discard them.
    for section, value in overrides.items():
        if section not in object_sections:
            document[section] = deepcopy(value)

    # Object sections in dependency order, so a referenced object is always
    # declared before the object that references it.
    for section in object_section_order():
        if section in overrides:
            document[section] = deepcopy(overrides[section])

    return document


def write_variation_model(
    base_path: str | Path,
    overrides: Mapping[str, Any] | None,
    out_path: str | Path,
) -> Path:
    """Write a ``BaseFile``-composed variation model to ``out_path``.

    Args:
        base_path: See :func:`build_variation_document`.
        overrides: See :func:`build_variation_document`.
        out_path: Destination file.  Parent directories are created.

    Returns:
        The path written.
    """
    out_path = Path(out_path)
    document = build_variation_document(base_path, overrides, out_path)

    out_path.parent.mkdir(parents=True, exist_ok=True)
    with open(out_path, "w", encoding="utf-8") as handle:
        yaml.dump(
            document,
            handle,
            Dumper=_NoAliasDumper,
            default_flow_style=False,
            allow_unicode=True,
            sort_keys=False,
        )
    return out_path
