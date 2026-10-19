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

Object overrides are modifications of existing named objects. List input is
converted to name-keyed mappings, because list syntax can recreate objects and
detach their connections (issue #2137). Creation, deletion, ordering and renaming
are rejected rather than inferred. Format source: Orcina, OrcaFlex online help,
"Variation models", "Text data format" and "Change tracking", accessed
2026-10-19:
https://www.orcina.com/webhelp/OrcaFlex/Content/html/Variationmodels.htm

This is a syntax guard for the known top-level object sections, not a solver
comparison. Object existence, nested named collections and preservation of all
unselected data after native resolution still require separate qualification.
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
from ..post_validator import _KNOWN_TOP_LEVEL_KEYS, _OBJECT_SECTIONS

__all__ = [
    "build_variation_document",
    "object_section_order",
    "validate_variation_document",
    "write_variation_model",
]

_COMPOSITION_DIRECTIVES = frozenset({"basefile", "basefilenotracking", "includefile"})


def _validate_object_modifications(section: str, objects: Mapping[str, Any]) -> None:
    """Reject structural operations in a name-keyed property override."""
    # Orcina's Text data format reserves these mapping keys for structural
    # operations. This writer only modifies properties of existing objects.
    for name, properties in objects.items():
        if not isinstance(name, str) or not name.strip():
            raise ValueError(f"{section}: object names must be nonempty text")
        if name.casefold() in {"new", "deleted", "order"}:
            raise ValueError(f"{section}: structural directives are not property overrides")
        if not isinstance(properties, Mapping):
            raise ValueError(f"{section}: each named object needs a property mapping")
        # Change tracking also permits a trailing '*' on a data item name
        # (Orcina, Variation models, Change tracking); it must not bypass this.
        if "Name" in properties or "Name*" in properties:
            raise ValueError(f"{section}: renaming is not a property override")


def _object_modifications(section: str, value: Any) -> dict[str, Any]:
    """Convert object-list input to modification syntax without changing inputs."""
    if isinstance(value, list):
        objects: dict[str, Any] = {}
        for item in value:
            if not isinstance(item, Mapping) or "Name" not in item:
                raise ValueError(f"{section}: each object needs a Name")
            name = item["Name"]
            if not isinstance(name, str) or not name.strip():
                raise ValueError(f"{section}: object names must be nonempty text")
            if name in objects:
                raise ValueError(f"{section}: duplicate object name")
            objects[name] = {key: val for key, val in item.items() if key != "Name"}
    elif isinstance(value, Mapping):
        objects = dict(value)
    else:
        raise ValueError(f"{section}: expected a named object list or mapping")

    _validate_object_modifications(section, objects)
    return deepcopy(objects)


def validate_variation_document(document: Mapping[str, Any]) -> None:
    """Check parsed, property-only variation syntax without resolving a model.

    Unlike writer input, an artifact must already use name-keyed mappings for
    the known top-level object sections. Lists are rejected, never repaired:
    a dictionary merge cannot demonstrate native object identity preservation.
    BaseFile must precede edits; extra composition directives are forbidden.
    Unknown top-level sections are rejected, including legacy singular-object
    creation and deletion syntax, rather than passed through unchecked.
    Raises ValueError on a syntax violation and leaves the document unchanged.

    This guard does not check object existence, nested named collections,
    duplicate YAML keys lost during parsing, or native resolved-model data.
    """
    if not isinstance(document, Mapping) or not document:
        raise ValueError("Variation document must be a nonempty mapping")
    if next(iter(document)) != "BaseFile":
        raise ValueError("BaseFile must precede variation overrides")
    base = document["BaseFile"]
    if not isinstance(base, str) or not base.strip():
        raise ValueError("BaseFile must be a nonempty reference")
    for section, value in document.items():
        if not isinstance(section, str):
            raise ValueError("Variation section names must be text")
        if section != "BaseFile" and section.casefold() in _COMPOSITION_DIRECTIVES:
            raise ValueError("Variation must not contain extra composition directives")
        if section not in _KNOWN_TOP_LEVEL_KEYS:
            raise ValueError("Unrecognised variation section; use a supported section")
        if section in _OBJECT_SECTIONS:
            if not isinstance(value, Mapping):
                raise ValueError(f"{section}: artifact must use named-object mappings")
            _validate_object_modifications(section, value)


def _check_source_destination(base_path: Path, out_path: Path) -> None:
    """Reject lexical aliases, symbolic links and hard links to the source."""
    same_path = base_path.resolve() == out_path.resolve()
    if not same_path and base_path.exists() and out_path.exists():
        same_path = os.path.samefile(base_path, out_path)
    if same_path:
        raise ValueError("Variation destination must not overwrite the source model")


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
            accept lists with unique Name fields or name-keyed property mappings.
            Both emit name-keyed modifications in dependency order. Structural
            directives and renaming raise ValueError. Other supported sections
            are emitted ahead of objects, in the order supplied; property tables
            stay lists. Unrecognised top-level sections raise ValueError.
        out_path: Where the document is destined.  The base reference is
            relative to this file's directory, so the path matters even when
            the file is never written.

    Returns:
        The document, key-ordered as it will be emitted.
    """
    base_path = Path(base_path)
    _check_source_destination(base_path, Path(out_path))
    out_dir = Path(out_path).parent

    document: dict[str, Any] = {
        "BaseFile": _relative_base_reference(base_path, out_dir),
    }
    if overrides is not None and not isinstance(overrides, Mapping):
        raise ValueError("Overrides must be a mapping of section names to values")
    if not overrides:
        return document

    if any(not isinstance(section, str) for section in overrides):
        raise ValueError("Override section names must be text")
    if any(
        section.casefold() in _COMPOSITION_DIRECTIVES for section in overrides
    ):
        raise ValueError("Overrides must not contain composition directives")

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
            document[section] = _object_modifications(section, overrides[section])

    validate_variation_document(document)
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
