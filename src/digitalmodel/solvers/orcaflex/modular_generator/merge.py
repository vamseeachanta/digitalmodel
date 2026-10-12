"""Name-keyed list merge with whole-object replacement semantics.

Ported from ``digitalmodel.solvers.orcaflex.template_generator.TemplateGenerator
._merge_object_lists`` (``template_generator.py:273-323``). The source module is
unchanged and remains the comparator for this port.

OrcaFlex variation models replace an object wholesale when an override names it:
the override entry supersedes the base entry in full rather than being merged
into it field by field. Base order is preserved, and names absent from the base
append in override order.
"""

from __future__ import annotations

from copy import deepcopy
from typing import Any


def merge_named_lists(
    base_list: list[Any],
    override_list: list[Any],
) -> list[Any]:
    """Merge two object lists by matching on the ``Name`` key.

    An entry in ``override_list`` whose ``Name`` matches a base entry **replaces
    that entry entirely** — no field-level merge is performed, so a key present
    only in the base entry does not survive. Entries whose ``Name`` is absent
    from the base append after the base entries, in override order. Every
    retained and appended entry is deep copied, so the returned list shares no
    mutable state with either input.

    Falsy entries (``None``, ``{}``) in either list are skipped.

    Args:
        base_list: Base list of objects.
        override_list: Override list of objects; takes precedence by ``Name``.

    Returns:
        A new list. Base order is preserved; unmatched override entries append.
    """
    # Name keying applies only when every truthy entry in BOTH lists is a dict
    # carrying a 'Name'. Otherwise the function falls back to replacing the list
    # wholesale.
    #
    # CAVEAT: this fallback is a local convenience inherited from
    # template_generator._merge_object_lists. Its correspondence to OrcaFlex
    # behaviour is NOT established, and establishing it is out of scope. It is
    # ported to preserve the existing contract, not because the vendor format is
    # known to behave this way.
    has_names = (
        all(isinstance(item, dict) and "Name" in item for item in base_list if item)
        and all(isinstance(item, dict) and "Name" in item for item in override_list if item)
    )

    if not has_names:
        return deepcopy(override_list) if override_list else deepcopy(base_list)

    override_by_name = {item["Name"]: item for item in override_list if item}

    result: list[Any] = []
    seen_names: set[Any] = set()

    # Base entries first, in base order, each replaced wholesale where overridden.
    for item in base_list:
        if item and "Name" in item:
            name = item["Name"]
            seen_names.add(name)
            result.append(deepcopy(override_by_name.get(name, item)))

    # Then override entries naming objects the base does not carry.
    for item in override_list:
        if item and "Name" in item and item["Name"] not in seen_names:
            result.append(deepcopy(item))

    return result
