"""Offline beam compatibility policy approved in issue #1676 (13 Nov 2026).

CIRC/B31 follows the issue's observed CalculiX v2.21 rejection. It is a
requested policy, not a universal prohibition established by the 2.22 manual.
PIPE/B32R follows the approved review's inference from manual 2.22 section 6.3:
general sections expand to C3D20R, the B32R expansion. This is an inference,
not a direct normative element restriction or a new solver verification.
"""

from numbers import Integral


def validate_beam_sections(elements, element_sets, sections, *,
                           element_type_map, beam_types, shell_types):
    """Validate CIRC/PIPE targets without changing mesh or writer state.

    IDs are global zero-based indices counting only exported element groups.
    Repeated explicit sets and generated sets of the same name form a union.
    Retain raw IDs until validation: True and 1.0 compare equal to integer 1.
    Other section shapes retain the writer's existing behavior.
    """
    restricted = [s for s in sections if s["section_type"] in {"CIRC", "PIPE"}]
    if not restricted:
        return

    groups, kinds = {}, []
    for name, data in elements.items():
        kind = element_type_map.get(name)
        if kind is None:
            continue
        start = len(kinds)
        kinds.extend([kind] * len(data["connectivity"]))
        if kind in beam_types:
            target = f"EBEAM_{kind}"
        elif kind in shell_types:
            target = f"ESHELL_{kind}"
        else:
            target = "EALL"
        groups.setdefault(target, []).extend(range(start, len(kinds)))
    for group in element_sets:
        groups.setdefault(group["name"], []).extend(group["ids"])

    for section in restricted:
        shape, target = section["section_type"], section["elset"]
        context = f"{shape} beam section target '{target}'"
        ids = groups.get(target, [])
        if not ids:
            raise ValueError(f"{context} requires a nonempty resolved element set")
        for eid in ids:
            if (not isinstance(eid, Integral) or isinstance(eid, bool)
                    or eid < 0 or eid >= len(kinds)):
                raise ValueError(
                    f"{context}: invalid exported element ID {eid!r}; "
                    "require an integer global zero-based index into the exported mesh"
                )
        exported = {kinds[eid] for eid in ids}
        found = ", ".join(sorted(exported))
        if not exported <= beam_types:
            raise ValueError(
                f"{context} requires a beam-only set; found exported type(s): {found}"
            )
        if shape == "CIRC" and exported != {"B32"}:
            raise ValueError(
                f"{context} requires B32; found exported type(s): {found}. "
                "Issue #1676 policy follows recorded CalculiX v2.21 CIRC/B31 rejection; "
                "writer documentation baseline is 2.22"
            )
        if shape == "PIPE" and exported != {"B32R"}:
            raise ValueError(
                f"{context} requires B32R, unsupported by this writer; "
                f"found exported type(s): {found}. Issue #1676 policy follows the "
                "approved review's inference from CalculiX 2.22 section 6.3"
            )
