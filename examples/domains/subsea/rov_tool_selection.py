# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Compose capability selection with independent ROV interface screening.

Run offline from the repository root with::

    PYTHONPATH=src uv run --no-project python examples/domains/subsea/rov_tool_selection.py

Synthetic input basis (all numerical inputs below): the torque duty is 20 N m
and 5 turns. Known capacities equal the duty, except the low-capacity tool at
19 N m; the unknown tool has no torque rating. The nominal tool box is
(0.2, 0.3, 0.4) m with 0.5 m approach demand. The target provides
(0.3, 0.4, 0.5) m and 0.6 m approach. Independent counterexamples increase the
first tool dimension to 0.4 m or approach demand to 0.7 m. These are deliberately
chosen teaching inputs, not equipment data or standard-derived ratings. Class
labels "1" and "2" use the existing core's allowed torque identifiers, with no
rating inferred from either label (see interface_checks.ToolInterface).

For every requested capacity q, selection requires a known capacity c >= q and
a matching tool kind. Unknown c cannot satisfy q. Separately, mating requires
equal interface family and class, each fixed-orientation tool dimension <= its
target dimension, required approach <= available approach, and distance <= reach.
No inequality compensates for failure of another. A tool satisfying duty can
therefore fail geometry. The geometry screen runs only on capability matches.

The target location (3, 4, 0) m gives a right triangle: d**2 = 3**2 + 4**2 = 25,
so d = 5 m. The default 5 m reach includes this exact boundary; a limit just
below it fails. This ideal spherical bound does not model collisions, joint
limits, orientation changes, seals or mating tolerances. API 17H edition/clause
metadata and qualified equipment data remain open; no compliance is claimed.
"""

from __future__ import annotations

import json
from dataclasses import asdict, replace

from digitalmodel.subsea.rov.interface_checks import (
    InterfaceKind,
    InterfaceSpec,
    ToolInterface,
    check_interface,
)
from digitalmodel.subsea.rov.tool_library import (
    TaskDuty,
    ToolKind,
    ToolSpec,
    find_tools,
)


def select_and_screen(manipulator_reach_m: float = 5.0) -> dict[str, object]:
    """Return separate capacity and geometry outcomes for the synthetic basis.

    Args:
        manipulator_reach_m: Ideal spherical reach in metres; the default is
            the hand-calculated right-triangle distance in the module basis.

    Returns:
        Capacity matches with their interface demands, capacity exclusions and
        geometry outcomes for each capacity match.
    """
    # All values cite the synthetic input basis in the module docstring.
    nominal_interface = ToolInterface(
        kind=InterfaceKind.TORQUE,
        class_id="1",
        envelope_m=(0.2, 0.3, 0.4),
        required_approach_m=0.5,
    )
    nominal = ToolSpec(
        name="synthetic-fit",
        kind=ToolKind.TORQUE,
        interface=nominal_interface,
        torque_nm=20.0,
        turns=5.0,
    )
    tools = (
        nominal,
        replace(
            nominal,
            name="synthetic-wide",
            interface=replace(nominal_interface, envelope_m=(0.4, 0.3, 0.4)),
        ),
        replace(
            nominal,
            name="synthetic-family",
            interface=replace(nominal_interface, kind=InterfaceKind.HOT_STAB),
        ),
        replace(
            nominal,
            name="synthetic-class",
            interface=replace(nominal_interface, class_id="2"),
        ),
        replace(
            nominal,
            name="synthetic-approach",
            interface=replace(nominal_interface, required_approach_m=0.7),
        ),
        replace(nominal, name="synthetic-low", torque_nm=19.0),
        replace(nominal, name="synthetic-unknown", torque_nm=None),
    )
    duty = TaskDuty(kind=ToolKind.TORQUE, torque_nm=20.0, turns=5.0)
    target = InterfaceSpec(
        kind=InterfaceKind.TORQUE,
        class_id="1",
        envelope_m=(0.3, 0.4, 0.5),
        approach_clearance_m=0.6,
        location_m=(3.0, 4.0, 0.0),
    )

    matches = find_tools(tools, duty)
    return {
        "capacity_matches": [
            {"name": tool.name, "interface": asdict(tool.interface)}
            for tool in matches
        ],
        "capacity_rejections": [tool.name for tool in tools if tool not in matches],
        "geometry": {
            tool.name: asdict(check_interface(tool.interface, target, manipulator_reach_m))
            for tool in matches
        },
    }


if __name__ == "__main__":
    print(json.dumps(select_and_screen()))
