# ABOUTME: Knowledge query interface for naval architecture data
# ABOUTME: Searches worked examples, equations, and ship data by topic
"""
Naval architecture knowledge query interface.

Provides search across extracted worked examples, equations, ship data,
and regulatory requirements. Used by the expert skill to find source-
traced answers.

Usage:
    from digitalmodel.naval_architecture.knowledge import query_knowledge
    results = query_knowledge("stability", "DDG-51")
"""

import json
from pathlib import Path
from typing import Optional

from digitalmodel.naval_architecture.ship_data import (
    get_cross_curves,
    get_curves_of_form,
    get_ship,
    list_ships,
)

# Module lookup for routing queries to calculation functions
_MODULE_MAP = {
    "stability": "stability",
    "gz": "stability",
    "righting arm": "stability",
    "metacentric": "hydrostatics",
    "buoyancy": "hydrostatics",
    "inclining": "hydrostatics",
    "holtrop": "holtrop_mennen",
    "resistance": "resistance",
    "ittc": "resistance",
    "powering": "resistance",
    "seakeeping": "seakeeping",
    "roll": "seakeeping",
    "heave": "seakeeping",
    "pitch": "seakeeping",
    "rao": "seakeeping",
    "maneuver": "maneuverability",
    "rudder": "maneuverability",
    "turning": "maneuverability",
    "imo": "compliance",
    "regulatory": "compliance",
    "solas": "compliance",
    "freeboard": "compliance",
    "hull form": "hull_form",
    "block coefficient": "hull_form",
    "prismatic": "hull_form",
    "damage": "damage_stability",
    "flooding": "damage_stability",
    "lost buoyancy": "damage_stability",
}


def find_module(topic: str) -> Optional[str]:
    """Map a topic keyword to the relevant calculation module."""
    topic_lower = topic.lower()
    for keyword, module in _MODULE_MAP.items():
        if keyword in topic_lower:
            return module
    return None


def find_ship_data(query: str) -> list[dict]:
    """Search ship registry for matching vessels."""
    results = []
    query_lower = query.lower()
    for hull_id in list_ships():
        ship = get_ship(hull_id)
        if not ship:
            continue
        name_lower = ship["name"].lower()
        if query_lower in hull_id.lower() or query_lower in name_lower:
            entry = {"hull_id": hull_id, **ship}
            curves = get_cross_curves(hull_id)
            if curves:
                entry["has_cross_curves"] = True
            cof = get_curves_of_form(hull_id)
            if cof:
                entry["has_curves_of_form"] = True
            results.append(entry)
    return results


def query_knowledge(
    topic: str,
    vessel: str = "",
    jsonl_path: Optional[str] = None,
) -> dict:
    """Query naval architecture knowledge base.

    Returns:
        Dict with: module, ship_data, worked_examples, suggestions
    """
    result = {
        "topic": topic,
        "module": find_module(topic),
        "ship_data": [],
        "worked_examples": [],
        "suggestions": [],
    }

    # Ship data lookup
    if vessel:
        result["ship_data"] = find_ship_data(vessel)

    # Worked examples search
    if jsonl_path:
        result["worked_examples"] = _search_examples(
            topic, jsonl_path
        )

    # Suggestions
    if result["module"]:
        result["suggestions"].append(
            f"Use digitalmodel.naval_architecture.{result['module']}"
        )
    if not result["ship_data"] and vessel:
        result["suggestions"].append(
            f"No ship data for '{vessel}'. Available: {list_ships()}"
        )

    return result


def _search_examples(topic: str, jsonl_path: str) -> list[dict]:
    """Search worked examples JSONL for topic matches."""
    path = Path(jsonl_path)
    if not path.exists():
        return []

    topic_lower = topic.lower()
    matches = []
    with open(path) as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            try:
                record = json.loads(line)
            except json.JSONDecodeError:
                continue
            title = record.get("title", "").lower()
            if topic_lower in title:
                matches.append(record)

    return matches[:20]  # cap at 20 results
