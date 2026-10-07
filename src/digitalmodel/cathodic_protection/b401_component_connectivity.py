"""Component input and electrical-continuity validation for B401 risers."""

from __future__ import annotations

import math
from collections import deque
from collections.abc import Mapping
from typing import Any

from digitalmodel.cathodic_protection.b401_component_allocations import named_rows
from digitalmodel.cathodic_protection.b401_component_zones import positive


def components(inputs: Mapping[str, Any]) -> dict[str, dict[str, Any]]:
    """Validate component-local lives and electrochemical environments."""
    rows = named_rows(inputs.get("components"), "components")
    for name, row in rows.items():
        row["design_life_years"] = positive(
            row.get("design_life_years"), f"component {name} design_life_years"
        )
        environment = row.get("environment")
        if not isinstance(environment, Mapping):
            raise ValueError(f"component {name} environment is required")
        env = dict(environment)
        for key in (
            "seawater_temperature_C",
            "electrolyte_resistivity_ohm_m",
            "anode_surface_temperature_C",
        ):
            if key not in env:
                raise ValueError(f"component {name} environment {key} is required")
            value = float(env[key])
            if not math.isfinite(value) or ("resistivity" in key and value <= 0.0):
                raise ValueError(f"component {name} environment {key} is invalid")
            env[key] = value
        if env.get("anode_environment") not in {"seawater", "sediment"}:
            raise ValueError(f"component {name} anode_environment is invalid")
        row["environment"] = env
    return rows


def continuity(
    raw: Any, component_rows: Mapping[str, Mapping[str, Any]]
) -> dict[str, dict[str, float]]:
    """Build an undirected graph from unique, finite-duration edges."""
    graph: dict[str, dict[str, float]] = {name: {} for name in component_rows}
    seen: set[tuple[str, str]] = set()
    for edge in raw or []:
        ends = edge.get("components") if isinstance(edge, Mapping) else None
        if not isinstance(ends, list) or len(ends) != 2:
            raise ValueError("electrical continuity edges require two components")
        left, right = map(str, ends)
        if left not in graph or right not in graph or left == right:
            raise ValueError(
                "electrical continuity edge has unknown or identical endpoints"
            )
        key = (left, right) if left < right else (right, left)
        if key in seen:
            raise ValueError("duplicate electrical continuity edge")
        seen.add(key)
        life = positive(edge.get("available_for_years"), "available_for_years")
        graph[left][right] = life
        graph[right][left] = life
    return graph


def continuity_path(
    host: str,
    target: str,
    life: float,
    graph: Mapping[str, Mapping[str, float]],
    component_rows: Mapping[str, Mapping[str, Any]],
) -> list[str]:
    """Select the deterministic shortest path after filtering by protected life."""
    if float(component_rows[host]["design_life_years"]) < life:
        raise ValueError(f"electrical continuity host {host!r} has insufficient life")
    if host == target:
        return [host]
    queue: deque[list[str]] = deque([[host]])
    visited = {host}
    while queue:
        path = queue.popleft()
        for neighbor in sorted(graph[path[-1]]):
            if neighbor in visited or graph[path[-1]][neighbor] < life:
                continue
            if float(component_rows[neighbor]["design_life_years"]) < life:
                continue
            candidate = [*path, neighbor]
            if neighbor == target:
                return candidate
            visited.add(neighbor)
            queue.append(candidate)
    raise ValueError(f"no electrical continuity path from {host!r} to {target!r}")


def continuity_records(
    host: str,
    targets: Mapping[str, Any],
    component_rows: Mapping[str, Mapping[str, Any]],
    graph: Mapping[str, Mapping[str, float]],
) -> tuple[dict[str, list[str]], dict[str, dict[str, Any]]]:
    """Return resolved paths and explicit protected-life evidence."""
    paths = {
        target: continuity_path(
            host,
            target,
            float(component_rows[target]["design_life_years"]),
            graph,
            component_rows,
        )
        for target in sorted(targets)
    }
    checks = {
        target: {
            "protected_life_years": float(component_rows[target]["design_life_years"]),
            "minimum_edge_availability_years": None
            if len(path) == 1
            else min(
                (graph[left][right] for left, right in zip(path, path[1:])),
            ),
            "host_life_check": float(component_rows[host]["design_life_years"])
            >= float(component_rows[target]["design_life_years"]),
            "life_check": True,
        }
        for target, path in paths.items()
    }
    return paths, checks


__all__ = ["components", "continuity", "continuity_path", "continuity_records"]
