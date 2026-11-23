"""Shared builders for offline structural finite-element workflows."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any

import numpy as np

from digitalmodel.infrastructure.base_solvers.structural.elements.beam_element import (
    BeamElement,
)


REPO_ROOT = Path(__file__).resolve().parents[4]


@dataclass(frozen=True)
class BeamModel:
    nodes: dict[int, np.ndarray]
    elements: list[BeamElement]
    boundary_conditions: dict[str, Any]
    loads: list[dict[str, Any]]
    material: dict[str, Any]


def build_beam_model(settings: dict[str, Any], workflow_name: str) -> BeamModel:
    """Build solver-ready beam model data from a workflow config section."""
    material = _material(settings.get("material") or {}, workflow_name)
    nodes = _nodes(settings.get("nodes"), workflow_name)
    elements = _elements(settings.get("elements"), nodes, material, workflow_name)
    boundary_conditions = _boundary_conditions(
        settings.get("boundary_conditions"), nodes, workflow_name
    )
    loads = _loads(settings.get("loads"), nodes, workflow_name)
    return BeamModel(
        nodes=nodes,
        elements=elements,
        boundary_conditions=boundary_conditions,
        loads=loads,
        material=material,
    )


def results_csv_path(
    cfg: dict[str, Any], settings: dict[str, Any], basename: str
) -> Path:
    cfg_dir = _config_dir(cfg)
    output_dir = Path(settings.get("output_dir", "results"))
    if not output_dir.is_absolute():
        output_dir = cfg_dir / output_dir
    return output_dir / f"{_input_stem(cfg, basename)}_{basename}.csv"


def display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return str(resolved)


def _nodes(raw_nodes: Any, workflow_name: str) -> dict[int, np.ndarray]:
    if raw_nodes is None:
        raise ValueError(f"{workflow_name} nodes are required")

    nodes: dict[int, np.ndarray] = {}
    if isinstance(raw_nodes, list):
        for item in raw_nodes:
            node_id, coords = _node_item(item, workflow_name)
            nodes[node_id] = coords
    elif isinstance(raw_nodes, dict):
        for key, value in raw_nodes.items():
            node_id = int(key)
            nodes[node_id] = _node_coords(value, workflow_name)
    else:
        raise TypeError(f"{workflow_name} nodes must be a list or mapping")

    if not nodes:
        raise ValueError(f"{workflow_name} nodes must not be empty")
    expected_ids = set(range(len(nodes)))
    if set(nodes) != expected_ids:
        raise ValueError(
            f"{workflow_name} node ids must be zero-based contiguous: "
            f"expected {sorted(expected_ids)}, got {sorted(nodes)}"
        )
    return nodes


def _node_item(item: Any, workflow_name: str) -> tuple[int, np.ndarray]:
    if not isinstance(item, dict) or item.get("id") is None:
        raise ValueError(f"{workflow_name} node entries require id, x, and y")
    return int(item["id"]), _node_coords(item, workflow_name)


def _node_coords(value: Any, workflow_name: str) -> np.ndarray:
    if isinstance(value, dict):
        if value.get("coordinates") is not None:
            coords = value["coordinates"]
        else:
            coords = [value["x"], value["y"]]
    elif isinstance(value, (list, tuple)):
        coords = value
    else:
        raise TypeError(f"{workflow_name} node coordinates must be a mapping or list")

    if len(coords) != 2:
        raise ValueError(f"{workflow_name} node coordinates must have x and y")
    return np.array([float(coords[0]), float(coords[1])], dtype=float)


def _elements(
    raw_elements: Any,
    nodes: dict[int, np.ndarray],
    material: dict[str, Any],
    workflow_name: str,
) -> list[BeamElement]:
    if not isinstance(raw_elements, list) or not raw_elements:
        raise ValueError(f"{workflow_name} elements must be a non-empty list")

    elements: list[BeamElement] = []
    for item in raw_elements:
        if not isinstance(item, dict):
            raise TypeError(f"{workflow_name} element entries must be mappings")
        elem_id = int(item["id"])
        node1 = int(item["node1"])
        node2 = int(item["node2"])
        _require_node(node1, nodes, workflow_name)
        _require_node(node2, nodes, workflow_name)
        elements.append(
            BeamElement(
                elem_id=elem_id,
                node1=node1,
                node2=node2,
                coord1=nodes[node1],
                coord2=nodes[node2],
                E=float(item.get("E", material["E"])),
                I=float(item["I"]),
                A=float(item["A"]),
            )
        )
    return elements


def _boundary_conditions(
    raw_bcs: Any,
    nodes: dict[int, np.ndarray],
    workflow_name: str,
) -> dict[str, Any]:
    if not isinstance(raw_bcs, dict) or not raw_bcs:
        raise ValueError(
            f"{workflow_name} boundary_conditions must be a non-empty mapping"
        )

    bcs = dict(raw_bcs)
    fixed_nodes = [int(node) for node in bcs.get("fixed_nodes", [])]
    for node in fixed_nodes:
        _require_node(node, nodes, workflow_name)
    if fixed_nodes:
        bcs["fixed_nodes"] = fixed_nodes
    return bcs


def _loads(
    raw_loads: Any,
    nodes: dict[int, np.ndarray],
    workflow_name: str,
) -> list[dict[str, Any]]:
    if not isinstance(raw_loads, list) or not raw_loads:
        raise ValueError(f"{workflow_name} loads must be a non-empty list")

    loads: list[dict[str, Any]] = []
    for item in raw_loads:
        if not isinstance(item, dict):
            raise TypeError(f"{workflow_name} load entries must be mappings")
        node = int(item["node"])
        _require_node(node, nodes, workflow_name)
        direction = str(item["direction"]).lower()
        if direction not in {"x", "y"}:
            raise ValueError(f"{workflow_name} load direction must be 'x' or 'y'")
        loads.append(
            {
                "node": node,
                "direction": direction,
                "magnitude": float(item["magnitude"]),
            }
        )
    return loads


def _material(raw_material: dict[str, Any], workflow_name: str) -> dict[str, Any]:
    material = {
        "E": _required_float(raw_material, "E", workflow_name),
        "nu": _required_float(raw_material, "nu", workflow_name),
        "rho": float(raw_material.get("rho", 7850.0)),
        "sigma_y": _required_float(raw_material, "sigma_y", workflow_name),
    }
    return material


def _required_float(source: dict[str, Any], name: str, workflow_name: str) -> float:
    if source.get(name) is None:
        raise ValueError(f"{workflow_name} material.{name} is required")
    return float(source[name])


def _require_node(
    node_id: int, nodes: dict[int, np.ndarray], workflow_name: str
) -> None:
    if node_id not in nodes:
        raise ValueError(f"{workflow_name} references missing node id {node_id}")


def _config_dir(cfg: dict[str, Any]) -> Path:
    if cfg.get("_config_dir_path"):
        return Path(cfg["_config_dir_path"])
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).parent
    return Path.cwd()


def _input_stem(cfg: dict[str, Any], basename: str) -> str:
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).stem
    return str(cfg.get("basename", basename))
