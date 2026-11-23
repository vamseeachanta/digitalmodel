"""Shared wireframe constants for box-shaped buoy builders.

Extracted verbatim from buoys_builder.py lines 18-42 as part of #504 refactor
(plan: workspace-hub/docs/plans/2026-04-24-issue-504-orcaflex-buoys-builder-refactor.md).

Single source of truth for the four sub-builders (RollerBuilder, TugBuilder,
EndBuoyBuilder) that share the default cube wireframe. BuoyancyBuilder uses
its own bespoke smaller-box wireframe and does NOT import these.
"""
from __future__ import annotations


DEFAULT_WIREFRAME_VERTICES = [
    [3, 3, 3],
    [-3, 3, 3],
    [-3, -3, 3],
    [3, -3, 3],
    [3, 3, -3],
    [-3, 3, -3],
    [-3, -3, -3],
    [3, -3, -3],
]

DEFAULT_WIREFRAME_EDGES = [
    [1, 2, None],
    [2, 3, None],
    [3, 4, None],
    [4, 1, None],
    [5, 6, None],
    [6, 7, None],
    [7, 8, None],
    [8, 5, None],
    [1, 5, None],
    [2, 6, None],
    [3, 7, None],
    [4, 8, None],
]
