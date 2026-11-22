"""Directed control connectivity with explicit component and link identities.

Nodes represent hosts, manifolds, subsea control modules (SCMs) and trees.
Links represent caller-supplied directed connectivity, including parallel links
for dual umbilicals. Separate SCM nodes can describe alternate control paths.
All components of the graph must be acyclic; disconnected nodes are permitted.

Control and architecture labels describe caller intent only. Reachability does
not establish hydraulic/electrical feasibility, independent redundancy, FMEA,
signal quality, availability or safety. No dimensional quantities or unit
conversions are used. API 17F / ISO 13628-6 edition and clause mapping remain
unverified; this module makes no standards-compliance claim.
"""

from __future__ import annotations

from collections import deque
from collections.abc import Iterable
from dataclasses import dataclass


def _validate_id(value: str, field: str) -> None:
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{field} must be a nonblank string")


@dataclass(frozen=True)
class Node:
    """An identified component with kind host, manifold, scm or tree."""

    id: str
    kind: str

    def __post_init__(self) -> None:
        """Validate identity and the supported component kind."""
        _validate_id(self.id, "node.id")
        if self.kind not in ("host", "manifold", "scm", "tree"):
            raise ValueError("node.kind must be host, manifold, scm or tree")


@dataclass(frozen=True)
class Link:
    """An identified directed connection; parallel endpoints are permitted."""

    id: str
    source: str
    target: str

    def __post_init__(self) -> None:
        """Validate link and endpoint identities."""
        for field in ("id", "source", "target"):
            _validate_id(getattr(self, field), f"link.{field}")


@dataclass(frozen=True)
class ControlTopology:
    """An immutable directed acyclic graph rooted for queries at host_id.

    Input sequences are copied to tuples. Node IDs and link IDs each occupy
    their own namespace. Every link is a possible directed path; node kind
    does not impose a physical connection rule. Both labels must be explicit,
    with no inference of compatibility between them.
    """

    nodes: tuple[Node, ...]
    links: tuple[Link, ...]
    host_id: str
    control_mode: str
    architecture: str

    def __post_init__(self) -> None:
        """Validate identities, endpoints, labels, designated host and cycles."""
        object.__setattr__(self, "nodes", tuple(self.nodes))
        object.__setattr__(self, "links", tuple(self.links))
        node_by_id = {node.id: node for node in self.nodes}
        if len(node_by_id) != len(self.nodes):
            raise ValueError("duplicate node.id")
        if len({link.id for link in self.links}) != len(self.links):
            raise ValueError("duplicate link.id")
        _validate_id(self.host_id, "host_id")
        if self.host_id not in node_by_id or node_by_id[self.host_id].kind != "host":
            raise ValueError("host_id must identify an existing host node")
        if self.control_mode not in ("multiplex", "direct-hydraulic"):
            raise ValueError("control_mode must be multiplex or direct-hydraulic")
        if self.architecture not in ("electro-hydraulic", "all-electric"):
            raise ValueError("architecture must be electro-hydraulic or all-electric")

        successors: dict[str, list[str]] = {node.id: [] for node in self.nodes}
        indegree = dict.fromkeys(node_by_id, 0)
        for link in self.links:
            if link.source not in node_by_id or link.target not in node_by_id:
                raise ValueError(f"link endpoint unknown for link.id {link.id!r}")
            if link.source == link.target:
                raise ValueError(f"self link for link.id {link.id!r}")
            successors[link.source].append(link.target)
            indegree[link.target] += 1
        pending = deque(node_id for node_id, count in indegree.items() if count == 0)
        visited = 0
        while pending:
            visited += 1
            for target in successors[pending.popleft()]:
                indegree[target] -= 1
                if indegree[target] == 0:
                    pending.append(target)
        if visited != len(self.nodes):
            raise ValueError("links contain a directed cycle")

    def reachable_tree_ids(
        self,
        disabled_nodes: Iterable[str] = (),
        disabled_links: Iterable[str] = (),
    ) -> tuple[str, ...]:
        """Return sorted tree IDs reachable from the designated host.

        Empty fault sets mean all nodes and links are enabled. Disabled nodes
        block every incident link; disabled links block only their own identity.
        Unknown disabled IDs raise ValueError rather than silently having no
        effect. Faults are applied only to this query, never to the input graph.
        """
        node_ids = {node.id for node in self.nodes}
        link_ids = {link.id for link in self.links}
        blocked_nodes, blocked_links = set(disabled_nodes), set(disabled_links)
        if blocked_nodes - node_ids:
            raise ValueError("disabled_nodes contains unknown node IDs")
        if blocked_links - link_ids:
            raise ValueError("disabled_links contains unknown link IDs")
        successors: dict[str, list[str]] = {node.id: [] for node in self.nodes}
        for link in self.links:
            if (link.id not in blocked_links and link.source not in blocked_nodes
                    and link.target not in blocked_nodes):
                successors[link.source].append(link.target)
        reached = set() if self.host_id in blocked_nodes else {self.host_id}
        pending = list(reached)
        while pending:
            for target in successors[pending.pop()]:
                if target not in reached:
                    reached.add(target)
                    pending.append(target)
        return tuple(sorted(node.id for node in self.nodes
                            if node.kind == "tree" and node.id in reached))
