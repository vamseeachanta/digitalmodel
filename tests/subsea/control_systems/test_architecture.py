"""Behavioural checks for topology validation and directed reachability."""

import json
from pathlib import Path
import runpy
import subprocess
import sys
from dataclasses import FrozenInstanceError

import pytest

from digitalmodel.subsea.control_systems import ControlTopology, Link, Node


def topology(nodes=None, links=None, **kwargs):
    """Build two separately controlled trees with parallel umbilical links."""
    return ControlTopology(
        nodes=nodes if nodes is not None else (
            Node("host", "host"), Node("manifold", "manifold"),
            Node("scm-a", "scm"), Node("scm-b", "scm"),
            Node("tree-a", "tree"), Node("tree-b", "tree"),
        ),
        links=links if links is not None else (
            Link("umb-a", "host", "manifold"),
            Link("umb-b", "host", "manifold"),
            Link("control-a", "manifold", "scm-a"),
            Link("control-b", "manifold", "scm-b"),
            Link("well-a", "scm-a", "tree-a"),
            Link("well-b", "scm-b", "tree-b"),
        ),
        host_id="host", control_mode="multiplex",
        architecture="electro-hydraulic", **kwargs,
    )


@pytest.mark.parametrize(
    "nodes, links, expected",
    [((), (), ("tree-a", "tree-b")),
     (("scm-a",), (), ("tree-b",)),
     ((), ("control-b",), ("tree-a",)),
     ((), ("umb-a",), ("tree-a", "tree-b")),
     ((), ("umb-a", "umb-b"), ()),
     (("manifold",), (), ()), (("host",), (), ()),
     (("tree-a",), (), ("tree-b",)),
     (("scm-a",), ("well-b",), ())],
)
def test_fault_reachability(nodes, links, expected):
    model = topology()
    assert model.reachable_tree_ids(nodes, links) == expected
    assert model.reachable_tree_ids() == ("tree-a", "tree-b")


def test_dual_scm_paths_preserve_tree_until_both_fail():
    model = topology()
    model = topology(links=model.links + (Link("backup", "scm-b", "tree-a"),))
    assert model.reachable_tree_ids(("scm-a",)) == ("tree-a", "tree-b")
    assert model.reachable_tree_ids(("scm-a", "scm-b")) == ()


def test_links_are_directed_and_disconnected_trees_are_unreachable():
    nodes = (Node("host", "host"), Node("tree", "tree"))
    assert topology(nodes, (Link("reverse", "tree", "host"),)).reachable_tree_ids() == ()
    assert topology(nodes, ()).reachable_tree_ids() == ()


def test_input_order_and_caller_mutations_do_not_change_results():
    model = topology()
    nodes, links = list(reversed(model.nodes)), list(reversed(model.links))
    reordered = topology(nodes, links)
    nodes.clear()
    links.clear()
    assert reordered.reachable_tree_ids() == ("tree-a", "tree-b")
    assert reordered.reachable_tree_ids(disabled_links=("umb-a", "umb-b")) == ()
    with pytest.raises(FrozenInstanceError):
        reordered.host_id = "other"
    with pytest.raises(FrozenInstanceError):
        model.nodes[0].id = "other"
    with pytest.raises(FrozenInstanceError):
        model.links[0].target = "other"


@pytest.mark.parametrize("bad", ["", "  ", None, 1])
@pytest.mark.parametrize("record", ["node", "link", "source", "target"])
def test_invalid_record_ids_raise(bad, record):
    with pytest.raises(ValueError, match="id|source|target"):
        if record == "node":
            Node(bad, "tree")
        elif record == "link":
            Link(bad, "host", "tree")
        elif record == "source":
            Link("link", bad, "tree")
        else:
            Link("link", "host", bad)


def test_unsupported_node_kind_raises():
    with pytest.raises(ValueError, match="kind"):
        Node("node", "valve")


@pytest.mark.parametrize("field, value", [
    ("control_mode", "automatic"), ("architecture", "hydraulic"),
    ("host_id", "missing"), ("host_id", "tree-a"), ("host_id", ""),
])
def test_invalid_topology_labels_and_host_raise(field, value):
    model = topology()
    args = dict(nodes=model.nodes, links=model.links, host_id=model.host_id,
                control_mode=model.control_mode, architecture=model.architecture)
    args[field] = value
    with pytest.raises(ValueError, match=field):
        ControlTopology(**args)


@pytest.mark.parametrize("mode", ["multiplex", "direct-hydraulic"])
@pytest.mark.parametrize("architecture", ["electro-hydraulic", "all-electric"])
def test_labels_describe_intent_without_changing_connectivity(mode, architecture):
    model = topology()
    labelled = ControlTopology(model.nodes, model.links, "host", mode, architecture)
    assert labelled.reachable_tree_ids() == ("tree-a", "tree-b")


def test_duplicate_node_and_link_ids_raise():
    model = topology()
    with pytest.raises(ValueError, match="node.*id"):
        topology(nodes=model.nodes + (model.nodes[0],))
    with pytest.raises(ValueError, match="link.*id"):
        topology(links=model.links + (Link("umb-a", "host", "scm-a"),))


@pytest.mark.parametrize("source, target", [("missing", "tree-a"), ("host", "missing")])
def test_unknown_link_endpoints_raise(source, target):
    with pytest.raises(ValueError, match="endpoint"):
        topology(links=(Link("bad", source, target),))


@pytest.mark.parametrize("source, target", [("host", "host"), ("tree-a", "host")])
def test_self_links_and_cycles_raise(source, target):
    model = topology()
    with pytest.raises(ValueError, match="self|cycle"):
        topology(links=model.links + (Link("bad", source, target),))


def test_cycle_in_disconnected_component_raises():
    model = topology()
    with pytest.raises(ValueError, match="cycle"):
        topology(nodes=model.nodes + (Node("x", "scm"), Node("y", "scm")),
                 links=model.links + (Link("xy", "x", "y"), Link("yx", "y", "x")))


@pytest.mark.parametrize("field", ["disabled_nodes", "disabled_links"])
def test_unknown_disabled_ids_raise(field):
    with pytest.raises(ValueError, match=field):
        topology().reachable_tree_ids(**{field: ("missing",)})


def test_clean_import_has_no_solver_or_third_party_dependency(tmp_path):
    src = Path(__file__).resolve().parents[3] / "src"
    code = (
        "import sys, json; sys.path.insert(0, sys.argv[1]); "
        "from digitalmodel.subsea.control_systems import ControlTopology; "
        "print(json.dumps(sorted(sys.modules)))"
    )
    result = subprocess.run([sys.executable, "-S", "-c", code, str(src)],
                            cwd=tmp_path, text=True, capture_output=True, check=True)
    modules = json.loads(result.stdout)
    assert "digitalmodel.subsea.control_systems.architecture" in modules
    assert not any(name.startswith(("digitalmodel.solvers", "OrcFxAPI", "ansys"))
                   for name in modules)


def test_worked_example_matches_independently_enumerated_sets(capsys):
    example = Path(__file__).resolve().parents[3] / "examples/subsea/control_topology.py"
    runpy.run_path(str(example), run_name="__main__")
    assert capsys.readouterr().out.splitlines() == [
        "healthy: ('tree-a', 'tree-b')",
        "one umbilical disabled: ('tree-a', 'tree-b')",
        "one SCM disabled: ('tree-a', 'tree-b')",
        "both umbilicals disabled: ()",
        "both SCMs disabled: ()",
    ]
