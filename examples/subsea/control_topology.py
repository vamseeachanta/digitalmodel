"""Synthetic two-tree connectivity example, without physical qualification.

Run with repository source on the import path. Both SCMs connect to both trees,
so either SCM is sufficient for graph reachability. Parallel umbilical links
are distinct connections. Neither arrangement establishes physical independence
or availability; no reliability rates or engineering units are assumed.
"""

from digitalmodel.subsea.control_systems import ControlTopology, Link, Node


def main() -> None:
    """Print healthy and faulted tree sets for independently listed cases."""
    model = ControlTopology(
        nodes=(Node("host", "host"), Node("manifold", "manifold"),
               Node("scm-a", "scm"), Node("scm-b", "scm"),
               Node("tree-a", "tree"), Node("tree-b", "tree")),
        links=(Link("umb-a", "host", "manifold"),
               Link("umb-b", "host", "manifold"),
               Link("control-a", "manifold", "scm-a"),
               Link("control-b", "manifold", "scm-b"),
               Link("a-a", "scm-a", "tree-a"), Link("a-b", "scm-a", "tree-b"),
               Link("b-a", "scm-b", "tree-a"), Link("b-b", "scm-b", "tree-b")),
        host_id="host", control_mode="multiplex", architecture="electro-hydraulic",
    )
    print("healthy:", model.reachable_tree_ids())
    print("one umbilical disabled:", model.reachable_tree_ids(disabled_links=("umb-a",)))
    print("one SCM disabled:", model.reachable_tree_ids(disabled_nodes=("scm-a",)))
    print("both umbilicals disabled:", model.reachable_tree_ids(disabled_links=("umb-a", "umb-b")))
    print("both SCMs disabled:", model.reachable_tree_ids(disabled_nodes=("scm-a", "scm-b")))


if __name__ == "__main__":
    main()
