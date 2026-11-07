# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Run the synthetic supported surface pull-in in shore_approach_pull_in.md."""

import argparse

from digitalmodel.marine_ops.installation.shore_approach import RouteSegment, pull_in


def synthetic_route(*, net_weight_kn_m: float, friction: float) -> list[RouteSegment]:
    """Build the idealised straight contact pieces in the note's Synthetic inputs.

    Args:
        net_weight_kn_m: Positive net downward line weight in kN/m.
        friction: Caller-supplied equivalent axial friction, dimensionless.

    Returns:
        Segments ordered from the trailing end towards the winch.
    """
    # Source: adjacent note, Synthetic inputs; inclines use a 30:40:50 triangle.
    # No additional buoyancy aids: weight_kn_m is already the net line weight.
    return [
        RouteSegment(20.0, net_weight_kn_m, friction),
        RouteSegment(50.0, net_weight_kn_m, friction, rise_m=30.0),
        RouteSegment(50.0, net_weight_kn_m, friction, rise_m=-30.0),
    ]


def run_example(
    segments: list[RouteSegment],
    *,
    tail_tension_kn: float,
    efficiency: float,
    capacity_kn: float | None = None,
) -> dict:
    """Calculate and print force components, endpoint tensions and winch demand.

    Args:
        segments: Straight supported pieces in tail-to-winch order.
        tail_tension_kn: Prescribed trailing tension in kN.
        efficiency: Caller-supplied mechanical efficiency, dimensionless.
        capacity_kn: Optional declared winch capacity in kN on the demand basis.

    Returns:
        The pull-in result with force values in kN and dimensionless utilisation.

    Raises:
        ValueError: The route or inputs are outside the supported method.
    """
    result = pull_in(
        segments, tail_tension_kn=tail_tension_kn,
        efficiency=efficiency, capacity_kn=capacity_kn,
    )
    print("Quasi-static, continuously supported surface pull-in")
    print("Unevaluated: HDD, bends, breakout and dynamic loads.")
    print(f"Tail tension: {tail_tension_kn:.1f} kN")
    start_tension = tail_tension_kn
    # Source: adjacent note, Hand calculation: F=mu*N and G=w*rise.
    for index, (segment, normal, end_tension) in enumerate(zip(
        segments, result["segment_normal_loads_kn"], result["segment_end_tensions_kn"],
    ), start=1):
        friction = segment.friction * normal
        gravity = (segment.weight_kn_m - segment.buoyancy_kn_m) * segment.rise_m
        print(
            f"Segment {index}: normal={normal:.1f} kN, friction={friction:.1f} kN, "
            f"gravity={gravity:.1f} kN, tension={start_tension:.1f} -> {end_tension:.1f} kN"
        )
        start_tension = end_tension
    print(f"Total friction: {result['friction_kn']:.1f} kN")
    print(f"Total gravity: {result['gravity_kn']:.1f} kN")
    print(f"Pipe head tension: {result['pipe_head_tension_kn']:.1f} kN")
    print(f"Peak pipe tension: {result['peak_pipe_tension_kn']:.1f} kN")
    print(f"Winch efficiency: {efficiency:g} (dimensionless)")
    print(f"Winch demand: {result['winch_demand_kn']:.1f} kN")
    if capacity_kn is None:
        print("Declared winch capacity: not supplied")
        print("Capacity utilisation: not evaluated")
    else:
        print(f"Declared winch capacity: {capacity_kn:.1f} kN")
        # Percent conversion: one whole is 100 percent.
        print(f"Capacity utilisation: {100 * result['capacity_utilisation']:.1f} %")
    print(f"Capacity status: {result['capacity_status']}")
    return result


def main() -> None:
    """Run the offline example with optional caller-supplied loading inputs."""
    parser = argparse.ArgumentParser(description=__doc__)
    # Source for all synthetic defaults: adjacent note, Synthetic inputs.
    parser.add_argument("--friction", type=float, default=0.25, help="Axial friction [1]")
    parser.add_argument("--net-weight-kn-m", type=float, default=2.0, help="Net weight [kN/m]")
    parser.add_argument("--tail-tension-kn", type=float, default=10.0, help="Tail tension [kN]")
    parser.add_argument("--efficiency", type=float, default=0.8, help="Mechanical efficiency [1]")
    parser.add_argument("--capacity-kn", type=float, help="Optional declared winch capacity [kN]")
    args = parser.parse_args()
    try:
        run_example(
            synthetic_route(net_weight_kn_m=args.net_weight_kn_m, friction=args.friction),
            tail_tension_kn=args.tail_tension_kn,
            efficiency=args.efficiency, capacity_kn=args.capacity_kn,
        )
    except ValueError as error:
        parser.error(str(error))


if __name__ == "__main__":
    main()
