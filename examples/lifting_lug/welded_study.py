"""Run the synthetic worked example; no historical geometry is implied."""

import argparse

from digitalmodel.lifting_lug.welded_study import (
    Geometry,
    Load,
    build_study,
    write_outputs,
)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "output_directory", help="new output directory for the complete study"
    )
    args = parser.parse_args()
    # Synthetic inputs: independent hand calculation in the accompanying method
    # document, docs/domains/lifting/welded-padeye.html, Worked example section.
    geometry = Geometry(
        width=4,
        height=6,
        hole_x=0,
        hole_y=3,
        hole_radius=0.5,
        weld_leg=0.25,
        source="idealized rectangle; method document worked example",
    )
    load = Load(
        gross_kip=12,
        factor=1.5,
        lug_count=3,
        source="synthetic arithmetic example; unfactored gross weight",
    )
    data = build_study(geometry, load)
    write_outputs(data, args.output_directory)
    print(
        f"225 cases; revision {data['revision']}; historical reproduction NOT_EVALUATED"
    )


if __name__ == "__main__":
    main()
