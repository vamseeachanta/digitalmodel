# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Synthetic offline G4 example; run with PYTHONPATH=src python this_file.py.

Hand calculation: M=100 kg, V=99 m^3, rho=1 kg/m^3 gives displaced mass
99 kg and abs(100-99)/100 = 0.01, at the G4.1a boundary. Panel dimension
2 m and supplied shortest wavelength 12 m give 2=12/6. Aspect ratio 3
is at the ordinary G4.4b bound. All inputs are synthetic arithmetic examples.
Criteria source: diffraction knowledge pack r4 section 5, AQG r0 section 8.
No native hydrostatics, orientation, response or removal evidence is supplied,
so the overall answer is NOT_RUN even though these individual checks pass.
"""

import json

from digitalmodel.aqg.diffraction import Artifact, assess, check_mass, check_panels


def main():
    evidence = Artifact("synthetic-hand-calculation", __doc__.encode())
    rows = [check_mass(100, 99, 1, evidence)]
    rows.extend(check_panels([3], [False], 2, 12, evidence))
    print(json.dumps(assess(rows).to_dict(), indent=2, allow_nan=False))


if __name__ == "__main__":
    main()
