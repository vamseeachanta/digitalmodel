"""Geometry evidence for pmxp2; no native solution or acceptance is implied."""
from pathlib import Path
import unittest

import yaml


CAMPAIGN = (Path(__file__).resolve().parents[2]
            / 'docs/domains/orcaflex/library/campaigns/generic-drilling-riser')


class LayAzimuthGeometryTests(unittest.TestCase):
    def test_reported_cases_have_vertical_reset_ends_and_seabed_clearance(self):
        for case in ('o0-c1', 'o0-c2', 'o0-c3'):
            with self.subTest(case=case):
                folder = CAMPAIGN / 'models' / case / 'includes'
                objects = yaml.safe_load((folder / '20_generic_objects.yml').read_text())
                env = yaml.safe_load((folder / '03_environment.yml').read_text())['Environment']
                self.assertEqual(env['SeabedType'], 'Flat')
                self.assertEqual(env['SeabedSlope'], 0)
                seabed_z = env['WaterSurfaceZ'] - env['SeabedOriginDepth']
                self.assertEqual(seabed_z, -1020)
                buoys = {b['Name']: b for b in objects['6DBuoys']}
                self.assertEqual(objects['Vessels'][0]['InitialPosition'], [0, 0, 0])
                for name, expected_ends, expected_length, clearance in (
                    ('Drill String', [[0, 0, 34], [0, 0, -1000]], 1035, 19),
                    ('Flex Joint Rigid Piece', [[0, 0, -1000], [0, 0, -1000.25]],
                     0.25, 19.5),
                ):
                    line = next(x for x in objects['Lines'] if x['Name'] == name)
                    self.assertEqual(line['TopEnd'], 'End A')
                    self.assertEqual(line['Representation'], 'Finite element')
                    self.assertEqual(line['StaticsStep1'], 'Catenary')
                    self.assertEqual(line['StaticsStep2'], 'Full statics')
                    self.assertEqual(line['StaticsSeabedFrictionPolicy'], 'As laid')
                    self.assertEqual(line['LayAzimuth'], 180)
                    rows = next(v for k, v in line.items() if k.startswith('Connection, '))
                    ends = []
                    for row in rows:
                        xyz = row[1:4]
                        if row[0] not in ('Free', 'Fixed'):
                            buoy = buoys[row[0]]
                            self.assertEqual(buoy['Connection'], 'Free')
                            self.assertEqual(buoy['InitialAttitude'], [0, 0, 0])
                            xyz = [a + b for a, b in zip(xyz, buoy['InitialPosition'])]
                        ends.append(xyz)
                    self.assertEqual(ends, expected_ends)
                    self.assertEqual(ends[0][:2], ends[1][:2])
                    length = (sum(s['Length'] for s in line['Sections'])
                              if 'Sections' in line else
                              sum(s[1] for s in line['LineType, Length, TargetSegmentLength']))
                    self.assertEqual(length, expected_length)
                    # Unstretched screens only: free A for the drill string,
                    # fixed B for the rigid piece. No claim about native motion.
                    reference_z = ends[0][2] if name == 'Drill String' else ends[1][2]
                    self.assertEqual(reference_z - length - seabed_z, clearance)
                    self.assertGreater(min(p[2] for p in ends), seabed_z)


if __name__ == '__main__':
    unittest.main()
