"""Failure Assessment Diagram (FAD) construction and evaluation.

Per BS 7910:2013 (Guide to methods for assessing the acceptability of flaws
in metallic structures) — builds the legacy Lr/Kr table of the Option 1
failure-assessment curve.  Since #2160 every ordinate comes from the single
canonical implementation,
:func:`digitalmodel.asset_integrity.assessment.crack_fad.fad_curve_option1`;
this module only owns the legacy grid and DataFrame shape.
"""

class FAD():
    def __init__(self, cfg):
        self.cfg = cfg
        self.init_assign_key_properties()
        self.FAD = {}

    def get_BS7910_2013_FAD(self):
        self.BS7910_2013_option_1()
        self.BS7910_2013_option_2()
        self.BS7910_2013_option_3()

        return self.FAD

    def BS7910_2013_option_1(self):
        """Lr/Kr table of the BS 7910:2013 Option 1 curve (legacy shape).

        Every ordinate is the canonical
        :func:`digitalmodel.asset_integrity.assessment.crack_fad.fad_curve_option1`
        (#2160).  This method owns only the legacy grid — 101 points on
        0 <= Lr <= 1, 99 points on 1 < Lr < Lr_max — and the plot-convention
        closing row ``(Lr_max, 0)``.  E and SMYS may be in any consistent
        unit; only their ratio enters the curve.
        """
        import logging

        import pandas as pd

        from digitalmodel.asset_integrity.assessment.crack_fad import (
            fad_curve_option1,
        )

        smys = self.material_grade_properties['SMYS']
        smus = self.material_grade_properties['SMUS']
        E = self.material_properties['E']
        plastic_collapse_load_ratio_limit = self.get_plastic_collapse_load_ratio_limit()
        logging.info("Plastic collapse load ratio limit : {}" .format(plastic_collapse_load_ratio_limit))

        n_divisions = 100
        Lr_values = [Lr_index * 1/n_divisions for Lr_index in range(0, n_divisions+1, 1)]
        Lr_values += [1 + Lr_index * (plastic_collapse_load_ratio_limit-1)/n_divisions
                      for Lr_index in range(1, n_divisions, 1)]
        rows = [[Lr_value, fad_curve_option1(Lr_value, smys, smus, E)]
                for Lr_value in Lr_values]
        rows.append([plastic_collapse_load_ratio_limit, 0.0])
        self.FAD['option_1'] = pd.DataFrame(rows, columns=['L_r', 'K_r'])

    def BS7910_2013_option_2(self):
        self.FAD.update({'option_2': None})

    def BS7910_2013_option_3(self):
        self.FAD.update({'option_3': None})

    def get_plastic_collapse_load_ratio_limit(self):
        # BS7910, Clause 7.3.2
        Lr_max = (self.material_grade_properties['SMYS'] + self.material_grade_properties['SMUS'])/(2*self.material_grade_properties['SMYS'])
        plastic_collapse_load_ratio_limit = Lr_max

        return plastic_collapse_load_ratio_limit

    def init_assign_key_properties(self):
        self.material_grade = self.cfg['Outer_Pipe']['Material']['Material_Grade']
        self.material = self.cfg['Outer_Pipe']['Material']['Material']
        self.material_properties = self.cfg['Material'][self.material]
        self.material_grade_properties = self.cfg['Material'][self.material]['Grades'][self.material_grade]
        self.pipe_properties = None

