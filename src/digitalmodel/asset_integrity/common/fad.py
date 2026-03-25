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
        import logging
        import math

        import pandas as pd
        df = pd.DataFrame(columns = ['L_r', 'K_r'])

        plastic_collapse_load_ratio_limit = self.get_plastic_collapse_load_ratio_limit()
        logging.info("Plastic collapse load ratio limit : {}" .format(plastic_collapse_load_ratio_limit))
        mue = min(0.001*self.material_properties['E']/self.material_grade_properties['SMYS'], 0.6)
        N = 0.3*(1- self.material_grade_properties['SMYS']/self.material_grade_properties['SMUS'])

        n_divisions = 100
        for Lr_index in range(0, n_divisions+1, 1):
            Lr_value = Lr_index * 1/n_divisions
            Fr_value = ((1 + 0.5*Lr_value**2)**(-0.5))*(0.3 + 0.7*math.exp(-mue * Lr_value**6))
            df.loc[len(df)] = [Lr_value, Fr_value]

        f_1 = Fr_value
        n_divisions = 100
        for Lr_index in range(1, n_divisions, 1):
            Lr_value = 1+ Lr_index * (plastic_collapse_load_ratio_limit-1)/n_divisions
            Fr_value = f_1 * (Lr_value)**((N-1)/2/N)
            df.loc[len(df)] = [Lr_value, Fr_value]

        Lr_value = plastic_collapse_load_ratio_limit
        Fr_value = 0
        df.loc[len(df)] = [Lr_value, Fr_value]
        self.FAD['option_1'] = df

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

