import math
from scipy import interpolate
from digitalmodel.common.data import SaveData

save_data = SaveData()


class DNVRPH103_hydrodynamics():

    def __init__(self):
        pass

    def get_orcaflex_6dbuoy(self):
        pass

    def get_properties(self, cfg):
        self.cfg = cfg
        self.get_translational_properties()
        self.get_mass_moment_of_inertia()
        self.get_rotational_properties()

    def get_translational_properties(self):
        if self.cfg['properties']['shape'] == 'rectangular':
            self.get_translational_rectangular()
        elif self.cfg['properties']['shape'] == 'circular':
            dimensions = self.get_translational_circular()

    def get_translational_rectangular(self):
        a = min(self.cfg['properties']['h'], self.cfg['properties']['w'])
        b = max(self.cfg['properties']['h'], self.cfg['properties']['w'])
        c = self.cfg['properties']['l']
        volume = (self.cfg['properties']['m_air'] -
                  self.cfg['properties']['m_water']) / self.cfg['rho_water']

        dimensions = {'a': a, 'b': b, 'c': c, 'volume': volume}

        self.get_translational_rectangular_added_mass(dimensions)

        return dimensions

    def get_translational_rectangular_added_mass(self, dimensions):
        b_over_a = dimensions['b'] / dimensions['a']
        ratio = self.cfg['look_up']['plate']['added_mass']['ratio']
        value = self.cfg['look_up']['plate']['added_mass']['value']

        f = interpolate.interp1d(ratio, value)
        c = f(b_over_a)
        lambda_0 = math.sqrt(dimensions['a'] * dimensions['b']) / (
            dimensions['c'] + math.sqrt(dimensions['a'] * dimensions['b']))

        mass_added_term_1 = c * math.pi / 4 * self.cfg[
            'rho_water'] * dimensions['a']**2 * dimensions['b']
        mass_added_term_2 = 1 + math.sqrt(
            (1 - lambda_0**2) / (2 * (1 + lambda_0**2)))
        mass_added = mass_added_term_1 * mass_added_term_2
        perforation_ratio = self.cfg['properties']['perforation_ratio']['x']
        mass_added_with_perforation = self.get_mass_added_with_perforation(
            self, mass_added, perforation_ratio)

    def get_translational_circular(self):

        a = b = 2 * self.cfg['properties']['r']
        c = self.cfg['l']

        dimensions = {'a': a, 'b': b, 'c': c}

        return {'a': a, 'b': b, 'c': c}

    def get_mass_added_with_perforation(self, mass_added, perforation_ratio):
        if perforation_ratio < 0.05:
            reduction_factor = 1
        elif perforation_ratio < 0.34:
            reduction_factor = 0.7 + 0.3 * math.cos(
                math.pi * (perforation_ratio - 0.05) / 0.34)
        else:
            reduction_factor = math.exp((0.1 - perforation_ratio) / 0.28)

        mass_added_with_perforation = mass_added * reduction_factor

        return mass_added_with_perforation

    def get_mass_moment_of_inertia(self):
        pass

    def get_rotational_properties(self):
        pass

    def get_orcaflex_6dbuoy(self):
        pass
