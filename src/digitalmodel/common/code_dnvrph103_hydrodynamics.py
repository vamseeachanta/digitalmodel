import math
from scipy import interpolate
from digitalmodel.common.data import SaveData

save_data = SaveData()


class DNVRPH103_rectangular_hydrodynamics():

    def __init__(self):
        pass

    def get_orcaflex_6dbuoy(self):
        pass

    def get_properties(self, cfg):
        self.cfg = cfg
        translational_properties = self.get_translational_properties()
        I = self.get_mass_moment_of_inertia()
        AM = self.get_rotational_properties(translational_properties)

    def get_translational_properties(self):

        x_ca, x_cd = self.get_translational_x()
        y_ca, y_cd = self.get_translational_y()
        z_ca, z_cd = self.get_translational_z()

        translational_properties = {
            'x': {
                'ca': x_ca,
                'cd': x_cd
            },
            'y': {
                'ca': y_ca,
                'cd': y_cd
            },
            'z': {
                'ca': z_ca,
                'cd': z_cd
            }
        }

        return translational_properties

    def get_translational_x(self):

        ca = self.get_translational_x_ca()
        cd = self.get_translational_x_cd()

        return ca, cd

    def get_translational_y(self):
        ca = self.get_translational_y_ca()
        cd = self.get_translational_y_cd()

        return ca, cd

    def get_translational_z(self):
        ca = self.get_translational_z_ca()
        cd = self.get_translational_z_cd()

        return ca, cd

    def get_translational_x_ca(self):
        dimensions = self.get_x_dimensions()

        perforation_ratio = self.cfg['properties']['perforation_ratio']['x']
        ca = self.get_translational_ca(dimensions, perforation_ratio)

        return ca

    def get_translational_y_ca(self):
        dimensions = self.get_y_dimensions()

        perforation_ratio = self.cfg['properties']['perforation_ratio']['y']
        ca = self.get_translational_ca(dimensions, perforation_ratio)

        return ca

    def get_translational_z_ca(self):
        dimensions = self.get_z_dimensions()

        perforation_ratio = self.cfg['properties']['perforation_ratio']['z']
        ca = self.get_translational_ca(dimensions, perforation_ratio)

        return ca

    def get_translational_ca(self, dimensions, perforation_ratio):
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

        mass_added_with_perforation = self.get_mass_added_with_perforation(
            mass_added, perforation_ratio)
        ca = mass_added_with_perforation / dimensions['volume'] / self.cfg[
            'rho_water']

        return ca

    def get_translational_x_cd(self):

        dimensions = self.get_x_dimensions()
        perforation_ratio = self.cfg['properties']['perforation_ratio']['x']

        return self.get_translational_cd(dimensions, perforation_ratio)

    def get_translational_y_cd(self):

        dimensions = self.get_y_dimensions()
        perforation_ratio = self.cfg['properties']['perforation_ratio']['y']

        return self.get_translational_cd(dimensions, perforation_ratio)

    def get_translational_z_cd(self):

        dimensions = self.get_z_dimensions()
        perforation_ratio = self.cfg['properties']['perforation_ratio']['z']

        return self.get_translational_cd(dimensions, perforation_ratio)

    def get_translational_cd(self, dimensions, perforation_ratio):
        drag_area = dimensions['a'] * dimensions['b'] * (1 - perforation_ratio)
        aspect_ratio = dimensions['c'] / math.sqrt(
            (dimensions['a'] * dimensions['b']))

        if aspect_ratio < 1:
            b_over_a = dimensions['b'] / dimensions['a']
            ratio = self.cfg['look_up']['plate']['drag']['ratio']
            value = self.cfg['look_up']['plate']['drag']['value']

            f = interpolate.interp1d(ratio, value)
            deep = f(b_over_a)
        else:
            ratio = self.cfg['look_up']['prism']['drag']['ratio']
            value = self.cfg['look_up']['prism']['drag']['value']

            f = interpolate.interp1d(ratio, value)
            deep = f(aspect_ratio)

        splash = self.get_splash_from_deep_drag(deep)

        cd = {'deep': deep, 'splash': splash}

        return cd

    def get_x_dimensions(self):
        a = min(self.cfg['properties']['h'], self.cfg['properties']['w'])
        b = max(self.cfg['properties']['h'], self.cfg['properties']['w'])
        c = self.cfg['properties']['l']
        volume = (self.cfg['properties']['m_air'] -
                  self.cfg['properties']['m_water']) / self.cfg['rho_water']

        dimensions = {'a': a, 'b': b, 'c': c, 'volume': volume}

        return dimensions

    def get_y_dimensions(self):
        a = min(self.cfg['properties']['h'], self.cfg['properties']['l'])
        b = max(self.cfg['properties']['h'], self.cfg['properties']['l'])
        c = self.cfg['properties']['w']
        volume = (self.cfg['properties']['m_air'] -
                  self.cfg['properties']['m_water']) / self.cfg['rho_water']

        dimensions = {'a': a, 'b': b, 'c': c, 'volume': volume}

        return dimensions

    def get_z_dimensions(self):
        a = min(self.cfg['properties']['w'], self.cfg['properties']['l'])
        b = max(self.cfg['properties']['w'], self.cfg['properties']['l'])
        c = self.cfg['properties']['h']
        volume = (self.cfg['properties']['m_air'] -
                  self.cfg['properties']['m_water']) / self.cfg['rho_water']

        dimensions = {'a': a, 'b': b, 'c': c, 'volume': volume}

        return dimensions

    def get_splash_from_deep_drag(self, deep):
        splash = 3 * deep
        if splash < 2.5:
            splash = 2.5

        return splash

    def get_translational_circular(self):

        a = b = 2 * self.cfg['properties']['r']
        c = self.cfg['l']

        dimensions = {'a': a, 'b': b, 'c': c}

        return {'a': a, 'b': b, 'c': c}

    def get_mass_added_with_perforation(self, mass_added, perforation_ratio):
        if perforation_ratio <= 0.05:
            reduction_factor = 1
        elif perforation_ratio < 0.34:
            reduction_factor = 0.7 + 0.3 * math.cos(
                math.pi * (perforation_ratio - 0.05) / 0.34)
        else:
            reduction_factor = math.exp((0.1 - perforation_ratio) / 0.28)

        mass_added_with_perforation = mass_added * reduction_factor

        return mass_added_with_perforation

    def get_mass_moment_of_inertia(self):
        props = self.cfg['properties']
        m_air = props['m_air']
        l = props['l']
        w = props['w']
        h = props['h']

        term_1 = 1 / 12 * m_air * (l**2 + w**2)
        term_2 = m_air * (props['cog']['y']**2 + props['cog']['z']**2)
        Ix = term_1 + term_2

        term_1 = 1 / 12 * m_air * (l**2 + h**2)
        term_2 = m_air * (props['cog']['x']**2 + props['cog']['z']**2)
        Iy = term_1 + term_2

        term_1 = 1 / 12 * m_air * (l**2 + w**2)
        term_2 = m_air * (props['cog']['y']**2 + props['cog']['x']**2)
        Iz = term_1 + term_2

        I = {'Ix': Ix, 'Iy': Iy, 'Iz': Iz}

        return I

    def get_rotational_properties(self, translational_properties):
        am = self.get_area_moment_of_intertia(translational_properties)
        cd = self.get_moment_coefficient()
        I = self.get_hydrodynamic_intertia()
        ca = self.get_added_inertia_coefficient()

    def get_area_moment_of_intertia(self, translational_properties):
        am_splash = self.get_area_moment_of_inertia_by_type(
            translational_properties, type='splash')
        am_deep = self.get_area_moment_of_inertia_by_type(
            translational_properties, type='deep')

        am = {'splash': am_splash, 'deep': am_deep}

    def get_moment_coefficient(self):
        cd = {'x': 1.0, 'y': 1.0, 'z': 1.0}

        return cd

    def get_hydrodynamic_intertia(self):
        props = self.cfg['properties']
        l = props['l']
        w = props['w']
        h = props['h']
        rho_water = self.cfg['rho_water']
        volume = (props['m_air'] - props['m_water']) / rho_water

        I_x = volume * rho_water * (w**2 * h**2) / 12
        I_y = volume * rho_water * (h**2 * l**2) / 12
        I_z = volume * rho_water * (l**2 * w**2) / 12

        I = {'x': I_x, 'y': I_y, 'z': I_z}

        return I

    def get_area_moment_of_inertia_by_type(self, translational_properties,
                                           type):

        props = self.cfg['properties']
        l = props['l']
        w = props['w']
        h = props['h']
        am_x = (translational_properties['y']['cd']['splash'] * l * h**4 +
                translational_properties['z']['cd']['splash'] * l * w**4) / 12

        am_y = (translational_properties['x']['cd']['splash'] * w * h**4 +
                translational_properties['z']['cd']['splash'] * w * l**4) / 12

        am_z = (translational_properties['x']['cd']['splash'] * h * w**4 +
                translational_properties['y']['cd']['splash'] * h * l**4) / 12

        am = {'x': am_x, 'y': am_y, 'z': am_z}

        return am

    def get_added_inertia_coefficient(self):
        ca_x = self.get_added_inertia_coefficient_x()
        ca_y = self.get_added_inertia_coefficient_y()
        ca_z = self.get_added_inertia_coefficient_z()

    def get_added_inertia_coefficient_x(self):
        props = self.cfg['properties']
        l = props['l']
        w = props['w']
        h = props['h']
        a_x = l / 2
        b_x = math.sqrt(w * h / math.pi)
        ratio_1 = b_x / a_x
        ratio_2 = a_x / b_x


        if ratio_1 < 1.6
            ratio = self.cfg['look_up']['rotational']['ratio']
            value = self.cfg['look_up']['rotational']['m55_upper']

            f = interpolate.interp1d(ratio, value, kind='cubic')
            c = f(b_over_a)
            ca_x = 0.2 + 0.8 * ratio_1
        else:
            ca_x = 1.0

    def get_orcaflex_6dbuoy(self):
        pass
