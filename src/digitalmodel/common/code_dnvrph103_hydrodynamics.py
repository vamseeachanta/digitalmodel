import math
import numpy as np
from scipy import interpolate
from digitalmodel.common.data import SaveData
from digitalmodel.common.orcaflex_model_utilities import OrcaflexModelUtilities

save_data = SaveData()
omu = OrcaflexModelUtilities()


class DNVRPH103_rectangular_hydrodynamics:

    def __init__(self):
        pass

    def get_orcaflex_6dbuoy(self, cfg):
        properties = self.get_properties(cfg)
        buoy_6d = self.get_6d_buoy(properties)

    def get_properties(self, cfg):
        self.cfg = cfg
        translational_properties = self.get_translational_properties()
        I = self.get_mass_moment_of_inertia()
        rotational_properties = self.get_rotational_properties(
            translational_properties)

        properties = {
            'translational': translational_properties,
            'rotational': rotational_properties,
            'I': I
        }

        return properties

    def get_6d_buoy(self, properties):

        buoy_6d_template = omu.get_6d_buoy_template()
        buoy_6d = self.update_6d_buoy(buoy_6d_template, properties)

    def update_6d_buoy(self, buoy_6d_template, properties):
        buoy_6d = buoy_6d_template.copy()
        structure = self.cfg['inputs']['name']
        buoy_6d['6DBuoys']['New'][0] = structure
        buoy_6d['6DBuoys'][structure] = buoy_6d['6DBuoys'].pop('structure')

        buoy_data = buoy_6d['6DBuoys'][structure]

        buoy_data.update({'Mass': self.cfg['inputs']['m_air'] / 1000})
        buoy_data.update({'MomentsOfInertia': []})
        buoy_data.update({
            'CentreOfMass': [
                self.cfg['inputs']['cog']['x'], self.cfg['inputs']['cog']['y'],
                self.cfg['inputs']['cog']['z']
            ]
        })

        dimensions = self.get_x_dimensions()
        buoy_data.update({'Volume': dimensions['volume']})
        buoy_data.update({'Height': self.cfg['inputs']['h']})
        buoy_data.update({
            'CentreOfVolume': [
                self.cfg['inputs']['cov']['x'], self.cfg['inputs']['cov']['y'],
                self.cfg['inputs']['cov']['z']
            ]
        })

        buoy_data.update({
            'DragArea': [
                round(properties['translational']['area_drag']['x'], 3),
                round(properties['translational']['area_drag']['y'], 3),
                round(properties['translational']['area_drag']['z'], 3)
            ]
        })

        return buoy_6d

    def get_translational_properties(self):

        x_added_mass, x_ca, x_area_drag, x_cd = self.get_translational_x()
        y_added_mass, y_ca, y_area_drag, y_cd = self.get_translational_y()
        z_added_mass, z_ca, z_area_drag, z_cd = self.get_translational_z()

        translational_properties = {
            'added_mass': {
                'x': x_added_mass,
                'y': y_added_mass,
                'z': z_added_mass
            },
            'ca': {
                'x': x_ca,
                'y': y_ca,
                'z': z_ca
            },
            'area_drag': {
                'x': x_area_drag,
                'y': y_area_drag,
                'z': z_area_drag
            },
            'cd': {
                'deep': {
                    'x': x_cd['deep'],
                    'y': y_cd['deep'],
                    'z': z_cd['deep'],
                },
                'splash': {
                    'x': x_cd['splash'],
                    'y': y_cd['splash'],
                    'z': z_cd['splash']
                }
            }
        }

        return translational_properties

    def get_translational_x(self):

        added_mass, ca = self.get_translational_x_ca()
        area_drag, cd = self.get_translational_x_cd()

        return added_mass, ca, area_drag, cd

    def get_translational_y(self):
        added_mass, ca = self.get_translational_y_ca()
        area_drag, cd = self.get_translational_y_cd()

        return added_mass, ca, area_drag, cd

    def get_translational_z(self):
        added_mass, ca = self.get_translational_z_ca()
        area_drag, cd = self.get_translational_z_cd()

        return added_mass, ca, area_drag, cd

    def get_translational_x_ca(self):
        dimensions = self.get_x_dimensions()

        perforation_ratio = self.cfg['inputs']['perforation_ratio']['x']
        added_mass, ca = self.get_translational_ca(dimensions,
                                                   perforation_ratio)

        return added_mass, ca

    def get_translational_y_ca(self):
        dimensions = self.get_y_dimensions()

        perforation_ratio = self.cfg['inputs']['perforation_ratio']['y']
        added_mass, ca = self.get_translational_ca(dimensions,
                                                   perforation_ratio)

        return added_mass, ca

    def get_translational_z_ca(self):
        dimensions = self.get_z_dimensions()

        perforation_ratio = self.cfg['inputs']['perforation_ratio']['z']
        added_mass, ca = self.get_translational_ca(dimensions,
                                                   perforation_ratio)

        return added_mass, ca

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

        return mass_added_with_perforation, ca

    def get_translational_x_cd(self):

        dimensions = self.get_x_dimensions()
        perforation_ratio = self.cfg['inputs']['perforation_ratio']['x']

        return self.get_translational_cd(dimensions, perforation_ratio)

    def get_translational_y_cd(self):

        dimensions = self.get_y_dimensions()
        perforation_ratio = self.cfg['inputs']['perforation_ratio']['y']

        return self.get_translational_cd(dimensions, perforation_ratio)

    def get_translational_z_cd(self):

        dimensions = self.get_z_dimensions()
        perforation_ratio = self.cfg['inputs']['perforation_ratio']['z']

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

        return drag_area, cd

    def get_x_dimensions(self):
        a = min(self.cfg['inputs']['h'], self.cfg['inputs']['w'])
        b = max(self.cfg['inputs']['h'], self.cfg['inputs']['w'])
        c = self.cfg['inputs']['l']
        volume = (self.cfg['inputs']['m_air'] -
                  self.cfg['inputs']['m_water']) / self.cfg['rho_water']

        dimensions = {'a': a, 'b': b, 'c': c, 'volume': volume}

        return dimensions

    def get_y_dimensions(self):
        a = min(self.cfg['inputs']['h'], self.cfg['inputs']['l'])
        b = max(self.cfg['inputs']['h'], self.cfg['inputs']['l'])
        c = self.cfg['inputs']['w']
        volume = (self.cfg['inputs']['m_air'] -
                  self.cfg['inputs']['m_water']) / self.cfg['rho_water']

        dimensions = {'a': a, 'b': b, 'c': c, 'volume': volume}

        return dimensions

    def get_z_dimensions(self):
        a = min(self.cfg['inputs']['w'], self.cfg['inputs']['l'])
        b = max(self.cfg['inputs']['w'], self.cfg['inputs']['l'])
        c = self.cfg['inputs']['h']
        volume = (self.cfg['inputs']['m_air'] -
                  self.cfg['inputs']['m_water']) / self.cfg['rho_water']

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
        inputs = self.cfg['inputs']
        m_air = inputs['m_air']
        l = inputs['l']
        w = inputs['w']
        h = inputs['h']

        term_1 = 1 / 12 * m_air * (l**2 + w**2)
        term_2 = m_air * (inputs['cog']['y']**2 + inputs['cog']['z']**2)
        Ix = term_1 + term_2

        term_1 = 1 / 12 * m_air * (l**2 + h**2)
        term_2 = m_air * (inputs['cog']['x']**2 + inputs['cog']['z']**2)
        Iy = term_1 + term_2

        term_1 = 1 / 12 * m_air * (l**2 + w**2)
        term_2 = m_air * (inputs['cog']['y']**2 + inputs['cog']['x']**2)
        Iz = term_1 + term_2

        I = {'Ix': Ix, 'Iy': Iy, 'Iz': Iz}

        return I

    def get_rotational_properties(self, translational_properties):
        am = self.get_area_moment_of_intertia(translational_properties)
        cd = self.get_moment_coefficient()
        I = self.get_hydrodynamic_intertia()
        ca = self.get_added_inertia_coefficient()

        rotational_properties = {'am': am, 'cd': cd, 'I': I, 'ca': ca}

        return rotational_properties

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
        inputs = self.cfg['inputs']
        l = inputs['l']
        w = inputs['w']
        h = inputs['h']
        rho_water = self.cfg['rho_water']
        volume = (inputs['m_air'] - inputs['m_water']) / rho_water

        I_x = volume * rho_water * (w**2 * h**2) / 12
        I_y = volume * rho_water * (h**2 * l**2) / 12
        I_z = volume * rho_water * (l**2 * w**2) / 12

        I = {'x': I_x, 'y': I_y, 'z': I_z}

        return I

    def get_area_moment_of_inertia_by_type(self, translational_properties,
                                           type):

        inputs = self.cfg['inputs']
        l = inputs['l']
        w = inputs['w']
        h = inputs['h']
        am_x = (translational_properties['cd']['splash']['y'] * l * h**4 +
                translational_properties['cd']['splash']['z'] * l * w**4) / 12

        am_y = (translational_properties['cd']['splash']['x'] * w * h**4 +
                translational_properties['cd']['splash']['z'] * w * l**4) / 12

        am_z = (translational_properties['cd']['splash']['x'] * h * w**4 +
                translational_properties['cd']['splash']['y'] * h * l**4) / 12

        am = {'x': am_x, 'y': am_y, 'z': am_z}

        return am

    def get_added_inertia_coefficient(self):
        inputs = self.cfg['inputs']
        l = inputs['l']
        w = inputs['w']
        h = inputs['h']

        ca_x = self.get_ca_intertial_x(l, w, h)
        ca_y = self.get_ca_intertial_y(l, w, h)
        ca_z = self.get_ca_intertial_z(l, w, h)

        ca = {'x': ca_x, 'y': ca_y, 'z': ca_z}

        return ca

    def get_ca_intertial_x(self, l, w, h):
        a = l / 2
        b = math.sqrt(w * h / math.pi)
        ratio_1 = b / a
        ratio_2 = a / b

        ca_ = self.get_newman_ca(a, b, ratio_1, ratio_2)

    def get_ca_intertial_y(self, l, w, h):
        a = w / 2
        b = math.sqrt(h * l / math.pi)
        ratio_1 = b / a
        ratio_2 = a / b

        ca_ = self.get_newman_ca(a, b, ratio_1, ratio_2)

    def get_ca_intertial_z(self, l, w, h):
        a = h / 2
        b = math.sqrt(l * w / math.pi)
        ratio_1 = b / a
        ratio_2 = a / b

        ca_ = self.get_newman_ca(a, b, ratio_1, ratio_2)

    def get_newman_ca(self, a, b, ratio_1, ratio_2):
        if ratio_1 < 1.6:
            ratio = self.cfg['look_up']['rotational']['ratio']
            value = self.cfg['look_up']['rotational']['m55_upper']
            p = np.polyfit(ratio, value, deg=6)
            ca = np.polyval(p, ratio_1)
        else:
            ratio = self.cfg['look_up']['rotational']['ratio']
            value = self.cfg['look_up']['rotational']['m55_lower']
            p = np.polyfit(ratio, value, deg=6)
            ca = np.polyval(p, ratio_2) * 2 * b**3 / (a * (a**2 + b**2))

        return ca
