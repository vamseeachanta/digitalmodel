# Standard library imports
import logging
import os

# Third party imports
import pandas as pd  # noqa : F401
from assetutilities.common.data import ReadData, SaveData
from assetutilities.common.file_management import FileManagement
from assetutilities.common.utilities import is_file_valid_func
from assetutilities.common.yml_utilities import WorkingWithYAML

# Reader imports
from digitalmodel.custom.aqwa.aqwa_utilities import AqwaUtilities

wwy = WorkingWithYAML()


fm = FileManagement()
au = AqwaUtilities()
rd = ReadData()
save_data = SaveData()
white_space = ' '

class AqwaDATFiles:

    def __init__(self):
        pass

    def router(self, cfg):
        self.process_all_data_categories(cfg)

    def process_all_data_categories(self, cfg):
        for dc_cfg in cfg['inputs']:
            data_category = str(dc_cfg['data']['category'])
            data_category = data_category.zfill(2)
            dc_data = self.get_dc_data(dc_cfg, data_category)
            self.write_dc_data(dc_data, data_category, dc_cfg, cfg)


    def get_dc_data(self, dc_cfg, data_category):

        dc_header = self.get_dc_header(data_category)

        dc_body_func = getattr(self, f"get_dc_{data_category}_body")
        dc_body = dc_body_func(dc_cfg)

        dc_footer = self.get_dc_footer(data_category)
        dc_data = dc_header + dc_body + dc_footer

        return dc_data

    def write_dc_data(self, dc_data, data_category, dc_cfg, cfg):
        output_directory = cfg['Analysis']['file_management_output_directory']
        file_prefix = dc_cfg.get('file_prefix', None)
        if file_prefix is None:
            file_prefix = ''
        file_name = file_prefix + data_category + '.' + dc_cfg['file_extension']
        io = os.path.join(output_directory, file_name)
        save_data_cfg = {
            'io': io,
            'data': dc_data
        }
        save_data.save_ascii_file_from_array(dc_data, io)

    def get_dc_header(self, data_category):
        file_name = f"{data_category}_header.deck"

        library_name = 'digitalmodel'
        library_file_cfg = {
            'filename': f"tests/test_data/aqwa/decks/{file_name}",
            'library_name': library_name
        }

        file_name = wwy.get_library_filename(library_file_cfg)
        rd_cfg = {'io': file_name}
        file_array = rd.from_ascii_file_get_lines_as_string_arrays(rd_cfg)

        return file_array


    def get_dc_footer(self, data_category):
        file_name = f"{data_category}_footer.deck"

        library_name = 'digitalmodel'
        library_file_cfg = {
            'filename': f"tests/test_data/aqwa/decks/{file_name}",
            'library_name': library_name
        }

        file_name = wwy.get_library_filename(library_file_cfg)
        rd_cfg = {'io': file_name}
        file_array = rd.from_ascii_file_get_lines_as_string_arrays(rd_cfg)

        return file_array

    def get_dc_03_body(self, dc_cfg):
        raw_data = dc_cfg['data']['raw']
        body = []
        for body_item_idx in range(0, len(raw_data)):
            body_item = raw_data[body_item_idx]
            node = body_item['node']
            mass = int(body_item['mass'])
            body_item_str = f"{white_space:>1s}{white_space:>3s}{body_item_idx+1:>2d}{white_space:>4s}{white_space:>5s}{node:>5d}{mass:>10d}"
            body.append(body_item_str)

        return body

    def get_dc_04_body(self, dc_cfg):
        raw_data = dc_cfg['data']['raw']
        element_type = dc_cfg['data']['element_type']
        body = []
        for body_item_idx in range(0, len(raw_data)):
            body_item = raw_data[body_item_idx]
            node = body_item['node']
            Ixx = body_item.get('Ixx', 0)
            Ixx = f"{Ixx:.3e}"
            Ixy = body_item.get('Ixy', 0)
            Ixy = f"{Ixy:.3e}"
            Ixz = body_item.get('Ixz', 0)
            Ixz = f"{Ixz:.3e}"
            Iyy = body_item.get('Iyy', 0)
            Iyy = f"{Iyy:.3e}"
            Iyz = body_item.get('Iyz', 0)
            Iyz = f"{Iyz:.3e}"
            Izz = body_item.get('Izz', 0)
            Izz = f"{Izz:.3e}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{body_item_idx+1:>2d}{element_type:>4s}{white_space:>5s}{node:>5d}{Ixx:>10s}{Ixy:>10s}{Ixz:>10s}{Iyy:>10s}{Iyz:>10s}{Izz:>10s}"
            body.append(body_item_str)

        return body


    def get_dc_06_body(self, dc_cfg):
        raw_data = dc_cfg['data']['raw']
        element_type = dc_cfg['data']['element_type']
        body = []
        period_array = raw_data['period']
        period_array.sort(reverse=True)
        for body_item_idx in range(0, len(period_array)):
            period = raw_data['period'][body_item_idx]
            frequency = 1/period
            frequency = f"{frequency:.3e}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{body_item_idx+1:>2d}{element_type:>4s}{body_item_idx+1:>5d}{body_item_idx+1:>5d}{frequency:>10s}"
            body.append(body_item_str)

        return body


    def get_dc_07_body(self, dc_cfg):
        raw_data = dc_cfg['data']['raw']
        element_type = dc_cfg['data']['element_type']
        body = []
        for body_item_idx in range(0, 6):
            if body_item_idx in [0, 1, 2, 5]:
                data_array = [0]*6
            elif body_item_idx in [3]:
                data_array = [0]*3 + [raw_data['added_damping']['rxx']] + [0]*2
            elif body_item_idx in [4]:
                data_array = [0]*4 + [raw_data['added_damping']['ryy']] + [0]

            data_array_str = [f"{data:.3e}" for data in data_array]
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{element_type:>4s}{white_space:>5s}{body_item_idx+1:>5d}{data_array_str[0]:>10s}{data_array_str[1]:>10s}{data_array_str[2]:>10s}{data_array_str[3]:>10s}{data_array_str[4]:>10s}{data_array_str[5]:>10s}"
            body.append(body_item_str)

        return body


    def get_dc_10_body(self, dc_cfg):
        raw_data = dc_cfg['data']['raw']
        body = []

        directions = []
        for item in raw_data:
            element_type = item['element_type']
            csv_file = item['csv']
            is_file_valid, file_name = is_file_valid_func(csv_file)
            if not is_file_valid:
                logging.error(f"File {file_name} is not valid")
                raise ValueError(f"File {file_name} is not valid")
            
            df_item = pd.read_csv(file_name)
            directions += list(df_item['Direction'])
        
        directions = list(set(directions))
        direction_identifier_df = pd.DataFrame(columns=['Direction'], data=directions)
        direction_identifier_df.sort_values(by=['Direction'], inplace=True)
        direction_identifier_df['direction_identifier'] = [str(item) for item in list(range(1,len(directions)+1))]

        self.get_direction_data(body, direction_identifier_df)
        
        for item in raw_data:
            element_type = item['element_type']
            csv_file = item['csv']
            is_file_valid, file_name = is_file_valid_func(csv_file)
            if not is_file_valid:
                logging.error(f"File {file_name} is not valid")
                raise ValueError(f"File {file_name} is not valid")

            df_item = pd.read_csv(file_name)
            res_df = pd.merge(df_item, direction_identifier_df, how='inner', on=['Direction'])
            scaling = item['scaling']

            if element_type == 'CFC':
                body = self.get_CFC_data(body, res_df, scaling)

            elif element_type == 'WFC':
                body = self.get_WFC_data(body, res_df, scaling)


        return body

    def get_direction_data(self, body, direction_identifier_df):

        direction_index = 0
        for idx in range(0, len(direction_identifier_df)):
            direction_index = direction_index + 1
            direction = f"{direction_identifier_df.iloc[idx]['Direction']:.1f}"
            direction_identifier = direction_identifier_df.iloc[idx]['direction_identifier']
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'DIRN':>4s}{direction_identifier:>5s}{direction_identifier:>5s}{direction:>10s}"
            body.append(body_item_str)


    def get_WFC_data(self, body, df_item, scaling):
        for idx in range(0, len(df_item)):

            direction_identifier = df_item.iloc[idx]['direction_identifier']

            Cx = f"{df_item.iloc[idx]['Cx'] * scaling['Cx']:.3e}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'WIFX':>4s}{direction_identifier:>5s}{direction_identifier:>5s}{Cx:>10s}"
            body.append(body_item_str)

            Cy = f"{df_item.iloc[idx]['Cy'] * scaling['Cy']:.3e}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'WIFY':>4s}{direction_identifier:>5s}{direction_identifier:>5s}{Cy:>10s}"
            body.append(body_item_str)

            Cz = f"{df_item.iloc[idx]['Cz'] * scaling['Cz']:.3e}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'WIFZ':>4s}{direction_identifier:>5s}{direction_identifier:>5s}{Cz:>10s}"
            body.append(body_item_str)
                    
            Cnx = f"{df_item.iloc[idx]['Cnx'] * scaling['Cnx']:.3e}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'WIRX':>4s}{direction_identifier:>5s}{direction_identifier:>5s}{Cnx:>10s}"
            body.append(body_item_str)
                    
            Cny = f"{df_item.iloc[idx]['Cny'] * scaling['Cny']:.3e}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'WIRY':>4s}{direction_identifier:>5s}{direction_identifier:>5s}{Cny:>10s}"
            body.append(body_item_str)
                    
            Cnz = f"{df_item.iloc[idx]['Cnz'] * scaling['Cnz']:.3e}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'WIRZ':>4s}{direction_identifier:>5s}{direction_identifier:>5s}{Cnz:>10s}"
            body.append(body_item_str)
            
        return body

    def get_CFC_data(self, body, df_item, scaling):
        for idx in range(0, len(df_item)):

            direction_identifier = df_item.iloc[idx]['direction_identifier']

            Cx = f"{df_item.iloc[idx]['Cx'] * scaling['Cx']:.3e}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'CUFX':>4s}{direction_identifier:>5s}{direction_identifier:>5s}{Cx:>10s}"
            body.append(body_item_str)

            Cy = f"{df_item.iloc[idx]['Cy'] * scaling['Cy']:.3e}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'CUFY':>4s}{direction_identifier:>5s}{direction_identifier:>5s}{Cy:>10s}"
            body.append(body_item_str)

            Cz = f"{df_item.iloc[idx]['Cz'] * scaling['Cz']:.3e}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'CUFZ':>4s}{direction_identifier:>5s}{direction_identifier:>5s}{Cz:>10s}"
            body.append(body_item_str)
                    
            Cnx = f"{df_item.iloc[idx]['Cnx'] * scaling['Cnx']:.3e}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'CURX':>4s}{direction_identifier:>5s}{direction_identifier:>5s}{Cnx:>10s}"
            body.append(body_item_str)
                    
            Cny = f"{df_item.iloc[idx]['Cny'] * scaling['Cny']:.3e}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'CURY':>4s}{direction_identifier:>5s}{direction_identifier:>5s}{Cny:>10s}"
            body.append(body_item_str)
                    
            Cnz = f"{df_item.iloc[idx]['Cnz'] * scaling['Cnz']:.3e}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'CURZ':>4s}{direction_identifier:>5s}{direction_identifier:>5s}{Cnz:>10s}"
            body.append(body_item_str)

        return body

    def get_dc_11_body(self, dc_cfg):
        raw_data = dc_cfg['data']['raw']
        body = []
        
        for item in raw_data:
            element_type = item['element_type']

            if element_type == "CPRF":
                depth = item['depth']
                if depth > 0:
                    logging.error('Depth must be negative')
                    raise ValueError('Depth must be negative')
                depth = f"{depth:.1f}"
                speed = item['speed']
                speed = f"{speed:.2f}"
                direction = item['direction']
                direction = f"{direction:.1f}"
                body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{element_type:>4s}{depth:>10s}{speed:>10s}{direction:>10s}"

            elif element_type == "WIND":
                speed = item['speed']
                speed = f"{speed:.2f}"
                direction = item['direction']
                direction = f"{direction:.1f}"
                body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{element_type:>4s}{speed:>10s}{direction:>10s}"

            body.append(body_item_str)

        return body


    def get_dc_18_body(self, dc_cfg):
        raw_data = dc_cfg['data']['raw']
        element_type = dc_cfg['data']['element_type']
        body = []

        structure = raw_data['structure']
        node_array = raw_data['nodes']
        for node in node_array:
            body_item_idx = node_array.index(node)
            node = node_array[body_item_idx]

            body_item_str = f"{white_space:>1s}{white_space:>3s}{body_item_idx+1:>2d}{element_type:>4s}{structure:>5d}{node:>5d}"
            body.append(body_item_str)

        return body


