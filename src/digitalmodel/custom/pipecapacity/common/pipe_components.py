from pipecapacity.custom.MaterialProperties import MaterialProperties
from pipecapacity.custom.PipeCapacity import PipeCapacity
from pipecapacity.custom.PipeSizing import PipeSizing
from pipecapacity.custom.saveData import saveDataYaml

class PipeComponents:
    def __init__(self, cfg):
        self.cfg = cfg
        self.well_summary_df = None
        if self.cfg.default.__contains__('db'):
            self.environments = list(self.cfg.default['db'].keys())
            self.env = self.environments[0]
        self.add_custom_folder()

    def add_custom_folder(self):
        import os
        if self.cfg.default.__contains__('custom_folder'):
            if self.cfg.default['custom_folder'] is not None:
                if not os.path.exists(self.cfg.default['custom_folder']):
                    os.mkdir(self.cfg.default['custom_folder'])

                if os.path.exists(self.cfg.default['custom_folder']):
                    self.cfg['Analysis']['result_folder'] = self.cfg.default['custom_folder']

    def get_environments(self):
        self.environments = list(self.cfg.default['db'].keys())

    def set_up_db_connection(self):
        from common.database import Database
        db_properties = self.cfg.default['db'][self.env]
        self.dbe = Database(db_properties)
        self.dbe.enable_connection_and_cursor()

    def get_material_data(self):
        query = "SELECT TOP 1 * FROM [dbo].[Witsml] WITH (SNAPSHOT) WHERE WellID = {0} ORDER BY DateUpdatedInserted DESC".format(
            self.well_id)
        df = self.dbe.get_df_from_query(query)
        self.last_witsml = df


    def evaluate_pipe_capacity(self):

        cfg = self.cfg
        pipe_flag = 'Outer_Pipe'
        if cfg.__contains__(pipe_flag):
            material = cfg[pipe_flag]['Material']['Material']
            material_grade = cfg[pipe_flag]['Material']['Material_Grade']
            cfg[pipe_flag]['Material'].update(cfg['Material'][material])
            cfg[pipe_flag]['Material'].update(cfg['Material'][material]['Grades'][material_grade])
        pipe_flag = 'Inner_Pipe'
        if cfg.__contains__(pipe_flag) and cfg[pipe_flag] is not None:
            material = cfg[pipe_flag]['Material']['Material']
            material_grade = cfg[pipe_flag]['Material']['Material_Grade']
            cfg[pipe_flag]['Material'].update(cfg['Material'][material])
            cfg[pipe_flag]['Material'].update(cfg['Material'][material]['Grades'][material_grade])

        Pipe = PipeSizing(cfg)
        Pipe.evaluate_pipe_system_properties()
        Material = MaterialProperties(cfg)
        Material.evaluate_material_properties()

        # Evaluate Pipe Capacity
        Pipe_Capacity = PipeCapacity(cfg)
        Pipe_Capacity.evaluate_pipe_wall()

        saveDataYaml(cfg, cfg['Analysis']['result_folder'] + cfg['Analysis']['file_name'])
