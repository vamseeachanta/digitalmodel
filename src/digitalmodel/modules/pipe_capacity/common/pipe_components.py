from digitalmodel.modules.pipe_capacity.custom.MaterialProperties import (
    MaterialProperties,
)
from digitalmodel.modules.pipe_capacity.custom.PipeCapacity import PipeCapacity
from digitalmodel.modules.pipe_capacity.custom.PipeSizing import PipeSizing


# Reader imports


class PipeComponents:
    def __init__(self):
        pass

    # def add_custom_folder(self):
    #     # Standard library imports
    #     import os
    #     if self.cfg.default.__contains__('custom_folder'):
    #         if self.cfg.default['custom_folder'] is not None:
    #             if not os.path.exists(self.cfg.default['custom_folder']):
    #                 os.mkdir(self.cfg.default['custom_folder'])

    #             if os.path.exists(self.cfg.default['custom_folder']):
    #                 self.cfg['Analysis']['result_folder'] = self.cfg.default['custom_folder']

    # def get_environments(self):
    #     self.environments = list(self.cfg.default['db'].keys())

    # def set_up_db_connection(self):
    #     # Third party imports
    #     from common.database import Database
    #     db_properties = self.cfg.default['db'][self.env]
    #     self.dbe = Database(db_properties)
    #     self.dbe.enable_connection_and_cursor()

    # def get_material_data(self):
    #     query = "SELECT TOP 1 * FROM [dbo].[Witsml] WITH (SNAPSHOT) WHERE WellID = {0} ORDER BY DateUpdatedInserted DESC".format(
    #         self.well_id)
    #     df = self.dbe.get_df_from_query(query)
    #     self.last_witsml = df

    def evaluate_pipe_capacity(self, cfg: dict) -> None:
        pipe_flag = "Outer_Pipe"
        if cfg.__contains__(pipe_flag):
            material = cfg[pipe_flag]["Material"]["Material"]
            material_grade = cfg[pipe_flag]["Material"]["Material_Grade"]
            
            # Check if Material section exists in cfg
            if "Material" not in cfg:
                cfg["Material"] = {}
            
            # Add default material properties if not present
            if material not in cfg["Material"]:
                cfg["Material"][material] = {
                    "Density": 0.284,  # lb/in³ for steel
                    "Rho": 0.284,      # Same as Density - another alias
                    "E": 30000000,     # psi for steel
                    "Poissons_Ratio": 0.3,
                    "Poissionsratio": 0.3,  # Fix for typo in PipeSizing.py
                    "G": 11538462,     # Shear modulus for steel (E/2(1+v))
                    "Grades": {}
                }
            
            # Add default grade properties if not present
            if "Grades" not in cfg["Material"][material]:
                cfg["Material"][material]["Grades"] = {}
                
            if material_grade not in cfg["Material"][material]["Grades"]:
                cfg["Material"][material]["Grades"][material_grade] = {
                    "YieldStrength": 35000,  # psi default for ASTM A106 Grade C
                    "TensileStrength": 70000,
                    "SMYS": 35000,
                    "SMUS": 70000,  # Same as TensileStrength
                    "SMTS": 70000,  # Same as TensileStrength
                    "Manufacturing": "Seamless"  # Default manufacturing method
                }
            
            cfg[pipe_flag]["Material"].update(cfg["Material"][material])
            cfg[pipe_flag]["Material"].update(
                cfg["Material"][material]["Grades"][material_grade]
            )
            
            # Add default Manufacturing section if not present
            if "Manufacturing" not in cfg[pipe_flag]:
                cfg[pipe_flag]["Manufacturing"] = {
                    "Coupling Mass Ratio": 0.1
                }
        pipe_flag = "Inner_Pipe"
        if cfg.__contains__(pipe_flag) and cfg[pipe_flag] is not None:
            material = cfg[pipe_flag]["Material"]["Material"]
            material_grade = cfg[pipe_flag]["Material"]["Material_Grade"]
            
            # Check if Material section exists in cfg
            if "Material" not in cfg:
                cfg["Material"] = {}
            
            # Add default material properties if not present
            if material not in cfg["Material"]:
                cfg["Material"][material] = {
                    "Density": 0.284,  # lb/in³ for steel
                    "Rho": 0.284,      # Same as Density - another alias
                    "E": 30000000,     # psi for steel
                    "Poissons_Ratio": 0.3,
                    "Poissionsratio": 0.3,  # Fix for typo in PipeSizing.py
                    "G": 11538462,     # Shear modulus for steel (E/2(1+v))
                    "Grades": {}
                }
            
            # Add default grade properties if not present
            if "Grades" not in cfg["Material"][material]:
                cfg["Material"][material]["Grades"] = {}
                
            if material_grade not in cfg["Material"][material]["Grades"]:
                cfg["Material"][material]["Grades"][material_grade] = {
                    "YieldStrength": 35000,  # psi default for ASTM A106 Grade C
                    "TensileStrength": 70000,
                    "SMYS": 35000,
                    "SMUS": 70000,  # Same as TensileStrength
                    "SMTS": 70000,  # Same as TensileStrength
                    "Manufacturing": "Seamless"  # Default manufacturing method
                }
            
            cfg[pipe_flag]["Material"].update(cfg["Material"][material])
            cfg[pipe_flag]["Material"].update(
                cfg["Material"][material]["Grades"][material_grade]
            )
            
            # Add default Manufacturing section if not present
            if "Manufacturing" not in cfg[pipe_flag]:
                cfg[pipe_flag]["Manufacturing"] = {
                    "Coupling Mass Ratio": 0.1
                }

        # Add default DesignFactors if not present
        if "DesignFactors" not in cfg:
            cfg["DesignFactors"] = {}
            
        # Add default design factors for common codes
        if "API RP 1111-2009 Risers" not in cfg["DesignFactors"]:
            cfg["DesignFactors"]["API RP 1111-2009 Risers"] = {
                "internal_pressure": 0.67,
                "Longitudinal": 0.80,
                "EquivalentStress": 0.90,
                "D_over_T_Trasition_Ratio": 30
            }

        Pipe: PipeSizing = PipeSizing(cfg)
        Pipe.evaluate_pipe_system_properties()
        Material: MaterialProperties = MaterialProperties(cfg)
        Material.evaluate_material_properties()

        # Evaluate Pipe Capacity
        Pipe_Capacity: PipeCapacity = PipeCapacity(cfg)
        Pipe_Capacity.evaluate_pipe_wall()
