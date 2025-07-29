import os
import math
import pandas as pd
from assetutilities.common.update_deep import update_deep_dictionary

from assetutilities.common.yml_utilities import ymlInput
from assetutilities.common.data import SaveData

from assetutilities.common.utilities import is_file_valid_func

from digitalmodel.modules.orcaflex.orcaflex_linetypes import OrcaflexLineTypes

save_data = SaveData()
olt = OrcaflexLineTypes()


class Lower2ndEnd:
    def __init__(self):
        pass

    def installation_phase(self, cfg, phase):
        for step in phase["step"]:
            self.installation_step(cfg, step)

        return cfg

    def installation_step(self, cfg, step):
        installation_step_dict = {}

        installation_step_dict = update_deep_dictionary(
            installation_step_dict, {"includefile": step["includefile"]}
        )
        for target_settings in step["target"]:
            target_dict = self.get_target_result(cfg, target_settings, step)
            installation_step_dict = update_deep_dictionary(
                installation_step_dict, target_dict
            )

        # Write step file
        analysis_root_folder = cfg["Analysis"]["analysis_root_folder"]
        filename_path = os.path.join(analysis_root_folder, step["name"])
        save_data.saveDataYaml(
            installation_step_dict, filename_path, default_flow_style=False
        )

    def get_target_result(self, cfg, target_settings, step_cfg):
        if target_settings["type"] == "line_length":
            return self.get_target_line_length(cfg, target_settings)
        elif target_settings["type"] == "reference_distance":
            return self.get_target_reference_distance(cfg, target_settings, step_cfg)
        else:
            raise NotImplementedError("Target type not implemented.")

    def get_target_line_length(self, cfg, target_settings):
        reference_file = target_settings["reference_file"]
        analysis_root_folder = cfg["Analysis"]["analysis_root_folder"]
        file_is_valid, reference_file = is_file_valid_func(
            reference_file, analysis_root_folder
        )
        reference_file_yml = ymlInput(reference_file)
        Lines = target_settings["Lines"]
        reference_line_length = 0
        for line in Lines:
            columns = ["LineType", "Length", "TargetSegmentLength"]
            reference_file_dict = reference_file_yml["Lines"][line][
                "LineType, Length, TargetSegmentLength"
            ]
            df = pd.DataFrame(reference_file_dict, columns=columns)
            LineType = target_settings["LineType"]
            df_filter = df[df["LineType"].isin(LineType)]
            reference_line_length = reference_line_length + df_filter["Length"].sum()

        keychain_target = target_settings["keychain_target"]
        s = keychain_target[2]
        indx = int(s[s.find("[") + 1 : s.find("]")])
        current_line_length = reference_file_yml["Lines"][keychain_target[1]][
            "LineType, Length, TargetSegmentLength"
        ][indx - 1][1]
        length_correction = target_settings["value"] - reference_line_length

        dict = {}
        if length_correction != 0:
            target_length = round(float(current_line_length + length_correction), 1)
            dict.update(
                {"Lines": {keychain_target[1]: {keychain_target[2]: target_length}}}
            )

        return dict

    def get_target_reference_distance(self, cfg, target_settings, step_cfg):

        lay_direction = cfg["installation"]["lay_direction"]
        reference_distance_definition = cfg["installation"][
            "reference_distance_definition"
        ]
        installation_vessel_reference_location = cfg["installation"][
            "installation_vessel"
        ]["reference_location"]
        reference_distance = target_settings["value"]

        keychain_target_vessel = target_settings["keychain_target_vessel"]

        initial_heading = target_settings["initial_heading"]
        initial_x = (
            reference_distance_definition[0]
            + reference_distance * math.cos(math.radians(lay_direction))
            - installation_vessel_reference_location[0]
            * math.cos(math.radians(initial_heading))
        )
        initial_y = (
            reference_distance_definition[1]
            + reference_distance * math.sin(math.radians(lay_direction))
            - installation_vessel_reference_location[1]
            * math.sin(math.radians(initial_heading))
        )

        dict = {}
        dict = update_deep_dictionary(
            dict,
            {
                keychain_target_vessel[0]: {
                    keychain_target_vessel[1]: {
                        "InitialX": round(initial_x, 1),
                        "InitialY": round(initial_y, 1),
                        "InitialHeading": round(initial_heading, 1),
                    }
                }
            },
        )

        keychain_target_line = target_settings.get("keychain_target_line", None)
        if keychain_target_line is not None:
            endbx = initial_x
            endby = initial_y
            endbz = -20 - step_cfg["target"][0]["value"]
            for keychain_target_line_item in keychain_target_line:
                dict = update_deep_dictionary(
                    dict,
                    {
                        "Lines": {
                            keychain_target_line_item[1]: {
                                "EndBX": round(endbx, 1),
                                "EndBY": round(endby, 1),
                                "EndBZ": round(endbz, 1),
                            }
                        }
                    },
                )

        keychain_target_6DBuoys = target_settings.get("keychain_target_6DBuoys", None)
        if keychain_target_6DBuoys is not None:
            buoy_initial_x = initial_x
            buoy_initial_y = initial_y
            buoy_initial_z = -20 - step_cfg["target"][0]["value"]
            for keychain_target_6DBuoys_item in keychain_target_6DBuoys:
                dict = update_deep_dictionary(
                    dict,
                    {
                        "6DBuoys": {
                            keychain_target_6DBuoys_item[1]: {
                                "InitialX": round(buoy_initial_x, 1),
                                "InitialY": round(buoy_initial_y, 1),
                                "InitialZ": round(buoy_initial_z, 1),
                            }
                        }
                    },
                )

        keychain_target_3DBuoys = target_settings.get("keychain_target_3DBuoys", None)
        if keychain_target_3DBuoys is not None:
            buoy_initial_x = initial_x
            buoy_initial_y = initial_y
            buoy_initial_z = -20 - step_cfg["target"][0]["value"]

            for keychain_target_3DBuoys_item in keychain_target_3DBuoys:
                dict = update_deep_dictionary(
                    dict,
                    {
                        "3DBuoys": {
                            keychain_target_3DBuoys_item[1]: {
                                "InitialX": round(buoy_initial_x, 1),
                                "InitialY": round(buoy_initial_y, 1),
                                "InitialZ": round(buoy_initial_z, 1),
                            }
                        }
                    },
                )

        return dict
