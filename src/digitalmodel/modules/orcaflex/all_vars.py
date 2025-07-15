try:
    import OrcFxAPI
except Exception:
    raise RuntimeError("OrcaFlex license not available. Run on different computer")
from loguru import logger
import os
import pandas as pd
from digitalmodel.modules.orcaflex.orcaflex_utilities import OrcaflexUtilities
from digitalmodel.modules.orcaflex.orcaflex_objects import OrcaFlexObjects

ou = OrcaflexUtilities()  # noqa
of_objects = OrcaFlexObjects()  # noqa


class AllVars:
    def __init__(self, cfg=None):
        orcaflex_license_flag = ou.is_orcaflex_available()
        assert orcaflex_license_flag
        if not orcaflex_license_flag:
            raise Exception("Orcaflex license not available.")

    def router(self, cfg):
        analysis_flag = False
        if (
            "all_vars" in cfg["orcaflex"]["postprocess"]
            and cfg["orcaflex"]["postprocess"]["all_vars"]["flag"]
        ):
            analysis_flag = True
        if not analysis_flag:
            return cfg

        sim_files = cfg["file_management"]["input_files"]["sim"]
        var_data_all_files = []
        for fileIndex in range(0, len(sim_files)):
            file_name = sim_files[fileIndex]

            var_data_dict = self.get_var_data_by_model(cfg, file_name)
            var_data_all_files.append(var_data_dict)

        return var_data_all_files

    def get_var_data_by_model(self, cfg, file_name):
        model_dict = ou.get_model_and_metadata(file_name=file_name)
        model = model_dict["model"]

        if model is not None:
            base_name = os.path.basename(file_name)
            file_label, _ = os.path.splitext(base_name)
            object_dict = of_objects.get_model_objects(model)
            object_df = object_dict["object_df"]
            var_data_dict = self.get_var_data(cfg, model, object_df, file_label)
            var_data_dict.update({"file_name": file_name})
            var_data_dict.update({"file_label": file_label})

        return var_data_dict

    def get_var_data(self, cfg, model, object_df, file_label):
        """
        Get the variable data for an object_df.
        OrcFxAPI.ResultType:
            TimeHistory = 0
            RangeGraph = 1
            LinkedStatistics = 2
            FrequencyDomain = 3
        """
        output_dict = {}
        var_data_StaticResult = self.get_var_data_StaticResult(
            cfg, model, object_df, file_label
        )

        output_dict["StaticResult"] = var_data_StaticResult
        output_dict["TimeHistory"] = None
        output_dict["RangeGraph"] = None
        output_dict["LinkedStatistics"] = None
        output_dict["FrequencyDomain"] = None

        return output_dict

    def get_var_data_StaticResult(self, cfg, model, object_df, file_label):
        output_dict = self.initialize_var_data_output_dict(object_df)

        for df_index in range(0, len(object_df)):
            object_df_row = object_df.iloc[df_index]
            ObjectName = object_df_row["ObjectName"]
            object = model[ObjectName]

            # Get the variable names
            objectExtra = None
            objectExtraName = None
            if object.type.name == "Line":
                objectExtra = OrcFxAPI.oeEndB
                objectExtraName = "EndB"

            var_df_dict = of_objects.get_object_vars(
                cfg, model, object, objectExtra, ResultType=None
            )
            VarNames = var_df_dict["VarNames"]

            # Get variable Data
            try:
                if objectExtra is not None:
                    VarData = object.StaticResult(VarNames, objectExtra=objectExtra)
                else:
                    VarData = object.StaticResult(VarNames)

                data_df_row = [file_label, ObjectName, objectExtraName] + list(VarData)
                df_columns = ["file_label", "ObjectName", "objectExtraName"] + VarNames
                VarData_df = pd.DataFrame(columns=df_columns)
                VarData_df.loc[0] = data_df_row
            except Exception as e:
                logger.error(
                    f"Error retrieving StaticResult for {object_df_row['ObjectName']}: {e}"
                )
                VarData_df = pd.DataFrame()

            ObjectTypeName = object_df_row["ObjectTypeName"]
            ObjectTypeName_df = output_dict[ObjectTypeName].copy()
            ObjectTypeName_df = pd.concat([ObjectTypeName_df, VarData_df], axis=0)
            output_dict[ObjectTypeName] = ObjectTypeName_df.copy()

        # Save the variable data to a file
        self.save_var_data(cfg, output_dict)

        return output_dict

    def initialize_var_data_output_dict(self, object_df):
        """
        Initialize the output dictionary for the object_df.
        """
        output_dict = {}

        ObjectTypeNames = list(object_df["ObjectTypeName"].unique())

        for objectTypeName in ObjectTypeNames:
            output_dict[objectTypeName] = pd.DataFrame()

        return output_dict

    def save_var_data(self, cfg, output_dict):
        """
        Save the variable data to a file.
        """
        # Save the variable data to a file
        for object_type in output_dict.keys():
            var_data_df = output_dict[object_type]
            if not var_data_df.empty:
                result_folder = cfg["Analysis"]["result_folder"]
                file_name = cfg["Analysis"]["file_name_for_overwrite"]
                output_name = f"{file_name}_{object_type}_var_data.csv"
                output_file = os.path.join(result_folder, output_name)
                var_data_df.to_csv(output_file, index=False)
                logger.info(
                    f"Saved variable data for {object_type} " f"to {output_name}"
                )
