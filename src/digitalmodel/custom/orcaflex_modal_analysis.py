import os
import pandas as pd

import OrcFxAPI

from digitalmodel.custom.orcaflex_utilities import OrcaflexUtilities

ou = OrcaflexUtilities()


class OrcModalAnalysis:

    def __init__(self):
        self.modes_df_columns = [
            'modeIndex', 'name', 'node', 'dof', 'shapeWrtGlobal'
        ]
        self.modes_summary_df_columns = [
            'modeIndex', 'period', 'name', 'node', 'abs_max_dof',
            'shapeWrtGlobal', 'shape_dof_percentage'
        ]
        self.all_file_summary = {}
        self.all_file_summary_df_columns = ['Label'
                                           ] + self.modes_summary_df_columns

    def run_modal_analysis(self, cfg=None):
        self.cfg = cfg
        if cfg['default']['Analysis']['Analyze']['modal']:
            files = cfg['Files']
            for file in files:
                all_modes_summary_df = self.run_by_file(file=file['Name'])
                self.all_file_summary.update(
                    {file['Label']: all_modes_summary_df})

            self.summarize_all_files()

    def summarize_all_files(self):
        files = self.cfg['Files']
        for dof in self.cfg['default']['Analysis']['Analyze']['modal'][
                'summary_dof_array']:
            all_file_summary_df = pd.DataFrame(
                columns=self.all_file_summary_df_columns)
            for file in files:
                all_modes_summary_df = self.all_file_summary[file['Label']]
                df_dof = all_modes_summary_df[
                    all_modes_summary_df['abs_max_dof'] == dof]
                if len(df_dof) > 0:
                    all_file_summary_df.loc[len(all_file_summary_df.index)] = [
                        file['Label']
                    ] + df_dof.iloc[0].tolist()
            file_name = self.cfg.Analysis['result_folder'] + self.cfg.Analysis[
                'file_name_for_overwrite'] + f'_all_file_summary_{dof}.csv'
            all_file_summary_df.to_csv(file_name, index=False)

    def run_by_file(self, file=None):
        model = OrcFxAPI.Model()
        model.LoadData(file)
        model.CalculateStatics()
        ou.save_sim_file(model, file)
        lastMode = self.cfg['default']['Analysis']['Analyze']['modal'][
            'lastMode']
        spec = OrcFxAPI.ModalAnalysisSpecification(calculateShapes=True,
                                                   lastMode=lastMode)

        modes = OrcFxAPI.Modes(model, spec)
        # self.explore_mode_shapes(modes)
        all_modes_summary_df = self.save_mode_shapes(modes, file)

        return all_modes_summary_df

    def explore_mode_shapes(self, modes):
        print(f"{modes.modeCount} modes, {modes.dofCount} dofs")
        print(modes.period)
        for modeIndex in range(modes.modeCount):

            details = modes.modeDetails(modeIndex)
            print(details.modeNumber)
            print(details.period)
            # Nodal 3 dof displacements
            print(details.shapeWrtGlobal)
            # TODO Investigate why same value for all modes?
            print(details.percentageInAxialDirection)

            for dofIndex in range(modes.dofCount):
                print(f"{modes.owner[dofIndex].name} node {modes.nodeNumber[dofIndex]:2} " \
                f"dof {modes.dof[dofIndex]}: {details.shapeWrtGlobal[dofIndex]}")

    def save_mode_shapes(self, modes, file):
        print(f"{modes.modeCount} modes, {modes.dofCount} dofs")
        print(modes.period)
        all_modes_df = pd.DataFrame(columns=self.modes_df_columns)

        all_modes_summary_df = pd.DataFrame(
            columns=self.modes_summary_df_columns)
        for modeIndex in range(modes.modeCount):

            mode_df = pd.DataFrame(columns=self.modes_df_columns).copy()
            details = modes.modeDetails(modeIndex)
            for dofIndex in range(modes.dofCount):
                name = modes.owner[dofIndex].name
                node = modes.nodeNumber[dofIndex]
                dof = modes.dof[dofIndex]
                shapeWrtGlobal = details.shapeWrtGlobal[dofIndex]
                mode_array = [modeIndex, name, node, dof, shapeWrtGlobal]
                mode_df.loc[len(mode_df.index)] = mode_array

            all_modes_df = pd.concat([all_modes_df, mode_df], ignore_index=True)

            mode_summary_df = self.get_mode_summary(mode_df)
            mode_summary_df['period'] = details.period
            mode_summary_df['modeIndex'] = modeIndex

            all_modes_summary_df = pd.concat(
                [all_modes_summary_df, mode_summary_df], ignore_index=True)
            print(f"Processing mode {modeIndex} ... COMPLETE")

        file_name = os.path.splitext(file)[0] + '_modes.csv'
        all_modes_df.to_csv(file_name, index=False)
        all_modes_summary_df = self.save_summary_files(file,
                                                       all_modes_summary_df)
        return all_modes_summary_df

    def save_summary_files(self, file, all_modes_summary_df):
        file_name = os.path.splitext(file)[0] + '_modes_summary.csv'
        all_modes_summary_df.sort_values(by=['name', 'modeIndex'], inplace=True)
        all_modes_summary_df.to_csv(file_name, index=False)
        for dof in self.cfg['default']['Analysis']['Analyze']['modal'][
                'summary_dof_array']:
            file_name = os.path.splitext(file)[0] + f'_modes_summary_{dof}.csv'
            all_modes_summary_df[all_modes_summary_df['abs_max_dof'] ==
                                 dof].to_csv(file_name, index=False)

        return all_modes_summary_df

    def get_mode_summary(self, mode_df) -> pd.DataFrame:
        mode_summary_df = pd.DataFrame(columns=self.modes_summary_df_columns)
        ObjectNames = self.cfg['default']['Analysis']['Analyze']['modal'][
            'ObjectName']

        for ObjectName in ObjectNames:
            ObjectName_shapewrtglobal = mode_df[mode_df['name'] ==
                                                ObjectName].copy()
            idxmax = ObjectName_shapewrtglobal.shapeWrtGlobal.idxmax()
            idxmin = ObjectName_shapewrtglobal.shapeWrtGlobal.idxmin()
            shapeWrtGlobal_max = ObjectName_shapewrtglobal.shapeWrtGlobal.loc[
                idxmax]
            shapeWrtGlobal_min = ObjectName_shapewrtglobal.shapeWrtGlobal.loc[
                idxmin]

            if abs(shapeWrtGlobal_max) > abs(shapeWrtGlobal_min):
                shapeWrtGlobal = ObjectName_shapewrtglobal.shapeWrtGlobal.loc[
                    idxmax]
                max_dof = ObjectName_shapewrtglobal.dof.loc[idxmax]
                node = ObjectName_shapewrtglobal.node.loc[idxmax]

            else:
                shapeWrtGlobal = ObjectName_shapewrtglobal.shapeWrtGlobal.loc[
                    idxmin]
                max_dof = ObjectName_shapewrtglobal.dof.loc[idxmin]
                node = ObjectName_shapewrtglobal.node.loc[idxmin]

            ObjectName_shapewrtglobal_node = ObjectName_shapewrtglobal[
                ObjectName_shapewrtglobal.node == node].copy()
            shape_dof_percentage = round(
                abs(shapeWrtGlobal) /
                ObjectName_shapewrtglobal_node.shapeWrtGlobal.abs().sum() * 100,
                1)
            ObjectName_summary = [
                None, None, ObjectName, node, max_dof, shapeWrtGlobal,
                shape_dof_percentage
            ]
            mode_summary_df.loc[len(mode_summary_df.index)] = ObjectName_summary

        return mode_summary_df
