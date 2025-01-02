
class OPPVisualization():
    
    def __init__(self) -> None:
        pass

    def get_visualizations(self, cfg):
        self.save_views_for_files(cfg)

    def is_file_valid(self, file_name):
        is_file_valid, file_name = is_file_valid_func(file_name)

        return is_file_valid, file_name

    def save_views_for_files(self, cfg):
        model = OrcFxAPI.Model()
        combined_model = None

        if cfg.file_management['files']['files_in_current_directory'][
                'flag']:
            orcaflex_extensions = ['yml', 'yaml', 'dat', 'sim', 'txt']

        else:
            orcaflex_extensions = cfg.file_management['input_files'].keys()

        for file_ext in orcaflex_extensions:
            raw_input_files_for_ext = cfg.file_management['input_files'][
                file_ext]

            for input_file_index in range(0, len(raw_input_files_for_ext)):
                input_file = raw_input_files_for_ext[input_file_index]

                model.LoadData(input_file)

                if cfg['visualization_settings']['combined']:
                    print("Combined model code in library does not exist")
                    # combined_model = self.combine_models(combined_model, model)

                model = self.set_general_visualization_settings(model, cfg)
                model.CalculateStatics()
                self.save_all_views(model, input_file, cfg)

            #TODO 
            # if cfg['visualization_settings']['combined']:
            #     combined_model.CalculateStatics()
                
    def set_general_visualization_settings(self, model, cfg):
        #TODO for TDP Colour change
        # line = model[cfg['visualization_settings']['tdp_line']]
        # line.ContactPenColour = 128 * 65536 + 128 * 256 + 128

        env = model['Environment']
        # env.SeabedPenStyle = "Clear"
        # env.SeabedProfilePenStyle = "Clear"
        env.SeaSurfacePenStyle = "Clear"
        model.general.NorthDirectionDefined = "No"

        #TODO for vessel settings
        # vessel = model["SevenArctic"]
        # x_value = vessel.InitialX
        # y_value = vessel.InitialY
        # heading = vessel.InitialHeading

        hide_items = cfg['visualization_settings']['hide_items']

        all_objects = []
        for obj in model.objects:
            Name = str(obj)
            all_objects.append(Name)
        for item in hide_items:
            if item in all_objects:
                model[item].Hidden = "Yes"

        #TODO crane settings
        # crane = model["250TeCrane"]
        # crane.OutsidePenStyle = "Dot"
        # crane.InsidePenStyle = "Clear"
        # crane.NumberOfLines = 2
        return model

    def combine_models(self, combined_model, model):
        if combined_model is None:
            combined_model = model
        else:
            for obj in model.objects:
                combined_model.createObject(obj)
                line = combined_model.CreateObject(obj.type)

        combined_model.SaveData("combined_model.dat")
        return combined_model

    def save_all_views(self, model, file_name, cfg):

        viewparams_cfg = cfg['visualization_settings']['viewparams']
        for view_label in list(viewparams_cfg.keys()):
            viewparams = self.assign_view_parameters(model, cfg, view_label)
            self.save_image(model, file_name, viewparams, view_label)

    def assign_view_parameters(self, model, cfg, view_label):

        viewparams_view_label_cfg = cfg['visualization_settings']['viewparams'][view_label]
        viewparams = model.defaultViewParameters

        if 'SeaSurfacePenStyle' in viewparams_view_label_cfg:
            env = model['Environment']
            env.SeaSurfacePenStyle = viewparams_view_label_cfg['SeaSurfacePenStyle']

        for key in viewparams_view_label_cfg:
            try:
                if key == 'ViewCentre':
                    ViewCentre = viewparams_view_label_cfg['ViewCentre']
                    for i in range(0, len(ViewCentre)):
                        viewparams.ViewCentre[i] = ViewCentre[i]
                elif key == 'RelativeToObject':
                    viewparams.RelativeToObject = model[
                        viewparams_view_label_cfg['RelativeToObject']]
                else:
                    setattr(viewparams, key, viewparams_view_label_cfg[key])
            except Exception as e:
                logging.error(str(e))

        return viewparams

    def save_image(self, model, file_name, viewparams, view_label):
        file_location = os.path.split(file_name)[0]
        file_name_img = os.path.basename(file_name).split(
            ".")[0] + "_" + view_label + ".jpg"
        file_name_with_path = os.path.join(file_location, file_name_img)
        logging.info(f"Saving ...  {file_name_img}  view")
        model.SaveModelView(file_name_with_path, viewparams)
