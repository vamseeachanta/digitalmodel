import OrcFxAPI

model = OrcFxAPI.Model()
environment = model.environment
environment.MultipleCurrentDataCanBeDefined = "Yes"

# define two current data sets, with rather useless names
environment.CurrentName = "Current1", "Current2"

# select the first current data set, and set the speed
environment.SelectedCurrent = "Current1"
environment.RefCurrentSpeed = 0.3

# select the second current data set, and set the speed
environment.SelectedCurrent = "Current2"
environment.RefCurrentSpeed = 0.5
