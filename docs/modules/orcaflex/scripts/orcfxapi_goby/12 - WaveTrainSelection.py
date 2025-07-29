import OrcFxAPI

model = OrcFxAPI.Model()
environment = model.environment

# define two wave trains, with informative names
environment.WaveName = "Swell", "Wind generated"

# select the swell wave train, and set its data
environment.SelectedWave = "Swell"
environment.WaveType = "Single Airy"
environment.WaveDirection = 140.0
environment.WaveHeight = 4.5
environment.WavePeriod = 17.0

# select the wind generated wave train, and set its data
environment.SelectedWave = "Wind generated"
environment.WaveType = "JONSWAP"
environment.WaveDirection = 75.0
environment.WaveHs = 3.0
environment.WaveTp = 9.0
