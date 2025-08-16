# Signal Analysis Module Migration Report

Found 9 files requiring migration

Total changes required: 37


## Change Summary
- Import statements: 8
- Method calls: 5
- Configuration keys: 24

## Files Requiring Migration


### examples\signal_analysis_usage_example.py
Lines to update: 363, 367, 371

Line 363:
  Old: `get_rainflow_count_from_time_series`
  New: `count_cycles`
  Context: cycles_df, cycles_dict = adapter.get_rainflow_count_from_time_series(signal)

Line 367:
  Old: `window_average_fft`
  New: `window_averaged_fft`
  Context: fft_result = adapter.window_average_fft(legacy_cfg, signal, time_step=0.01)

Line 371:
  Old: `get_filtered_fft`
  New: `filter_spectrum`
  Context: filtered_fft = adapter.get_filtered_fft(legacy_cfg, fft_result)

### scripts\migrate_to_signal_analysis.py
Lines to update: 29, 32, 35, 39, 42, 43, 44, 47, 48, 56, 57, 59, 159, 160, 161, 175

Line 29:
  Old: `from digitalmodel.modules.time_series.time_series_components import TimeSeriesComponents`
  New: `from digitalmodel.modules.signal_analysis.adapters import TimeSeriesComponentsAdapter as TimeSeriesComponents`
  Context: 'from digitalmodel.modules.time_series.time_series_components import TimeSeriesComponents':

Line 32:
  Old: `from digitalmodel.common.fatigue_analysis import`
  New: `from digitalmodel.modules.signal_analysis.fatigue import`
  Context: 'from digitalmodel.common.fatigue_analysis import':

Line 35:
  Old: `from digitalmodel.time_series import`
  New: `from digitalmodel.modules.signal_analysis import`
  Context: 'from digitalmodel.time_series import':

Line 39:
  Old: `TimeSeriesComponents(`
  New: `TimeSeriesComponentsAdapter(`
  Context: 'TimeSeriesComponents(': 'TimeSeriesComponentsAdapter(',

Line 42:
  Old: `get_rainflow_count_from_time_series`
  New: `count_cycles`
  Context: 'get_rainflow_count_from_time_series': 'count_cycles',

Line 43:
  Old: `window_average_fft`
  New: `window_averaged_fft`
  Context: 'window_average_fft': 'window_averaged_fft',

Line 44:
  Old: `get_filtered_fft`
  New: `filter_spectrum`
  Context: 'get_filtered_fft': 'filter_spectrum',

Line 47:
  Old: `"time_series_components"`
  New: `"signal_analysis"`
  Context: '"time_series_components"': '"signal_analysis"',

Line 48:
  Old: `'time_series_components'`
  New: `'signal_analysis'`
  Context: "'time_series_components'": "'signal_analysis'",

Line 56:
  Old: `get_rainflow_count_from_time_series`
  New: `count_cycles`
  Context: 'get_rainflow_count_from_time_series',

Line 57:
  Old: `window_average_fft`
  New: `window_averaged_fft`
  Context: 'window_average_fft',

Line 59:
  Old: `'time_series_components'`
  New: `'signal_analysis'`
  Context: 'time_series_components'

Line 159:
  Old: `from digitalmodel.modules.time_series.time_series_components import TimeSeriesComponents`
  New: `from digitalmodel.modules.signal_analysis.adapters import TimeSeriesComponentsAdapter as TimeSeriesComponents`
  Context: report.append("from digitalmodel.modules.time_series.time_series_components import TimeSeriesComponents")

Line 160:
  Old: `TimeSeriesComponents(`
  New: `TimeSeriesComponentsAdapter(`
  Context: report.append("tsc = TimeSeriesComponents(cfg)")

Line 161:
  Old: `get_rainflow_count_from_time_series`
  New: `count_cycles`
  Context: report.append("cycles = tsc.get_rainflow_count_from_time_series(signal)")

Line 175:
  Old: `get_rainflow_count_from_time_series`
  New: `count_cycles`
  Context: report.append("cycles = adapter.get_rainflow_count_from_time_series(signal)")

### src\digitalmodel\engine.py
Lines to update: 16

Line 16:
  Old: `from digitalmodel.common.fatigue_analysis import`
  New: `from digitalmodel.modules.signal_analysis.fatigue import`
  Context: from digitalmodel.common.fatigue_analysis import FatigueAnalysis

### src\digitalmodel\time_series.py
Lines to update: 46

Line 46:
  Old: `TimeSeriesComponents(`
  New: `TimeSeriesComponentsAdapter(`
  Context: ts = TimeSeriesComponents(cfg)

### src\digitalmodel\common\fatigue_analysis.py
Lines to update: 166

Line 166:
  Old: `get_rainflow_count_from_time_series`
  New: `count_cycles`
  Context: rainflow_df, rainflow_dict = tsa.get_rainflow_count_from_time_series(timetrace)

### src\digitalmodel\common\ship_fatigue_analysis.py
Lines to update: 10

Line 10:
  Old: `from digitalmodel.common.fatigue_analysis import`
  New: `from digitalmodel.modules.signal_analysis.fatigue import`
  Context: from digitalmodel.common.fatigue_analysis import FatigueAnalysis

### src\digitalmodel\modules\orcaflex\opp_time_series.py
Lines to update: 11, 242

Line 11:
  Old: `from digitalmodel.modules.time_series.time_series_components import TimeSeriesComponents`
  New: `from digitalmodel.modules.signal_analysis.adapters import TimeSeriesComponentsAdapter as TimeSeriesComponents`
  Context: from digitalmodel.modules.time_series.time_series_components import TimeSeriesComponents

Line 242:
  Old: `TimeSeriesComponents(`
  New: `TimeSeriesComponentsAdapter(`
  Context: ts_comp = TimeSeriesComponents({"default": {"analysis": {"fft": cfg_fft}}})

### src\digitalmodel\modules\time_series\time_series_analysis.py
Lines to update: 9, 11, 26, 27, 29, 30, 33

Line 9:
  Old: `from digitalmodel.modules.time_series.time_series_components import TimeSeriesComponents`
  New: `from digitalmodel.modules.signal_analysis.adapters import TimeSeriesComponentsAdapter as TimeSeriesComponents`
  Context: from digitalmodel.modules.time_series.time_series_components import TimeSeriesComponents

Line 11:
  Old: `TimeSeriesComponents(`
  New: `TimeSeriesComponentsAdapter(`
  Context: tca = TimeSeriesComponents()

Line 26:
  Old: `window_average_fft`
  New: `window_averaged_fft`
  Context: if cfg["analysis"]["basic"]["sample_window_average_fft"]:

Line 27:
  Old: `window_average_fft`
  New: `window_averaged_fft`
  Context: sig_fft, filtered_signal = tca.sample_window_average_fft(cfg)

Line 29:
  Old: `window_average_fft`
  New: `window_averaged_fft`
  Context: if cfg["analysis"]["basic"]["window_average_fft"]:

Line 30:
  Old: `window_average_fft`
  New: `window_averaged_fft`
  Context: tca.window_average_fft(cfg)

Line 33:
  Old: `get_rainflow_count_from_time_series`
  New: `count_cycles`
  Context: rainflow_df, rainflow_dict = tca.get_rainflow_count_from_time_series(

### src\digitalmodel\modules\time_series\time_series_components.py
Lines to update: 35, 67, 75, 242, 623

Line 35:
  Old: `window_average_fft`
  New: `window_averaged_fft`
  Context: def window_average_fft(self, cfg):

Line 67:
  Old: `get_filtered_fft`
  New: `filter_spectrum`
  Context: average_fft_df = self.get_filtered_fft(cfg, average_fft_df)

Line 75:
  Old: `window_average_fft`
  New: `window_averaged_fft`
  Context: def sample_window_average_fft(self, cfg):

Line 242:
  Old: `get_filtered_fft`
  New: `filter_spectrum`
  Context: def get_filtered_fft(self, cfg, average_fft_df):

Line 623:
  Old: `get_rainflow_count_from_time_series`
  New: `count_cycles`
  Context: def get_rainflow_count_from_time_series(self, time_series: array) -> pd.DataFrame:

## Migration Instructions

1. **Backup your code** before running migration
2. **Review changes** in the report above
3. **Run migration** with `--apply` flag to make changes
4. **Test thoroughly** after migration
5. **Update configurations** in YAML/JSON files

## Example Migrations

### Old Code:
```python
from digitalmodel.modules.time_series.time_series_components import TimeSeriesComponents
tsc = TimeSeriesComponents(cfg)
cycles = tsc.get_rainflow_count_from_time_series(signal)
```

### New Code:
```python
from digitalmodel.modules.signal_analysis import RainflowCounter
counter = RainflowCounter()
cycles = counter.count_cycles(signal)
```

### Or using adapter for compatibility:
```python
from digitalmodel.modules.signal_analysis.adapters import TimeSeriesComponentsAdapter
adapter = TimeSeriesComponentsAdapter(cfg)
cycles = adapter.get_rainflow_count_from_time_series(signal)
```