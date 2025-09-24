import sys
import os
sys.path.insert(0, r'D:\github\digitalmodel')
os.chdir(r'D:\github\digitalmodel\specs\modules\signal-analysis\rainflow_with_visualization')

# Import and run the analyzer
from run_rainflow_analysis import main
sys.argv = ['run_rainflow_analysis.py', 'input/rainflow_analysis_config.yml']
main()