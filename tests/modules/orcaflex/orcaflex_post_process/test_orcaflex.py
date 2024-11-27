# Third party imports
from test_opp_summary1 import test_process as test_opp_summary1_test
from test_opp_summary2_master import test_process as test_opp_summary2_master_test
from test_opp_time_series1 import test_process as test_opp_time_series1_test
from test_opp_time_series2_master import (
    test_process as test_opp_time_series2_master_test,
)
from test_orcaflex_license import test_orcaflex_license
test_orcaflex_license()

test_opp_summary1_test()
test_opp_summary2_master_test()

test_opp_time_series1_test()
test_opp_time_series2_master_test()
