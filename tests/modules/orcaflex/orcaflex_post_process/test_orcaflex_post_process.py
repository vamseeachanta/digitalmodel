# Third party imports
from test_orcaflex_license import test_orcaflex_license
from test_opp_linked_statistics1 import test_process as test_opp_linked_statistics1_copy
from test_opp_summary1 import test_process as test_opp_summary1_copy
from test_opp_summary2_master import test_process as test_opp_summary2_master_copy
from test_opp_time_series1 import test_process as test_opp_time_series1_copy
from test_opp_time_series2_master import (
    test_process as test_opp_time_series2_master_copy,
)

test_orcaflex_license()

test_opp_summary1_copy()
test_opp_summary2_master_copy()

test_opp_linked_statistics1_copy()

test_opp_time_series1_copy()
test_opp_time_series2_master_copy()
