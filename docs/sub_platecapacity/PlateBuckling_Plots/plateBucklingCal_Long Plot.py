from DataProvision.plateBucklingCal_G import *
from DataProvision.plateBucklingCal_H import *
from DataProvision.plateBucklingCal_i import *
from DataProvision.plateBucklingCal_J import *
from DataProvision.plateBucklingCal_K import *


import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
box = dict(facecolor='Blue', pad=5, alpha=0.2)
fig=plt.figure(1)
ax1=fig.add_subplot(511)
plt.setp(ax1.get_xticklabels(), visible=False)
ax1.plot([ηsxG_simp,ηsxH_simp,ηsxI_simp,ηsxJ_simp,ηsxK_simp],color='k')

ax2=fig.add_subplot(512)
plt.setp(ax2.get_xticklabels(), visible=False)
ax2.plot([ηuxG_simp,ηuxH_simp,ηuxI_simp,ηuxJ_simp,ηuxK_simp],color='b')

ax3=fig.add_subplot(513)
ax3.set_ylabel('Utilization', bbox=box)
plt.setp(ax3.get_xticklabels(), visible=False)
ax3.plot([ηsxG_side,ηsxH_side,ηsxI_side,ηsxJ_side,ηsxK_side],color='g')

ax4=fig.add_subplot(514)
plt.setp(ax4.get_xticklabels(), visible=False)
ax4.plot([ηuxG_side,ηuxH_side,ηuxI_side,ηuxJ_side,ηuxK_side],color='m')


ax5=fig.add_subplot(515)
ax5.set_xlabel('Plate Number', bbox=box)
plt.suptitle('Longitudinal Direction Loading', bbox=box)
ax5.plot([LongitudinalG,LongitudinalH,LongitudinalI,LongitudinalJ,LongitudinalK],color='c')

plt.show()
