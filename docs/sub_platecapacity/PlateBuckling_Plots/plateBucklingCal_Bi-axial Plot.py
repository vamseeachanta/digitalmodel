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
plt.plot([ηseG_simp,ηseH_simp,ηseI_simp,ηseJ_simp,ηseK_simp],color='k')
ax2=fig.add_subplot(512)
plt.setp(ax2.get_xticklabels(), visible=False)
plt.plot([ηueG_simp,ηueH_simp,ηueI_simp,ηueJ_simp,ηueK_simp],color='b')
ax3=fig.add_subplot(513)
plt.setp(ax3.get_xticklabels(), visible=False)
ax3.set_ylabel('Utilization', bbox=box)
plt.plot([ηseG_side,ηseH_side,ηseI_side,ηseJ_side,ηseK_side],color='g')
ax4=fig.add_subplot(514)
plt.setp(ax4.get_xticklabels(), visible=False)
plt.plot([ηueG_side,ηueH_side,ηueI_side,ηueJ_side,ηueK_side],color='m')
ax5=fig.add_subplot(515)
ax5.set_xlabel('Plate Number', bbox=box)
plt.suptitle('Bi-axial Direction Loading', bbox=box)
plt.plot([ηueG_side,ηueH_side,ηueI_side,ηueJ_side,ηueK_side],color='c')

plt.show()
