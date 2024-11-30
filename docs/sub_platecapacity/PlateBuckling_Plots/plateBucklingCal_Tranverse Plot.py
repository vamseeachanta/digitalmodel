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
ax1.plot([FALSEG,FALSEH,FALSEI,FALSEJ,FALSEK],color='k')
ax2=fig.add_subplot(512)
plt.setp(ax2.get_xticklabels(), visible=False)
ax2.plot([ηuyG_simp,ηuyH_simp,ηuyI_simp,ηuyJ_simp,ηuyK_simp],color='b')
ax3=fig.add_subplot(513)
plt.setp(ax3.get_xticklabels(), visible=False)
ax3.set_ylabel('Utilization', bbox=box)
ax3.plot([ηsyG_side,ηsyH_side,ηsyI_side,ηsyJ_side,ηsyK_side],color='g')
ax4=fig.add_subplot(514)
plt.setp(ax4.get_xticklabels(), visible=False)
ax4.plot([ηuyG_side,ηuyH_side,ηuyI_side,ηuyJ_side,ηuyK_side],color='m')
ax5=fig.add_subplot(515)
ax5.set_xlabel('Plate Number', bbox=box)
plt.suptitle('Transverse Direction Loading', bbox=box)
ax5.plot([TransverseG,TransverseH,TransverseI,TransverseJ,TransverseK],color='c')

plt.show()
