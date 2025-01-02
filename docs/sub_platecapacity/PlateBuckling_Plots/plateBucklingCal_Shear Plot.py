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
plt.plot([ηszG_simp,ηszH_simp,ηszI_simp,ηszJ_simp,ηszK_simp],color='k')
ax2=fig.add_subplot(512)
plt.setp(ax2.get_xticklabels(), visible=False)
plt.plot([ηuzG_simp,ηuzH_simp,ηuzI_simp,ηuzJ_simp,ηuzK_simp],color='b')
ax3=fig.add_subplot(513)
plt.setp(ax3.get_xticklabels(), visible=False)
ax3.set_ylabel('Utilization', bbox=box)
plt.plot([ηszG_side,ηszH_side,ηszI_side,ηszJ_side,ηszK_side],color='g')
ax4=fig.add_subplot(514)
plt.setp(ax4.get_xticklabels(), visible=False)
plt.plot([ηuzG_side,ηuzH_side,ηuzI_side,ηuzJ_side,ηuzK_side],color='m')
ax5=fig.add_subplot(515)
ax5.set_xlabel('Plate Number', bbox=box)
plt.suptitle('Shear Direction Loading', bbox=box)
plt.plot([ShearG,ShearH,ShearI,ShearJ,ShearK],color='c')

plt.show()
