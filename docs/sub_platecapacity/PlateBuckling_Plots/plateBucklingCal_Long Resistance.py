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
ax1.plot([σscrxG_simp,σscrxH_simp,σscrxI_simp,σscrxJ_simp,σscrxK_simp],color='k')
ax2=fig.add_subplot(512)
plt.setp(ax2.get_xticklabels(), visible=False)
ax2.plot([σucrxG_simp1,σucrxH_simp1,σucrxI_simp1,σucrxJ_simp1,σucrxK_simp1],color='b')
ax3=fig.add_subplot(513)
plt.setp(ax3.get_xticklabels(), visible=False)
ax3.set_ylabel('Buckling resistance', bbox=box)
ax3.plot([σscrxG_side,σscrxH_side,σscrxI_side,σscrxJ_side,σscrxK_side],color='g')
ax4=fig.add_subplot(514)
plt.setp(ax4.get_xticklabels(), visible=False)
ax4.plot([σucrxG_side1,σucrxH_side1,σucrxI_side1,σucrxJ_side1,σucrxK_side1],color='m')
ax5=fig.add_subplot(515)
ax5.set_xlabel('Plate Number', bbox=box)
plt.suptitle('Longtudinal Direction Loading', bbox=box)
ax5.plot([σG_xrd,σH_xrd,σI_xrd,σJ_xrd,σK_xrd],color='c')

plt.show()
