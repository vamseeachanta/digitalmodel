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
ax1.plot([σscrzG_simp,σscrzH_simp,σscrzI_simp,σscrzJ_simp,σscrzK_simp],color='k')
ax2=fig.add_subplot(512)
plt.setp(ax2.get_xticklabels(), visible=False)
ax2.plot([σucrzG_simp1,σucrzH_simp1,σucrzI_simp1,σucrzJ_simp1,σucrzK_simp1],color='b')
ax3=fig.add_subplot(513)
plt.setp(ax3.get_xticklabels(), visible=False)
ax3.set_ylabel('Buckling resistance', bbox=box)
ax3.plot([σscrzG_side,σscrzH_side,σscrzI_side,σscrzJ_side,σscrzK_side],color='g')
ax4=fig.add_subplot(514)
plt.setp(ax4.get_xticklabels(), visible=False)
ax4.plot([σucrzG_side1,σucrzH_side1,σucrzI_side1,σucrzJ_side1,σucrzK_side1],color='m')
ax5=fig.add_subplot(515)
ax5.set_xlabel('Plate Number', bbox=box)
plt.suptitle('Shear Direction Loading', bbox=box)
ax5.plot([τ_rdG,τ_rdH,τ_rdI,τ_rdJ,τ_rdK],color='c')

plt.show()
