# -*- coding: utf-8 -*-
"""
Created on Fri May 17 08:39:31 2019

@author: karimia
"""
import pandas as pd


def rig_name_matcher_openwells(active_wells, rig_names):
    #print(active_wells[['WellId','WellName' ]])
    #print('\n')
    #print(rig_names['RigName'])
    
    rig_comapnies = ['Helmerich & Payne', 'Trinidad Drilling','Savanna Drilling',\
                     'Precision Drilling Company','Independence Drilling','Tuscany South America LTD',\
                     'Viva Energy Services', 'Ensign Energy Services','Pioneer', 'Patterson-UTI Energy Inc'\
                     'Transend Drilling']
    #rig_comapnies = list(map(lambda x:x.lower(), rig_comapnies))
    
    rig_comapnies_abbr = ['H&P', 'Trinidad','Savanna', \
                          'Precision Drilling', 'Independence Drilling', 'Tuscany South America',\
                           'Viva Energy', 'Ensign', 'Pioneer', 'Patterson']
    rig_comapnies_abbr =  list(map(lambda x:x.upper(), rig_comapnies_abbr))
    
    df_temp = active_wells.merge(rig_names, on = 'RigId', how = 'left')
    
    #current rig companies in Oxy
    active_wells_with_rig = df_temp[['WellId', 'WellName', 'WellStatusId', 'RigId', 'UIDWell',
       'WellDescription', 'NovWellId', 'CreatedDateTime_x', 'CreatedBy_x',
       'LastModifiedDateTime_x', 'LastModifiedBy_x', 'RigName']]
    
    # extracting the number from the rig name
    #active_wells_with_rig['RigNo'].iloc[:]= active_wells_with_rig['RigName'].str.extract('(\d+)')
    active_wells_with_rig.insert( loc = 5, column = 'OpenwellsRigname', value = 'not_found')  
    active_wells_with_rig.insert( loc = 6, column = 'RigNo', value = active_wells_with_rig['RigName'].str.extract('(\d+)'))

    
    for i in range(0, active_wells_with_rig.shape[0]):
        for j in range(0, len(rig_comapnies)):            
            if rig_comapnies[j] in active_wells_with_rig['RigName'].iloc[i]:
                active_wells_with_rig.loc[i, 'OpenwellsRigname']= rig_comapnies_abbr[j].upper() + ' ' + str(active_wells_with_rig['RigNo'].iloc[i])   
                break
             
    return active_wells_with_rig
    
    