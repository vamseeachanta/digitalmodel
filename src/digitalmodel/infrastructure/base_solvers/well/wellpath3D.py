### Michael Kramer ###
### Wellpath3D ###
### Borehole Path Calculation and Visualization ###
### Minimum Curvature Method ###
### Last Update 022.01.2020 ###

import math
import os
import sqlite3
import sys
import tkinter as tk
import tkinter.scrolledtext
from tkinter import *
from tkinter import Frame, ttk
from tkinter.filedialog import askopenfilename

import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib import cm
from matplotlib.figure import Figure
from mpl_toolkits.mplot3d import Axes3D
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
from scipy.interpolate import griddata


def instantTab(x):
    return x

base = ".db"
wellnames_given = []
wellactual = []
wells_plot = []
fmtop = []
### wellmap listen
base = ".db"
wellnamelist = []
wells_selected = []
well_objects = []
#global xyzFile
#xyzFile = "x" 
#Wellnames and Koordinates database to be created during first start

def firstStart():
    print('first start')
    if not os.path.exists('wellnames.db'):
        con = sqlite3.connect('wellnames.db')
        cursor = con.cursor()
        sql = 'CREATE TABLE wells('\
              'name TEXT, '\
              'latitude REAL, '\
              'longitude REAL,'\
              'altitude REAL)'
        cursor.execute(sql) 
        con.close()
   
    else:
        print("file wellnames.db already exists",
              " load wells from database...")
        con = sqlite3.connect('wellnames.db')
        cursor = con.cursor()
        sql = "SELECT * FROM wells"
        cursor.execute(sql)
        for dsatz in cursor:
            wellnames_given.append(dsatz[0])
        con.close()
    

class newwell:
    
    def __init__(self, nn, lat, lon, alt):
        self.newname = nn
        self.latitude = lat
        self.longitude = lon
        self.altitude = alt

    def create(self):

        tryname = self.newname.replace("'","")

        if "." in tryname:
           tryname.replace(".","*")
           
        else:
             tryname = tryname         
        
        if tryname in wellnames_given:
            self.newname = self.newname + "2"
        
        else:
         
            z="'"
            write_name = z+self.newname+z
            con = sqlite3.connect('wellnames.db')
            cursor = con.cursor()
            sql = "INSERT INTO wells VALUES("\
                  + write_name + ","\
                  + str(self.latitude) + "," + str(self.longitude) + "," + str(self.altitude) + ")"
            #print(sql)
            cursor.execute(sql)
            con.commit()
            con.close()
            
        #if not tryname in wellnames_given:
        connex = sqlite3.connect(tryname + base)
        cursor = connex.cursor()
        sql_0 = 'CREATE TABLE geosurvey('\
                'Depth REAL PRIMARY KEY, '\
                'Inc REAL, '\
                'Azi REAL,'\
                'TVD REAL,'\
                'North REAL,'\
                'East REAL,'\
                'Closure,'\
                'DLS)'
        cursor.execute(sql_0)
        sql_1 = "INSERT INTO geosurvey VALUES(0,0,0,0,0,0,0,0)"
        cursor.execute(sql_1)
        connex.commit()
        connex.close()

        # Create Table stratigraphy
        stratcon = sqlite3.connect(tryname + base)
        cursor = stratcon.cursor()
        sql_2 = 'CREATE TABLE stratigraphy('\
                'Depth REAL PRIMARY KEY, '\
                'Fm_Name TEXT, '\
                'Fm_Color TEXT,'\
                'Fm_TVD REAL,'\
                'Fm_Inc REAL,'\
                'Fm_Azi REAL,'\
                'Fm_North REAL,'\
                'Fm_East  REAL,'\
                'Fm_alt REAL,'\
                'Fm_lat REAL,'\
                'Fm_lon REAL)'
        cursor.execute(sql_2)

        sql_3 = "INSERT INTO stratigraphy VALUES(0,'Holocene','#fef2e0',0,0,0,0,0,0,0,0)"
        
        #+str(self.altitude)+","+str(self.latitude)+","+str(self.longitude)+")"
        #print(sql_3)
        cursor.execute(sql_3)
        stratcon.commit()

        sql_4 = "UPDATE stratigraphy SET Fm_alt = " +str(self.altitude) + ""
        sql_5 = "UPDATE stratigraphy SET Fm_lat = " +str(self.latitude) + ""
        sql_6 = "UPDATE stratigraphy SET Fm_lon = " +str(self.longitude) + ""
        cursor.execute(sql_4)
        cursor.execute(sql_5)
        cursor.execute(sql_6)
        stratcon.commit()
        
        stratcon.close()

        #Create Table wellmap
        mapcon = sqlite3.connect(tryname + base)
        cursor = mapcon.cursor()
        msql = 'CREATE TABLE wellmap('\
               'geotvd REAL,'\
               'latitude REAL,'\
               'longitude REAL)'
        cursor.execute(msql)
        mapcon.commit()
        xsql = "INSERT INTO wellmap VALUES("\
               + str(self.altitude) + ","\
               + str(self.latitude) + ","\
               + str(self.longitude) + ")"
        
        cursor.execute(xsql)
        mapcon.commit()
        mapcon.close()
        #create instance of the class well
        currentwell = well(self.newname,0,0,0)


class well:
    def __init__(self, bez, teu, inc, azi):
        self.name = bez
        self.Depth = teu
        self.Inc = inc
        self.Azi = azi
        self.depthlist = []
        self.inclist = []
        self.azilist = []

    def __str__(self):
        return str(self.name) + "," +str(self.Depth) + " m " + str(self.Inc) + "° "\
               + str(self.Azi) + "°"

    def depthtolist(self):  #Teufe als Liste 
        connection = sqlite3.connect(self.name + base)
        cursor = connection.cursor()
        #Abfrage
        sql = "SELECT * FROM geosurvey"
        #print(sql) #Kontrollprint der Abfrage
        cursor.execute(sql)
        for dsatz in cursor:
            self.depthlist.append(dsatz[0])
        #print(self.depthlist)
        connection.close()

    def inctolist(self):
        connection = sqlite3.connect(self.name + base)
        cursor = connection.cursor()
        sql = "SELECT * FROM geosurvey"
        #print(sql) #Kontrollprint
        cursor.execute(sql)

        for dsatz in cursor:
            self.inclist.append(dsatz[1])
        #print(self.inclist)
        connection.close()

    def azitolist(self):
        connection = sqlite3.connect(self.name + base)
        cursor = connection.cursor()
        #Abfrage
        sql = "SELECT * FROM geosurvey"
        #print(sql) #Kontrollprint
        cursor.execute(sql)

        for dsatz in cursor:
            self.azilist.append(dsatz[2])
        #print(self.azilist)

class wellmap:
    def __init__(self, bez, tvd, lat, lon):
        self.name = bez
        self.Tvd = tvd
        self.Lat = lat
        self.Lon = lon
        self.tvdlist = []
        self.latlist = []
        self.lonlist = []

        self.strat_altilist = []
        self.strat_latlist = []
        self.strat_lonlist = []
        self.strat_colorlist = []
        # check if wellname is already in the base
        
    def __str__(self):
        return str(self.name) + "," + str(self.Tvd) + " m "\
               + str(self.Lat) + "° " + str(self.Lon) + "°"

    def getName(self):
        return str(self.name)

    def add_coordinates(self, wert1, wert2, wert3):
        self.surface_alt = wert1              #hoehe über NN
        self.surface_lat = wert2
        self.surface_lon = wert3
        if not os.path.exists(self.name + base):
            wcon = sqlite3.connect(self.name + base)
            cursor = wcon.cursor()
            sql = 'CREATE TABLE surface('\
                  'surfacealt REAL PRIMARY KEY, '\
                  'surfacelat REAL, '\
                  'surfacelon REAL)'
            cursor.execute(sql)
            wcon.close()
        else:
            print('database loaded')

        wcon = sqlite3.connect(self.name + base)
        cursor = wcon.cursor()

        sql = "INSERT INTO wellmap VALUES(" + str(wert1) + ","\
              + str(wert2) + "," + str(wert3) + ")"
        #print(sql)
        cursor.execute(sql)
        wcon.commit()
        wcon.close()    
            
   
    def writedata(self, wert1, wert2, wert3):

        if not os.path.exists(self.name + base):
            wcon = sqlite3.connect(self.name + base)
            cursor = wcon.cursor()
            sql = 'CREATE TABLE wellmap('\
                  'tvd REAL PRIMARY KEY, '\
                  'lat REAL, '\
                  'lon REAL)'
            cursor.execute(sql)
            wcon.close()
        else:
            print('database loaded')

        wcon = sqlite3.connect(self.name + base)
        cursor = wcon.cursor()

        sql = "INSERT INTO wellmap VALUES(" + str(wert1*(-1)) + ","\
              + str(wert2) + "," + str(wert3) + ")"
        #print(sql)
        cursor.execute(sql)
        wcon.commit()
        wcon.close()

    def tvdtolist(self):  #Teufe als Liste 
        connection = sqlite3.connect(self.name + base)
        cursor = connection.cursor()
        #Abfrage
        sql = "SELECT * FROM wellmap"
        #print(sql) #Kontrollprint der Abfrage
        cursor.execute(sql)

        for dsatz in cursor:
            self.tvdlist.append(dsatz[0])
        #print(self.tvdlist)

    def lattolist(self):
        connection = sqlite3.connect(self.name + base)
        cursor = connection.cursor()
        #Abfrage
        sql = "SELECT * FROM wellmap"
        #print(sql) #Kontrollprint
        cursor.execute(sql)

        for dsatz in cursor:
            self.latlist.append(dsatz[1])
        #print(self.latlist)

        #Verbindung Trennen
        connection.close()

    def lontolist(self):
        connection = sqlite3.connect(self.name + base)
        cursor = connection.cursor()
        #Abfrage
        sql = "SELECT * FROM wellmap"
        #print(sql) #Kontrollprint
        cursor.execute(sql)

        for dsatz in cursor:
            self.lonlist.append(dsatz[2])
        #print(self.lonlist)

    def strattolist(self):
        
        conni = sqlite3.connect(self.name + base)
        cursor = conni.cursor()
        sql_st = "SELECT Fm_alt FROM stratigraphy"
        cursor.execute(sql_st)
        for dsatz in cursor:
            self.strat_altilist.append(dsatz[0])
        #print(self.strat_altilist)

        sql_slat = "SELECT Fm_lat FROM stratigraphy"
        cursor.execute(sql_slat)
        for lats in cursor:
            self.strat_latlist.append(lats[0])
        #print(self.strat_latlist)

        sql_slon = "SELECT Fm_lon FROM stratigraphy"
        cursor.execute(sql_slon)
        for lons in cursor:
            self.strat_lonlist.append(lons[0])
        #print(self.strat_lonlist)

        sql_color = "SELECT Fm_Color FROM stratigraphy"
        cursor.execute(sql_color)
        for color in cursor:
            self.strat_colorlist.append(color[0])
        #print(self.strat_lonlist)
        
        conni.close()


class myApplication():
    
    def __init__(self, root):
        self.root = root
        self.root.title = "Wellpath 3D Visualization"
        self.init_gui()
        
    def infobox(self, string):
        #print("infobox function active")
        self.infoLabel['text'] = string    

    def init_gui(self):
        self.createWindow()
        self.createWin()
        self.wellsWin()
        self.surveyWin()
        self.plotWin()
        self.wellmapWin()
        self.stratWin()

    def createWindow(self):
        logo = PhotoImage(file = 'icons/wpLogoSmall.png')
        self.mainLabel = tk.Label(root, image = logo)
        self.mainLabel.photo = logo
        #self.mainLabel = tk.Label(root, text="Wellpath 3D - Borehole Database and Visualization",
        #                    bg="#ffc600",pady=12, padx=12)
        self.mainLabel.grid(row=0, column=0, columnspan=2,sticky=E+W)
        style=ttk.Style()
        style.configure("TNotebook", bg="white")
        self.portal = ttk.Notebook(root)
        self.portal.grid(row=2, column=0, columnspan=2)

        self.tab0 = ttk.Frame(self.portal, padding=20)
        self.tab1 = ttk.Frame(self.portal, padding=20)
        self.tab2 = ttk.Frame(self.portal, padding=20)
        self.tab3 = ttk.Frame(self.portal, padding=20)
        self.tab4 = ttk.Frame(self.portal, padding=20)
        self.tab5 = ttk.Frame(self.portal, padding=20)

        self.portal.add(self.tab0, text="     Create     ")
        self.portal.add(self.tab1, text="     Wells      ")
        self.portal.add(self.tab2, text="    Survey      ")
        self.portal.add(self.tab3, text="    3D Plot     ")
        self.portal.add(self.tab4, text="    Wellmap     ")
        self.portal.add(self.tab5, text=" Stratigraphy   ")

        #main window bottom elements
        self.infoLabel = tk.Label(root, text="user information",
                                  bg="white", fg="#4d2e00")
        self.infoLabel.grid(row=3, column=0, columnspan=2, sticky=E+W)
        self.tab0Separator = ttk.Separator(root, orient="horizontal")
        self.tab0Separator.grid(row=7, column=0, columnspan=2, sticky=W+E)
        self.statusLabel = tk.Label(root, text="Active borehole: ", fg="red")
        self.statusLabel.grid(row=8, column=0, sticky=E)
        self.statusActive = tk.Label(root, text="Load from database or create new well")
        self.statusActive.grid(row=8, column=1, sticky=W)


    def createWin(self):

        def createWell():
            wellName = str(tab0nameEntry.get())
            lat = float(tab0latEntry.get())
            lon = float(tab0lonEntry.get())
            alt = float(tab0altEntry.get())
            
            well = newwell(wellName, lat, lon, alt)
            well.create()
            infoString =  "New borehole created"
            self.infoLabel['text'] = infoString
            
        boreholeFrame = tk.Frame(self.tab0)
        boreholeFrame.pack()
        tab0Label1 = tk.Label(boreholeFrame, text="Create a new borehole", bg="#ffffff", pady=4, padx=12)
        tab0Label1.grid(row=0, column=0, columnspan=2, sticky=E+W)
        tab0Separator = ttk.Separator(boreholeFrame, orient="horizontal")
        tab0Separator.grid(row=1, column=0, columnspan=2, sticky=W+E, pady=8)
        tab0BottomSeparator = ttk.Separator(boreholeFrame, orient="horizontal")
        tab0BottomSeparator.grid(row=6, column=0, columnspan=2, sticky=W+E, pady=8)
        tab0nameLabel = tk.Label(boreholeFrame, text="Name:", pady=4)
        tab0nameLabel.grid(row=2, column=0)
        tab0nameEntry = tk.Entry(boreholeFrame,  width=32)
        tab0nameEntry.grid(row=2, column=1)
        tab0latLabel = tk.Label(boreholeFrame, text="Latitude:", pady=4)
        tab0latLabel.grid(row=3, column=0)
        tab0latEntry = tk.Entry(boreholeFrame,  width=32)
        tab0latEntry.grid(row=3, column=1)
        tab0lonLabel = tk.Label(boreholeFrame, text="Longitude:", pady=4)
        tab0lonLabel.grid(row=4, column=0)
        tab0lonEntry = tk.Entry(boreholeFrame,  width=32)
        tab0lonEntry.grid(row=4, column=1)
        tab0altLabel = tk.Label(boreholeFrame, text="Altitude:", pady=4)
        tab0altLabel.grid(row=5, column=0)
        tab0altEntry = tk.Entry(boreholeFrame, width=32)
        tab0altEntry.grid(row=5, column=1)
        iconOk = PhotoImage(file = 'icons/iconOkSmall.png')
        tab0Button = tk.Button(boreholeFrame, text = 'Submit', padx = 4, image = iconOk,
                               compound = LEFT, command = createWell)
        tab0Button.image = iconOk
        #tab0Button = tk.Button(boreholeFrame, text="submit", command=createWell)
        tab0Button.grid(row=7, column=0, columnspan=2)
 
                  
    # Wells tab
    def wellsWin(self):

        def fileload():
       
        #Verbinde mit Datenbank
            connection = sqlite3.connect('wellnames.db')
            cursor = connection.cursor()
        #Abfrage
            sql = "SELECT * FROM wells"
            cursor.execute(sql)
            dsatz = cursor.fetchall()
            tab1Tree.delete(*tab1Tree.get_children())   #clear the table
            for row in dsatz:
                tab1Tree.insert("", tk.END,
                        values=row)
            connection.close()
            

        def selectItem(a):
            curItem = tab1Tree.focus()
            try:
                self.infoLabel['text'] = (tab1Tree.item(curItem)['values'][0])
            except:
                self.infoLabel['text'] = "choose a well with mouse button and press load"
                
            
        def openwell():
            curItem = tab1Tree.focus()
            try:
                #print(tab1Tree.item(curItem)['values'][0])
                ow = (tab1Tree.item(curItem)['values'][0])
                openWell = well(ow, 0,0,0)
            
            #print(openWell)
            
                currentwell = ow
                wellactual.append(currentwell)

                self.infoLabel['text'] = "Well loaded from database"
                self.statusActive['text'] = ow
                #surveyWin(loadsurvey()) #fuelle die survey tabelle
            except:
                self.infoLabel['text'] = "choose well from list"
            

        def delwell():
            currentItem = tab1Tree.focus()
            deletewell = (tab1Tree.item(currentItem)['values'][0])
            z = "'"
            abc = z+deletewell+z
           
            delcon = sqlite3.connect('wellnames.db')
            cursor = delcon.cursor()
            sqldel = "DELETE FROM wells WHERE Name = "+abc+""
            cursor.execute(sqldel)
            delcon.commit()
            delcon.close()
            os.remove(deletewell+base)
            self.infoLabel['text'] = "Well removed from database"

        
        ### gui
        wellsWinFrame = tk.Frame(self.tab1)
        wellsWinFrame.pack()
        #print("wellWinFrame")
        tab1Label1 = tk.Label(wellsWinFrame, text="Well Database", bg="#ffffff", pady=4, padx=12)
        tab1Label1.grid(row=0, column=0, columnspan=40, sticky=W+E)
        tab1Separator = ttk.Separator(wellsWinFrame, orient="horizontal")
        tab1Separator.grid(row=1, column=0, columnspan=40, sticky=W+E, pady=9)

        bottomSeparator = ttk.Separator(wellsWinFrame, orient="horizontal")
        bottomSeparator.grid(row=3, column=0, columnspan=40, sticky=W+E, pady=9)
            #Buttons
        tab1RefreshButton = tk.Button(wellsWinFrame, text="Show All", command=fileload)
        tab1RefreshButton.grid(row=4, column=0, sticky=W+E)

        tab1LoadButton = tk.Button(wellsWinFrame, text="Load Well", command=openwell)
        tab1LoadButton.grid(row=4, column=1, sticky=W+E)

        tab1DeleteButton = tk.Button(wellsWinFrame, text="Delete Well", command=delwell, bg="#ff4646")
        tab1DeleteButton.grid(row=4, column=2, sticky=W+E, padx=12)
            # Treeview
        tab1Tree = ttk.Treeview(wellsWinFrame, selectmode="browse",
                                column=("one", "two", "three", "four"),
                                show="headings")
        tab1Tree.heading("one", text="Well")
        tab1Tree.heading("two", text="Latitude")
        tab1Tree.heading("three", text="Longitude")
        tab1Tree.heading("four", text="Altitude")
        tab1Tree.bind('<ButtonRelease-1>', selectItem)
        tab1Tree.bind('<Double-Button 1>', selectItem) 
        tab1Tree.grid(row=2,column=0, columnspan=38)
            #Scrollbar
        wScroll = ttk.Scrollbar(wellsWinFrame ,command=tab1Tree.yview)
        wScroll.grid(row=2, column=39, sticky=N+S+W)
        tab1Tree.configure(yscrollcommand=wScroll.set, height=22)
            #open database 
        fileload()

    #surveyTab - tab 2

    def surveyWin(self):
        
        def importCsv():

            # Funktionen für die Berechnung der geographischen Koordinaten und Altitude                
            def north2lat(north, latitude):
             
                lat_p = math.sqrt(latitude**2)
                new_lat = latitude + (north/(110574+(lat_p*12.44)))

                if latitude >= 0 and new_lat >= 90:
                    return 180 - new_lat

                elif latitude <=0 and new_lat <= -90:
                     return -180 - new_lat
                else:
                    return new_lat
        
                        
            def east2lon(east, latitude, longitude):
                new_lon = longitude + ((east * math.cos(math.radians(latitude)))/111120)

                if new_lon >= 180:
                    return -360 + new_lon
                
                elif new_lon <= -180:
                    return 360 + new_lon
        
                else:
                     return new_lon
            try:
                currentwell = wellactual[-1]
                Tk().withdraw()
                csvFile = askopenfilename()
            
                csvName = str(csvFile)
                csv_names = ['Depth', 'Inc', 'Azi', 'TVD', 'North', 'East', 'Closure','DLS']
                csv_pdf = pd.read_csv(csvName, names = csv_names)
                self.infoLabel['text'] ='Loading File...'

            # make sqlite3 connection to write new csv data from dataframe
                csv_con = sqlite3.connect(currentwell + base)
                cursor = csv_con.cursor()
                csv_pdf.to_sql("geosurvey", con=csv_con, if_exists="append", index=False)
                csv_con.close()
            # Datenbank laden für wellmap
                well = currentwell
                z="'"
                cwell = z+str(currentwell)+z

                gcon = sqlite3.connect("wellnames.db")
                cursor = gcon.cursor()
                sqla = "SELECT altitude FROM wells WHERE name = " +cwell+ ""
                cursor.execute(sqla)

                for c in cursor:
                     z_surface = c[0]
            
                gcon.close()
                connex = sqlite3.connect("wellnames.db")
                cursor = connex.cursor()
                sqllat = "SELECT latitude FROM wells WHERE name = "+cwell+""
                cursor.execute(sqllat)
                for lat in cursor:
                    lat_surface = lat[0]
             #print(lat_surface)
                sqllon = "SELECT longitude FROM wells WHERE name = "+cwell+""
                cursor.execute(sqllon)
                for lon in cursor:
                    lon_surface = lon[0]

                slat = lat_surface 
                slon = lon_surface
         #### Datenbank geladen, Variablen deklariert
         #calculate lat, lon and altitude for wellmap
           
                csv_north_list = csv_pdf.North.tolist()
                csv_east_list = csv_pdf.East.tolist()
                csv_tvd_list = csv_pdf.TVD.tolist()
                csv_Lat = []
                csv_Lon = []
                csv_Alt = []

                for i in range(0, len(csv_north_list)):
                    increment_lat = north2lat(csv_north_list[i], lat_surface)
                    csv_Lat.append(increment_lat)

                for i in range(0, len(csv_east_list)):
                    increment_Lon = east2lon(csv_east_list[i],lat_surface,lon_surface)
                    csv_Lon.append(increment_Lon)

                for i in range(0,len(csv_tvd_list)):
                    increment_Tvd = z_surface - csv_tvd_list[i]
                    csv_Alt.append(increment_Tvd)

                se_lat = pd.Series(csv_Lat)
                se_lon = pd.Series(csv_Lon)
                se_alt = pd.Series(csv_Alt)
            
                new_dataframe = pd.DataFrame()
                new_dataframe['geotvd'] = se_alt.values
                new_dataframe['latitude'] = se_lat.values
                new_dataframe['longitude'] = se_lon.values
                connex.close()
        #write dataframe into database, table: wellmap
                dfcon = sqlite3.connect(currentwell + base)
                cursor = dfcon.cursor()
        #write csv data into database
                new_dataframe.to_sql("wellmap", con=dfcon, if_exists="append", index=False)
                dfcon.close() #close database
                self.infoLabel['text'] = "data import from csv-file"
            except:
                self.infoLabel['text'] = "some strange error occured during operation"
        
        def loadsurvey():
            if len(wellactual) == 0:
               self.infoLabel['text'] = "load a well from the Wells tab first"
            else:
                # print("from loadsurvey: ",wellactual)
                currentwell = wellactual[-1]
                #Verbinde mit Datenbank um Daten in Tree zu laden
                connection = sqlite3.connect(currentwell+base)
                cursor = connection.cursor()
                sql = "SELECT * FROM geosurvey"
                cursor.execute(sql)
                dsatz = cursor.fetchall()
                surveyTree.delete(*surveyTree.get_children())   #clear the table
                for row in dsatz:
                    surveyTree.insert("", tk.END,
                        values=(row[0],row[1],row[2],round(row[3],2),
                                round(row[4],2),round(row[5],2),round(row[6],2),round(row[7],3)))                     
                connection.close()
                #self.infoLabel['text'] = "read data from database"
            


        def writesurvey():

            def north2lat(north, latitude):
             
                lat_p = math.sqrt(latitude**2)
                new_lat = latitude + (north/(110574+(lat_p*12.44)))

                if latitude >= 0 and new_lat >= 90:
                    return 180 - new_lat

                elif latitude <=0 and new_lat <= -90:
                    return -180 - new_lat
                else:
                    return new_lat
                
            def east2lon(east, latitude, longitude):
                
                new_lon = longitude + ((east * math.cos(math.radians(latitude)))/111120)

                if new_lon >= 180:
                    return -360 + new_lon
                
                elif new_lon <= -180:
                    return 360 + new_lon

                else:
                    return new_lon
                
            depthlist = []
            inclist = []
            azilist = []
            tvdlist = []
            northlist = []
            eastlist = []

            try:
                depth = e1.get()
                inc = e2.get()
                azi = e3.get()
                d = float(depth)
                i = float(inc)
                a = float(azi)
            #okay, hier gehts los einen tab nach links
            #Laden der vorhanden Daten
                if len(wellactual) == 0:
                    self.infoLabel['text'] = "por favor selecciona un taladro"
                else:
                    currentwell = wellactual[-1]
                
                connection = sqlite3.connect(currentwell + base)
                cursor = connection.cursor()
                sql = "SELECT * FROM geosurvey"
                cursor.execute(sql)

                for dsatz in cursor:
                    depthlist.append(dsatz[0])
                    inclist.append(dsatz[1])
                    azilist.append(dsatz[2])
                    tvdlist.append(dsatz[3])
                    northlist.append(dsatz[4])
                    eastlist.append(dsatz[5])
                  
                #letzte Werte aus Datenbank
                ldepth = depthlist[-1]
                linc = inclist[-1]
                lazi = azilist[-1]
                ltvd = tvdlist[-1]
                lnorth = northlist[-1]
                least = eastlist[-1]
        
                incremDepth = d - ldepth
                incremInc = i - linc
                incremAzi = a - lazi

                ############ Berechnung mittels Minimum Curvature Method ##############
                DL = round(math.degrees(math.acos((math.sin(math.radians(linc))*math.sin(math.radians(i))*math.cos(math.radians(a-lazi)))\
                                                  + (math.cos(math.radians(linc))*math.cos(math.radians(i))))),3)
                if DL == 0:
                    DL = 1
    
                DLS = round(((DL*30)/incremDepth),3)

                RF = round((2/math.radians(DLS))*math.tan(math.radians(DLS)/2),5)

                t = round(((incremDepth/2)*(math.cos(math.radians(linc)) + math.cos(math.radians(i)))*RF),2) + ltvd # TVD

                n = round(((incremDepth/2)*((math.sin(math.radians(linc))*math.cos(math.radians(lazi)))+\
                                            (math.sin(math.radians(i))*math.cos(math.radians(a))))*RF),2) + lnorth #North

                e = round(((incremDepth/2)*((math.sin(math.radians(linc))*math.sin(math.radians(lazi)))+\
                                            (math.sin(math.radians(i))*math.sin(math.radians(a))))*RF),2) + least #East


                #closure distance
                CD = round(((n**2 + e**2)**0.5),2)
                #CD = '{:.2f}'.format(str(CD))
                #DLS = '{:.2f)'.format(str(DLS))
                #### Minimum Curvature Method Ende ###
                ### calculate wellmap data ###

                gcon = sqlite3.connect("wellnames.db")
                cursor = gcon.cursor() 
                well = currentwell
                z="'"
                cwell = z+str(currentwell)+z
                gcon = sqlite3.connect("wellnames.db")
                cursor = gcon.cursor()

                sqla = "SELECT altitude FROM wells WHERE name = " +cwell+ ""

                cursor.execute(sqla)

                for c in cursor:
                    z_surface = c[0]
                            
                gcon.close()

                connex = sqlite3.connect("wellnames.db")
                cursor = connex.cursor()
                sqllat = "SELECT latitude FROM wells WHERE name = "+cwell+""
                cursor.execute(sqllat)
                for lat in cursor:
                    lat_surface = lat[0]
                    #print(lat_surface)
                sqllon = "SELECT longitude FROM wells WHERE name = "+cwell+""
                cursor.execute(sqllon)
                for lon in cursor:
                    lon_surface = lon[0]
                #   print(lon_surface)

                geotvd = z_surface - t # Depth substracted from altitude
                
                slat = lat_surface 
                slon = lon_surface
            
                #Berechnung der geographischen Koordinaten
                north = n
                east = e
                latit = north2lat(north,slat)
                longit = east2lon(east, slat, slon,)
            
                #print("writing the data")
            except:   
                self.infoLabel['text'] = 'Please try again, check your input'
                
            try: 
                con = sqlite3.connect(currentwell + base)
                cursor = con.cursor()
                sql = "INSERT INTO geosurvey VALUES(" + str(d) + ","+str(i)+","+str(a)+","+ str(t)+","+str(n)+","+str(e)+","+str(CD)+","+str(DLS)+")"
                cursor.execute(sql)
                con.commit()
                con.close()
                self.infoLabel['text'] = "writing in progress"
                bcon = sqlite3.connect(currentwell + base)
                cursor = bcon.cursor()
        
                bsql = "INSERT INTO wellmap VALUES(" + str(geotvd) + "," + str(latit) + "," + str(longit) + ")"

                cursor.execute(bsql)
                bcon.commit()
                bcon
                self.infoLabel['text'] = "wellmap data written"       
            except:
                self.infoLabel['text'] = 'survey not written'
                
            loadsurvey() #neu laden, Tree erneut anzeigen

        def delData():
            if len(wellactual) == 0:
                self.infoLabel['text'] = "create or choose a well"
            else:
                currentwell = wellactual[-1]
                try:
                    de = deleteSurvey.get()
                    delsurvey = str(de)
    
                    #con = sqlite3.connect('geosurvey.db')
                    con = sqlite3.connect(currentwell + base)
                    cursor = con.cursor()
                    try:
                        ### get the ID
                        sql_1 = "SELECT ROWID FROM geosurvey WHERE Depth = " + delsurvey +""
                        
                        #sql ="DELETE FROM geosurvey WHERE Depth = " + delsurvey +""  
                        cursor.execute(sql_1)
                        con.commit()
                        for rowid in cursor:
                            print(rowid[0])
                        
                    except:
                        self.infoLabel['text'] = "specify the depth form which survey shall be removed"
                    con.close()
                    
                    con = sqlite3.connect(currentwell + base)
                    cursor = con.cursor()
                    sql_2 = "DELETE FROM geosurvey WHERE Depth = " + delsurvey +""  
                    cursor.execute(sql_2)
                    con.commit()
                    con.close()

                    row_to_delete = str(rowid[0])

                    con = sqlite3.connect(currentwell+base)
                    cursor = con.cursor()
                    sql_3 = "DELETE FROM wellmap WHERE ROWID = " + row_to_delete +""
                    cursor.execute(sql_3)
                    con.commit()
                    con.close()
                    self.infoLabel['text'] = "survey deleted"  
                    
                    deleteSurvey.delete(0,END)
                except:
                    self.infoLabel['text'] = "survey not removed, database error likely"

                    
        
        surveyWinFrame = tk.Frame(self.tab2)
        surveyWinFrame.pack()
        tab2Label1 = tk.Label(surveyWinFrame, text="Surveys", bg="#ffffff", pady=4, padx=12)
        tab2Label1.grid(row=0, column=0, columnspan=40, sticky=E+W)
        tab2Separator = ttk.Separator(surveyWinFrame, orient="horizontal")
        tab2Separator.grid(row=1, column=0, columnspan=40, sticky=W+E, pady=8)
        tab2BottomSeparator = ttk.Separator(surveyWinFrame, orient="horizontal")
        tab2BottomSeparator.grid(row=3, column=0, columnspan=40, sticky=W+E, pady=8)
        tab2refreshButton = tk.Button(surveyWinFrame, text="Show", command=loadsurvey)
        tab2refreshButton.grid(row=4, column=0, sticky=E+W)
        tab3writeButton = tk.Button(surveyWinFrame, text="Write", command=writesurvey)
        tab3writeButton.grid(row=4, column=1, sticky=E+W)        
        tab3importButton = tk.Button(surveyWinFrame, text="Import", command=importCsv)
        tab3importButton.grid(row=4, column=2,sticky=E+W)        
        
            #survey database table
        surveyTree = ttk.Treeview(surveyWinFrame, selectmode='browse', show='headings')
        surveyTree["columns"]=["one", "two", "three", "four", "five", "six", "seven", "eight"]
        surveyTree.column("one", width=100)
        surveyTree.column("two", width=100)
        surveyTree.column("three", width=100)
        surveyTree.column("four", width=100)
        surveyTree.column("five", width=100)
        surveyTree.column("six", width=100)
        surveyTree.column("seven", width=100)
        surveyTree.column("eight", width=100)
        surveyTree.heading("one", text="Depth")
        surveyTree.heading("two", text="Inc")
        surveyTree.heading("three", text="Azi")
        surveyTree.heading("four", text="TVD")
        surveyTree.heading("five", text="North")
        surveyTree.heading("six", text="East")
        surveyTree.heading("seven", text="Closure")
        surveyTree.heading("eight", text="DLS")
        surveyTree.grid(row=2, column=0, columnspan=38, sticky=E+W)
        wScroll2 = ttk.Scrollbar(surveyWinFrame ,command=surveyTree.yview)
        wScroll2.grid(row=2, column=39, sticky=N+S+W)
        surveyTree.configure(yscrollcommand=wScroll2.set,
                             height=12)

        pw1 = ttk.PanedWindow(surveyWinFrame, orient='horizontal')
        f1 = ttk.Labelframe(pw1, text='Add Survey', width=200, height=200)
        f2 = ttk.Labelframe(pw1, text='Delete Survey', width=200, height=200)
        #f3 = ttk.Labelframe(pw1, text='Import CSV data', width=200, height=200)
        pw1.add(f1)
        pw1.add(f2)
        #pw1.add(f3)
        pw1.grid(row=5, column=0, columnspan=40, sticky=E+W, pady=20)
        
        label1 = tk.Label(f1, text="Depth:")
        label1.pack(padx=24, pady=4)
        e1 = tk.Entry(f1, justify='center')
        e1.pack(padx=24)
        label2 = tk.Label(f1, text= "Inclination:")
        label2.pack(padx=24, pady=4)
        e2 = tk.Entry(f1, justify='center')
        e2.pack(padx=24, pady=4)
        label3 = tk.Label(f1, text="Azimuth:")
        label3.pack(padx=24, pady=4)
        e3 = tk.Entry(f1, justify='center')
        e3.pack(padx=24, pady=4)
            #e3.bind('<Return>', writesurvey) # nur mit event Parameter in Fkt

        dele_lb = tk.Label(f2, text="Survey from Depth:")
        dele_lb.pack(pady=4)
        deleteSurvey = tk.Entry(f2, justify='center')
        deleteSurvey.pack(padx=24, pady=4)
        surveyDeleteButton = tk.Button(f2, text="Delete", bg="#ff4646",
                                     command=delData, width=8)
        surveyDeleteButton.pack(pady=8)

        #csvLabel = tk.Label(f3, text="Specify source file")
        #csvEntry = tk.Entry(f3, justify='center')
        #csvLabel.pack(padx=24, pady=4)
        #csvEntry.pack(padx=24, pady=4)
        
        
        # Plot tab
        
        

    def plotWin(self):
        #print("plotWin is there")

        
        showstrat = tk.IntVar()
        showsite = tk.IntVar()
        showsite.set(1)
        setattrchwert = tk.IntVar()
        schwert = tk.IntVar()
        schwert.set(10)
        autoscale = tk.IntVar()
        autoscale.set(1)
        equalxy = tk.IntVar()
        idls = tk.IntVar()
        stratplane = tk.IntVar()
        scvp = tk.IntVar()
        scvp.set(10)
        scv_alpha = tk.StringVar()
        scv_alpha.set(0.4)
        scv_color = tk.IntVar()
        scv_color.set(8)

        def colorpick(self):
            if scv_color.get() == 0:
                farblabel["bg"] = '#0AA0AF'
            elif scv_color.get() == 2:
                farblabel["bg"] = '#AA00AF'
            elif scv_color.get() == 4:
                farblabel["bg"] = '#FFDD33'
            elif scv_color.get() == 6:
                farblabel["bg"] = '#ff0000'
            elif scv_color.get() == 8:
                farblabel["bg"] = '#00AFFE'
            else:
                farblabel["bg"] = '#090909'

        def show_site():
            
            Bohrplatz = [
                [-100,100, 0],
                [-100, -100, 0],
                [100, -100, 0],
                [100, 100, 0],
                [-100 ,100, 0],
                ]
        
        
        def show_strat():

            currentwell = wellactual[-1]
            global panstrat

            constr = sqlite3.connect(currentwell + base)

            panstrat = pd.read_sql_query("SELECT * FROM stratigraphy", constr)
            constr.close()
            
            #print("load panstrat data frame")
            #print(panstrat)

            panstrat.sort_values(by = ["Depth"]) # Im Normalfall !!

            nameFm = panstrat.Fm_Name.tolist()
            colFm = panstrat.Fm_Color.tolist()
            northFm = panstrat.Fm_North.tolist()
            eastFm = panstrat.Fm_East.tolist()
            tvdFm_pos = panstrat.Fm_TVD.tolist()

            tvdFm = []
            for value in tvdFm_pos:
                value = value * (-1)
                tvdFm.append(value)

        def plot_well():
            global panda
            
            if len(wellactual) == 0:
                self.infoLabel['text'] = "choose and load a well from database first"
            else:
                currentwell = wellactual[-1]
                 
            try:
                con = sqlite3.connect(currentwell + base)
                panda = pd.read_sql_query("SELECT * FROM geosurvey", con)
                con.close()

                panda.sort_values(by = ["Depth"]) # Im Normalfall !!

                north = panda.North.tolist()
                east = panda.East.tolist()
                tvd_positiv = panda.TVD.tolist()

                #colorplot mit DLG

                dls = panda.DLS.tolist()
                ###

                northmax = max(north)
                northmin = min(north)
                eastmax = max(east)
                eastmin = min(east)
 

                #Calculate Depth as negative values
                tvd = []
                for value in tvd_positiv:
                    value = value * (-1)
                    tvd.append(value)
            
                fig = plt.figure()
                plt.style.use('seaborn')
                ax = plt.axes(projection='3d')
            

                #color
                
                if scv_color.get() == 0:
                     line_color = '#0AA0AF'
                elif scv_color.get() == 2:
                    line_color = '#AA00AF'
                elif scv_color.get() == 4:
                    line_color = '#FFDD33'
                elif scv_color.get() == 6:
                    line_color = '#ff0000'
                elif scv_color.get() == 8:
                    line_color = '#00AFFE'
                else:
                      line_color = '#090909'
            
                ###
                if idls.get() == 1:
                    ax.scatter(east, north, tvd, c=dls, s=42,cmap='plasma', label=currentwell)
                    ax.plot(east,north,tvd,color='#010101', linewidth=1.2, label=currentwell)
                else:
                    ax.plot(east,north,tvd,color=line_color, linewidth=1.6, label=currentwell)
                
                ax.tick_params(labelsize=6)

                #Limits from schwert variables
                if equalxy.get() == 1:
            
                    if eastmin <= northmin:
                        eastmin = northmin
                    elif northmin <= eastmin:
                        eastmin = northmin

                    if eastmax >= northmax:
                        northmax = eastmax
                    elif northmax >= eastmax:
                        eastmax = northmax

                if autoscale.get() == 1:  
                    ax.set_xlim(eastmin-100,eastmax+100)
                    ax.set_ylim(northmin-100,northmax+100)

                else:
                    xlim = int(schwert.get())
                    ylim = int(schwert.get())
                    xlimneg = -1*int(schwert.get())
                    ylimneg = -1*int(schwert.get())
                    ax.set_xlim(xlimneg,xlim)
                    ax.set_ylim(ylimneg,ylim)
            
            
                ax.set_xlabel('Easting', fontsize=7)
                ax.set_ylabel('Northing', fontsize=7)
                ax.set_zlabel('Depth TVD[m]', fontsize=7)
                #plt.title('Well Visualisation', fontsize=12)
            
                #Add polycollection elements
                if showsite.get() == 1:
                    Bohrplatz = [
                    
                        [-30,30, 0],
                        [-30, -30, 0],
                        [30, -30, 0],
                        [30, 30, 0],
                        [-30 ,30, 0],
                        ]
                    layer4 = Poly3DCollection([Bohrplatz],alpha =0.4)
                    face_color = [0.2, 0.1, 0.2]
                    layer4.set_facecolor(face_color)
                    ax.add_collection3d(layer4)

                if showstrat.get() == 1 and stratplane.get() == 0:
                    global panstrat

                    constr = sqlite3.connect(currentwell + base)

                    panstrat = pd.read_sql_query("SELECT * FROM stratigraphy", constr)
                    constr.close()

                    panstrat.sort_values(by = ["Depth"]) # Im Normalfall !!

                    nameFm = panstrat.Fm_Name.tolist()
                    colFm = panstrat.Fm_Color.tolist()
                    northFm = panstrat.Fm_North.tolist()
                    eastFm = panstrat.Fm_East.tolist()
                    tvdFm_pos = panstrat.Fm_TVD.tolist()
 
                    tvdFm = []
                    for value in tvdFm_pos:
                        value = value * (-1)
                        tvdFm.append(value)

                    for i in range (len(tvdFm)):
                        ax.scatter3D(eastFm[i], northFm[i], tvdFm[i],\
                                     c=colFm[i], marker='o', s=24)
                    
                if stratplane.get() == 1:

                    global panTop

                    constr = sqlite3.connect(currentwell + base)

                    panTop = pd.read_sql_query("SELECT * FROM stratigraphy", constr)
                    constr.close()
            
                    #print("load panstrat data frame")
                    #print(panTop)

                    panTop.sort_values(by = ["Depth"]) # Im Normalfall !!

                    nameFm = panTop.Fm_Name.tolist()
                    colFm = panTop.Fm_Color.tolist()
                    northFm = panTop.Fm_North.tolist()
                    eastFm = panTop.Fm_East.tolist()
                    tvdFm_pos = panTop.Fm_TVD.tolist()
                    psize = int(scvp.get()) 
                    palpha = str(scv_alpha.get())
                    #print(palpha)

                    tvdFm = []
                    for value in tvdFm_pos:
                        value = value * (-1)
                        tvdFm.append(value)

                    for i in range (len(tvdFm)):

                        nameFm[i] = [
                            [eastFm[i]-psize,northFm[i]+psize,tvdFm[i]],
                            [eastFm[i]-psize,northFm[i]-psize,tvdFm[i]],
                            [eastFm[i]+psize,northFm[i]-psize,tvdFm[i]],
                            [eastFm[i]+psize,northFm[i]+psize,tvdFm[i]],
                            [eastFm[i]-psize,northFm[i]+psize,tvdFm[i]],
                            ]
                 
                        nameFm[i] = Poly3DCollection([nameFm[i]],alpha=palpha)
                        face_color = [colFm[i]]
                        nameFm[i].set_facecolor(face_color)
                        ax.add_collection3d(nameFm[i])
                

                plt.legend(fontsize=7)
                plt.show()
            except:
                self.infoLabel['text'] = "please select a well from database"

        #print("and now?")
        plotWinFrame = tk.Frame(self.tab3)
        plotWinFrame.pack()
        plotLabel1 = tk.Label(plotWinFrame, text="3D Plot of Single Well", bg="#ffffff", pady=4, padx=12)
        plotLabel1.grid(row=0, column=0, columnspan=6, sticky=E+W)
        plotSeparator = ttk.Separator(plotWinFrame, orient="horizontal")
        plotSeparator.grid(row=1, column=0, columnspan=6, sticky=W+E, pady=8)
        plotLabel2 = tk.Label(plotWinFrame, text="Show Elements")
        plotLabel2.grid(row=3, column=0)
        
        cb1 = tk.Checkbutton(plotWinFrame, text="Wellsite", variable=showsite,\
                                       #onvalue="Plot Stratigraphy", offvalue="Hide Stratigraphy",\
                                      #command=show_strat,
                                  padx=4, pady=4)
        cb1.grid(row=5, column=0, sticky=W)
        cb2 = tk.Checkbutton(plotWinFrame, text="Formations", variable=showstrat,\
                                  #onvalue="Plot Stratigraphy", offvalue="Hide Stratigraphy",\
                                  #command=show_strat,
                                  padx=4, pady=4)
        cb2.grid(row=6, column=0, sticky=W)

        cd3 = tk.Checkbutton(plotWinFrame, text="Formations as Planes", variable=stratplane,\
                                  padx=4, pady=4)
        cd3.grid(row=7, column=0, sticky=W)

        cb4 = tk.Checkbutton(plotWinFrame, text="Autoscale", variable=autoscale,\
                                  #onvalue="Plot Stratigraphy", offvalue="Hide Stratigraphy",\
                                  #command=show_strat,
                                    padx=4, pady=4)
        cb4.grid(row=8, column=0, sticky=W)
        cb6 = tk.Checkbutton(plotWinFrame, text="Equal xy-Scale", variable=equalxy,\
                                    padx=4, pady=4)
        cb6.grid(row=9, column=0, sticky=W)

        cb7 = tk.Checkbutton(plotWinFrame, text="Indicate Dogleg Severity", variable=idls,\
                                  padx=4, pady=4)
        cb7.grid(row=10, column=0, sticky=W)


        scv = tk.Scale(plotWinFrame, width=20, length=320,
                       orient='horizontal', from_=0, to= 2000,
                       resolution=10, tickinterval=400, label='Axis Scaling',
                       variable=schwert, showvalue=False)
        scv.grid(row=3, column=5, rowspan=3, padx=24, sticky=E)

        scv2 = tk.Scale(plotWinFrame, width=20, length=320,
                        orient='horizontal', from_=20, to=120,
                        resolution=10, tickinterval=20, label='Plane Size',
                        variable=scvp, showvalue=False)
        scv2.grid(row=6, column=5, rowspan=3, padx=24, sticky=E)

        scv3 = tk.Scale(plotWinFrame, width=20, length=320,
                        orient='horizontal', from_=0, to=1,
                        resolution=0.1, tickinterval=0.2, label='Alpha Level',
                        variable=scv_alpha, showvalue=False)
        scv3.grid(row=9, column=5, rowspan=3, padx=24, sticky=E)
        ### color slider
        scv4 = tk.Scale(plotWinFrame, width=20, length=320,
                        orient='horizontal', from_=0, to=10,
                        resolution=2, tickinterval=2, label='Color Slider',
                        variable=scv_color, command=colorpick, showvalue=False)
        scv4.grid(row=12, column=5, rowspan=3, padx=24, sticky=E)
        
        vertsep = ttk.Separator(plotWinFrame, orient="vertical")
        vertsep.grid(row=3, column=3, rowspan=14, sticky=N+S, padx=24)
        
        plotSeparator3 = ttk.Separator(plotWinFrame, orient="horizontal")
        plotSeparator3.grid(row=18, column=0, columnspan=6, sticky=W+E, pady=8)
        
        plotButton = tk.Button(plotWinFrame, text="3D Plot", padx=50, command=plot_well)
        plotButton.grid(row=19, column=0, columnspan=6)

        farblabel = tk.Label(plotWinFrame, bg='#00AFFE')
        farblabel.grid(row=17, column=5, columnspan=2, sticky=W+E, pady=0)


        #wellmap window
    
    def wellmapWin(self):

        mapstrat = tkinter.IntVar()
        showDEM = tkinter.IntVar()
        showPolygon = tkinter.IntVar()
        showLegend = tkinter.IntVar()
        
        def load_choice():

            del well_objects[:(len(well_objects))]
            
            for x in wellsToChoose.curselection():
                k = (wellsToChoose.get(x)[0])
                k=wellmap(k,0,0,0)
                well_objects.append(k)
                
        def show_objects():
            model = plt.figure()
            #plt.style.use('dark_background')
            plt.style.use('fivethirtyeight')
            ax = plt.axes(projection='3d')
            
            for y in well_objects:
                y.tvdtolist()
                y.lattolist()
                y.lontolist()
                labels=y.getName()
                ax.plot(y.lonlist, y.latlist, y.tvdlist, color='#ff0000', linewidth=1.4, label=labels)

                #TODO Bohrplatz
                
                Bohrplatz = [
                    [y.lonlist[0]-0.0005, y.latlist[0]+0.0005, y.tvdlist[0]],
                    [y.lonlist[0]-0.0005, y.latlist[0]-0.0005, y.tvdlist[0]],
                    [y.lonlist[0]+0.0005, y.latlist[0]-0.0005, y.tvdlist[0]],
                    [y.lonlist[0]+0.0005, y.latlist[0]+0.0005, y.tvdlist[0]],
                    [y.lonlist[0]-0.0005 ,y.latlist[0]+0.0005, y.tvdlist[0]],
                    ]
                layer_y = Poly3DCollection([Bohrplatz],alpha =0.2)
                face_color = [0.2, 0.1, 0.4]
                layer_y.set_facecolor(face_color)
                ax.add_collection3d(layer_y)

                if showLegend.get()==1:
                    ax.legend(fontsize=7)
                else:
                    continue
            #

            if mapstrat.get()==1:
                for y in well_objects:
                    y.strattolist()                    
                    for i in range (len(y.strat_altilist)):
                        ax.scatter3D(y.strat_lonlist[i], y.strat_latlist[i], y.strat_altilist[i],\
                                     c=y.strat_colorlist[i], marker='o', s=28)
                    
            ### TODO: limits of axes
            ax.tick_params(labelsize=6)
            ax.set_xlabel('Longitude', fontsize=7)
            ax.set_ylabel('Latitude', fontsize=7)
            ax.set_zlabel('Depth [m]', fontsize=7)

        # surface data from polygons (attribute, nodes)
            if showPolygon.get()==1:

                ### get file names and load
                attributes_file = attributesFile
                nodes_file = nodesFile
                attribute_names =["shapeid","ID","HOEHE"]
                node_names = ['shapeid','x','y']
                
                try:
                    panda_att = pd.read_csv(attributes_file, header=0,
                                            names=attribute_names)
                    

                    panda_nod = pd.read_csv(nodes_file, header=0,
                                          names=node_names)
                    
                    z_att = panda_att.HOEHE.tolist()
                    id_att = panda_att.shapeid.tolist()
                    x = panda_nod.x.tolist()
                    y = panda_nod.y.tolist()
                    id_nod = panda_nod.shapeid.tolist()

                    dict_att = dict(zip(id_att, z_att))
                    new_id = []
                    new_z = []

                    for i in range (len(id_nod)):
                        if id_nod[i] in id_att:
                            new_id.append(id_nod[i])

                    keys = dict_att.keys()

                    # Elemente der Liste mit dict abgleichen
                    #und value des Dict ausgeben und an Liste hängen
                    for i in id_nod:
                        if i in keys:
                            j = dict_att.get(i)
                            new_z.append(j)
                except:
                    self.infoLabel['text'] = "no nodes / attributes available"

                try:
                    #set up 2D grid
                    xi = np.linspace(min(x), max(x))
                    yi = np.linspace(min(y), max(y))
                    X, Y = np.meshgrid(xi,yi)
                    
                    # neu mit scipy
                    Z = griddata((x, y), new_z, (xi[None,:], yi[:,None]), method='linear')

                    ax.plot_surface(X,Y,Z, rstride=1,
                                    cstride=1, alpha=0.6,
                                    linewidth=1, antialiased=True)
                except:
                    self.infoLabel['text'] = "topography from nodes / attributes not found"

        
            # surface data from xyz-csv files
            
            if showDEM.get() == 1:
                 ### get files and load
                xyz_file = xyzFile
                xyz_header = ['Field_1', 'Field_2', 'Field_3']

                try:
                    xyz_dataframe = pd.read_csv(xyz_file, names=xyz_header)                    
                    x = xyz_dataframe['Field_1'].tolist()
                    y = xyz_dataframe['Field_2'].tolist()
                    z = xyz_dataframe['Field_3'].tolist()

                    xi = np.linspace(min(x), max(x))
                    yi = np.linspace(min(y), max(y))
                    X, Y = np.meshgrid(xi, yi)
                    
                    Z = griddata((x, y), z, (xi[None,:], yi[:,None]), method='linear')
                    
                    scat = ax.scatter(x,y,z, c="#aaaaaa", s=9)
                    #surf = ax.plot_surface(X, Y, Z, rstride=2, cstride=2, cmap=cm.terrain,
                                           #antialiased=True, linewidth=0)
                    cs = plt.tricontourf(x, y, z, 100, cmap='terrain', alpha=0.3)
                    plt.colorbar(cs, aspect= 5, shrink=0.5)
                    
                except:
                    self.infoLabel("xyz-file error")

            plt.show()

            
        def fileDialogXYZ():
            Tk().withdraw()
            global xyzFile
            xyzFile = askopenfilename()

        def fileDialogNodes():
            Tk().withdraw()
            global nodesFile
            nodesFile = askopenfilename()

        def fileDialogAttributes():
            Tk().withdraw()
            global attributesFile
            attributesFile = askopenfilename()
            
        #load database wellnamelist - scroll list to choose for the map
        mapcon = sqlite3.connect("wellnames.db")
        cursor = mapcon.cursor()
        msql = "SELECT Name FROM wells"
        cursor.execute(msql)
        for wn in cursor:
            wellnamelist.append(wn)
    
        mapcon.close()

        wellmapFrame = tk.Frame(self.tab4)
        wellmapFrame.pack()
        tab4Label1 = tk.Label(wellmapFrame, text="Plot Multiple Wells on a Map", bg="#ffffff")
        tab4Label1.grid(row=0, column=0, columnspan=8, sticky=E+W)
        tab4Separator = ttk.Separator(wellmapFrame, orient="horizontal")
        tab4Separator.grid(row=1, column=0, columnspan=8, sticky=W+E, pady=8)
        tab4BottomSeparator = ttk.Separator(wellmapFrame, orient="horizontal")
        tab4BottomSeparator.grid(row=12, column=0, columnspan=8, sticky=W+E, pady=8)
        wellmapBt1 = tk.Button(wellmapFrame, text="Load Wells", command=load_choice, relief='groove')
        wellmapBt1.grid(row=13, column=0,columnspan=2, sticky=W+E)
        wellmapBt2 = tk.Button(wellmapFrame, text="Plot Map", command=show_objects, relief='groove')
        wellmapBt2.grid(row=13, column=2, sticky=W+E)
        wellmapChk1 = tk.Checkbutton(wellmapFrame, text="Include stratigraphy", variable=mapstrat)
        wellmapChk1.grid(row=3, column=5, columnspan=3, sticky=W)
        DEMcheckbox = tk.Checkbutton(wellmapFrame, text="Plot xyz-data",
                                          variable=showDEM)
        DEMcheckbox.grid(row=4, column=5, columnspan=3, sticky=W)
        
        polygonCheckbox = tk.Checkbutton(wellmapFrame, text="Plot Polygon-data",
                                          variable=showPolygon)
        polygonCheckbox.grid(row=5, column=5, columnspan=3, sticky=W)

        legendCheckbox = tk.Checkbutton(wellmapFrame, text="Show Legend",
                                          variable=showLegend)
        legendCheckbox.grid(row=6, column=5, columnspan=3, sticky=W)
        
        addGeometryLabel = tk.Label(wellmapFrame, text="xyz-Point Geometry Data",
                      pady=8, padx=32, bg="#fff5e6")
        addGeometryLabel.grid(row=14, column=0, columnspan=8, sticky=E+W, pady=4)

        searchXyzLabel = tk.Label(wellmapFrame, text="Add xyz-Data", bg="#fefefe")
        searchXyzLabel.grid(row=15, column=0, columnspan=4, sticky=E+W)

        xyzButton = tk.Button(wellmapFrame, text="Open File", command=fileDialogXYZ)
        xyzButton.grid(row=15, column=5)

        searchPolygonLabel = tk.Label(wellmapFrame, text="Polygon Data", bg="#fff5e6", pady=8)
        searchPolygonLabel.grid(row=16, column=0, columnspan=8, sticky=E+W, pady=4)
        
        nodeLabel = tk.Label(wellmapFrame, text="Add nodes", bg="#fefefe")
        nodeLabel.grid(row=17, column=0, columnspan=4, sticky=W+E, pady=4)
        nodeButton = tk.Button(wellmapFrame, text="Open File", command=fileDialogNodes)
        nodeButton.grid(row=17, column=5)
        attributeLabel = tk.Label(wellmapFrame, text="Add Attributes", bg="#fefefe")
        attributeLabel.grid(row=18, column=0, columnspan=4, sticky=W+E, pady=4)
        attributeButton = tk.Button(wellmapFrame, text="Open File", command=fileDialogAttributes)
        attributeButton.grid(row=18, column=5)
        
        selectLabel = tk.Label(wellmapFrame, text="Select Wells")
        selectLabel.grid(row=2, column=0)
        scbChoose = tk.Scrollbar(wellmapFrame, orient="vertical")
        wellsToChoose = tk.Listbox(wellmapFrame, height=16, width=32, yscrollcommand=scbChoose.set,\
                                   selectmode="multiple")
        scbChoose["command"] = wellsToChoose.yview
            #welllist ????
        welllist = wellnamelist

        for wellmap_well in welllist:    
            wellsToChoose.insert("end", wellmap_well)
        wellsToChoose.grid(row=3, column=0, columnspan=4, rowspan=8, sticky=W+E+N+S)
        scbChoose.grid(row=3, column=4, rowspan=9, sticky=W+N+S)

        #print("and wellmap?")
    # stratigraphy - tab 5


    def stratWin(self):

        def interpolate():
            
            try:
                fm_dep = stratentry2.get()
                fm_depth = str(float(fm_dep))
                
            
                currentwell = wellactual[-1]
                global pandastrat
                con = sqlite3.connect(currentwell+base)
                panda_fm_min = pd.read_sql_query("SELECT * FROM geosurvey WHERE DEPTH <= " + fm_depth + "", con)
                panda_fm_max = pd.read_sql_query("SELECT * FROM geosurvey WHERE DEPTH >= "+fm_depth+"", con)
                con.close()
                self.infoLabel['text'] = "interpolate formation top"
                panda_fm_min.sort_values(by = ["Depth"])
                panda_fm_max.sort_values(by = ["Depth"])
            
                up_survey = panda_fm_min.tail(1)
                down_survey= panda_fm_max.head(1)
           
                #panda dataframe entpacken us: upper survey, ds: lower survey
                us_depth = up_survey.Depth.tolist()
                ls_depth = down_survey.Depth.tolist()
                us_inc = up_survey.Inc.tolist()
                ls_inc = down_survey.Inc.tolist()
                us_azi = up_survey.Azi.tolist()
                ls_azi = down_survey.Azi.tolist()
                us_tvd = up_survey.TVD.tolist()
                ls_tvd = down_survey.TVD.tolist()
                us_north = up_survey.North.tolist()
                ls_north = down_survey.North.tolist()
                us_east = up_survey.East.tolist()
                ls_east = down_survey.East.tolist()
                try:
                    deltaMd_survey = ls_depth[0] - us_depth[0]
                    depth_us_fm = float(fm_dep) - us_depth[0]
                except:
                    self.infoLabel['text'] = "Depth is out of range, value may not be lower than lowermost survey"
            
                if depth_us_fm == 0:    # incept Null division error
                    deltaIncStep = (((ls_inc[0] -  us_inc[0])**2)**0.5)
                else:
                    deltaIncStep = (((ls_inc[0] -  us_inc[0])**2)**0.5)/deltaMd_survey

                if deltaMd_survey == 0:
                    deltaMd_survey = 1
            
                # case build or drop inclination
                if ls_inc[0] >= us_inc[0]:
                    incFm = us_inc[0] + (deltaIncStep*depth_us_fm)
                else: incFm = us_inc[0] - (deltaIncStep*depth_us_fm)

                # Azi Fm
                aziFmi = round(((math.sqrt((float(us_azi[0])-float(ls_azi[0]))**2)/deltaMd_survey) * \
                               depth_us_fm),2)
                if float(us_azi[0]) <= float(ls_azi[0]) :
                    aziFm = round((float(us_azi[0]) + aziFmi),2)
                else:
                    aziFm = round((float(us_azi[0]) - aziFmi),2)
                
                # TVD, Northing, Easting - Formation

                tvdFm = round(((depth_us_fm* math.cos(math.radians(incFm))) + us_tvd[0]),2)
            

                north_Fm = round(((depth_us_fm*math.sin(math.radians((us_inc[0] + incFm)/2))*\
                                   math.cos(math.radians(((us_azi[0] + aziFm)/2))))+us_north[0]),2)
            
                east_Fm = round(((depth_us_fm*math.sin(math.radians((us_inc[0] + incFm)/2))*\
                                   math.sin(math.radians(((us_azi[0] + aziFm)/2))))+us_east[0]),2)

                self.infoLabel['text'] = tvdFm, north_Fm, east_Fm
            except:
                self.infoLabel['text'] = "please check the input"

        def deleteStratigraphy():

                if len(wellactual) == 0:
                    self.infoLabel['text'] = "choose a well from database first"
                else:
                    currentwell = wellactual[-1]
                    try:
                
                        xd = stratentry2.get()
                        xdepth = str(xd)
                    
                        delcon = sqlite3.connect(currentwell + base)
                        cursor = delcon.cursor()
                        #Abfrage
                        delFm_sql = "DELETE FROM stratigraphy WHERE Depth = " + xdepth + ""
                        cursor.execute(delFm_sql)
                        delcon.commit()
                        delcon.close()
                        stratentry2.delete(0,END)
                        self.infoLabel['text'] = "formation top deleted from list"
                    except:
                        self.infoLabel['text'] = "to be deleted from which depth?"

        def addstrat():
            
            if len(wellactual) == 0:
                self.infoLabel['text'] = "choose a well first"
            else:
                currentwell = wellactual[-1]

            def north2lat(north, latitude):
                lat_p = math.sqrt(latitude**2)
                new_lat = latitude + (north/(110574+(lat_p*12.44)))

                if latitude >= 0 and new_lat >= 90:
                    return 180 - new_lat

                elif latitude <=0 and new_lat <= -90:
                    return -180 - new_lat
                else:
                    return new_lat
                
                
            def east2lon(east, latitude, longitude):
                new_lon = longitude + ((east * math.cos(math.radians(latitude)))/111120)

                if new_lon >= 180:
                    return -360 + new_lon
                
                elif new_lon <= -180:
                    return 360 + new_lon

                else:
                    return new_lon
                

            # Calculation of Formation-Top after Averange-Angle Method
            try:
                for formation in li.curselection():
                        F  =  li.get(formation)
                        #FN = F[0]
                        #HexColor = F[1]
                    
                fm_nam = str(F)
                print(fm_nam)
                fm_nam.replace('"', '')
                z="'"
                hcol = z + str(fm_nam) + z

                ### get colorcode:
                con = sqlite3.connect('geocolors.db')
                cursor = con.cursor()
                sql = "SELECT hexcolor FROM stratcolor WHERE unit = "+hcol+""
                print(sql)
                cursor.execute(sql)
                for color in cursor:
                    HexColor = color[0]
                con.close()
                print(HexColor)

                
                hexcol = str(HexColor)

                fm_dep = stratentry2.get()
            
                fm_col = hexcol
                # Alter to write database
                z="'"
                Fm_Name = z+str(fm_nam)+z
                Fm_Depth = str(fm_dep)
                Fm_Color = z+str(fm_col)+z
            
                try:
                    fm_depth = str(float(fm_dep))
                except:
                    self.infoLabel['text'] = "please check the input"

                global pandastrat
                con = sqlite3.connect(currentwell+base)
                panda_fm_min = pd.read_sql_query("SELECT * FROM geosurvey WHERE DEPTH <= " + fm_depth + "", con)
                panda_fm_max = pd.read_sql_query("SELECT * FROM geosurvey WHERE DEPTH >= "+fm_depth+"", con)
                con.close()
            
                panda_fm_min.sort_values(by = ["Depth"])
                panda_fm_max.sort_values(by = ["Depth"])
                up_survey = panda_fm_min.tail(1)
                down_survey= panda_fm_max.head(1)

            ### get the geographic data to calculate lat/lon/alt from formation tops
                constrat = sqlite3.connect(currentwell+base)
                cursor = constrat.cursor()
                getdata = "SELECT Fm_alt, Fm_lat, Fm_lon FROM stratigraphy WHERE Depth=0"
                cursor.execute(getdata)
                for cu in cursor:
                    surface_alt = cu[0]
                    surface_lat = cu[1]
                    surface_lon = cu[2]
                constrat.close()

                #panda dataframe entpacken us: upper survey, ds: lower survey
                us_depth = up_survey.Depth.tolist()
                ls_depth = down_survey.Depth.tolist()
                us_inc = up_survey.Inc.tolist()
                ls_inc = down_survey.Inc.tolist()
                us_azi = up_survey.Azi.tolist()
                ls_azi = down_survey.Azi.tolist()
                us_tvd = up_survey.TVD.tolist()
                ls_tvd = down_survey.TVD.tolist()
                us_north = up_survey.North.tolist()
                ls_north = down_survey.North.tolist()
                us_east = up_survey.East.tolist()
                ls_east = down_survey.East.tolist()

                deltaMd_survey = ls_depth[0] - us_depth[0]
                depth_us_fm = float(fm_dep) - us_depth[0]
            
                if depth_us_fm == 0:    # incept Null division error
                    deltaIncStep = (((ls_inc[0] -  us_inc[0])**2)**0.5)
                else:
                    deltaIncStep = (((ls_inc[0] -  us_inc[0])**2)**0.5)/deltaMd_survey
                
            # case build or drop inclination
                if ls_inc[0] >= us_inc[0]:
                    incFm = us_inc[0] + (deltaIncStep*depth_us_fm)
                else:
                    incFm = us_inc[0] - (deltaIncStep*depth_us_fm)

            # Azi Fm
                if deltaMd_survey == 0:
                    deltaMd_survey = 1
                
                aziFmi = round(((math.sqrt((float(us_azi[0])-float(ls_azi[0]))**2)/deltaMd_survey) * \
                               depth_us_fm),2)
                if float(us_azi[0]) <= float(ls_azi[0]) :
                    aziFm = round((float(us_azi[0]) + aziFmi),2)
                else:
                    aziFm = round((float(us_azi[0]) - aziFmi),2)
            # TVD, Northing, Easting - Formation

                tvdFm = round(((depth_us_fm* math.cos(math.radians(incFm))) + us_tvd[0]),2)
         
                self.infoLabel['text'] = tvdFm

                north_Fm = round(((depth_us_fm*math.sin(math.radians((us_inc[0] + incFm)/2))*\
                                   math.cos(math.radians(((us_azi[0] + aziFm)/2))))+us_north[0]),2)
            
                east_Fm = round(((depth_us_fm*math.sin(math.radians((us_inc[0] + incFm)/2))*\
                                   math.sin(math.radians(((us_azi[0] + aziFm)/2))))+us_east[0]),2)


                Fm_TVD  = str(tvdFm)
                Fm_Inc = str(incFm)
                Fm_Azi = str(aziFm)
                Fm_North = str(north_Fm)
                Fm_East = str(east_Fm)

                latFm = north2lat(north_Fm,surface_lat)
                lonFm = east2lon(east_Fm, surface_lat, surface_lon)
                altFm = surface_alt - tvdFm 

                Fm_alt = str(altFm)
                Fm_lat = str(latFm)
                Fm_lon = str(lonFm)
                # Ende Berechnung Fm-Top

                #Write into database
                connex = sqlite3.connect(currentwell + base)
                cursor = connex.cursor()
                sql = "INSERT INTO stratigraphy VALUES(" + Fm_Depth + "," + Fm_Name + "," + Fm_Color + ","\
                      + Fm_TVD + "," + Fm_Inc + "," + Fm_Azi + "," + Fm_North + "," + Fm_East + ","\
                      + Fm_alt + "," + Fm_lat + "," + Fm_lon + ")"
            
                cursor.execute(sql)
                connex.commit()
                connex.close()
            except:
                self.infoLabel['text'] = "load well, check your input or pick a stratigraphic unit from the list"

        def viewstrat():

            def del_strat():
                
                def delfm():
                    try:
                        xd = stratentry2.get()
                        xdepth = str(xd)
                    
                        delcon = sqlite3.connect(currentwell + base)
                        cursor = delcon.cursor()
                        #Abfrage
                        delFm_sql = "DELETE FROM stratigraphy WHERE Depth = " + xdepth + ""
                        cursor.execute(delFm_sql)
                        delcon.commit()
                        delcon.close()
                        stratentry2.delete(0,END)
                        #self.delwin.quit()
                    except:
                        self.infoLabel['text'] = "from which depth delete?"
            
                
        def update():
            if len(wellactual) == 0:
                self.infoLabel['text'] = "Create or Select Well from Database"
            else:
                currentwell = wellactual[-1]

                #Verbinde mit Datenbank
                connection = sqlite3.connect(currentwell+base)
                cursor = connection.cursor()
                sql = "SELECT * FROM stratigraphy"
                cursor.execute(sql)
                dsatz = cursor.fetchall()
                tab5Tree.delete(*tab5Tree.get_children())   #clear the table
                for row in dsatz:
                    tab5Tree.insert("", tk.END,
                            values=row)
                connection.close()

        
        stratWinFrame = tk.Frame(self.tab5)
        stratWinFrame.pack()
        tab5Label1 = tk.Label(stratWinFrame, text="Formations", bg="#ffffff", pady=4, padx=12)
        tab5Label1.grid(row=0, column=0, columnspan=20, sticky=E+W)
        tab5Separator = ttk.Separator(stratWinFrame, orient="horizontal")
        tab5Separator.grid(row=1, column=0, columnspan=20, sticky=W+E, pady=8)

        tab5BottomSeparator = ttk.Separator(stratWinFrame, orient="horizontal")
        tab5BottomSeparator.grid(row=3, column=0, columnspan=20, sticky=W+E, pady=8)
            ###
        formbutton = tk.Button(stratWinFrame, text="Interpolate",\
                                command=interpolate)
        formbutton.grid(row=4, column=3, sticky=W+E)

        write_fm = tk.Button(stratWinFrame, text="Write",\
                              command=addstrat)
        write_fm.grid(row=4, column=1, sticky=W+E)

        deleteStratigraphyButton = tk.Button(stratWinFrame, text="Delete", command=deleteStratigraphy)
        deleteStratigraphyButton.grid(row=4, column=2, sticky=W+E)

        view_fm = tk.Button(stratWinFrame, text="Show", \
                                 command=update)
        view_fm.grid(row=4, column=0, sticky=W+E)

        tab5Tree = ttk.Treeview(stratWinFrame, selectmode="browse",
                                column=("one", "two","three", "four"),
                                show="headings")
        tab5Tree.heading("one", text="Depth [m]")
        tab5Tree.heading("two", text="Unit")
        tab5Tree.heading("three", text="Colorcode")
        tab5Tree.heading("four", text="TVD [m]")
        tab5Tree.grid(row=2,column=0, columnspan=20, sticky=W+E)
            #Scrollbar
        wScroll = ttk.Scrollbar(stratWinFrame ,command=tab5Tree.yview)
        wScroll.grid(row=2, column=19, sticky=N+S+E)
        tab5Tree.configure(yscrollcommand=wScroll.set,
                           height=10)
            #open database
        ###
        lb2 = tk.Label(stratWinFrame, text="New Formation-Top [m]:")
        lb2.grid(row=5, column=4, columnspan=19, sticky=W, pady=6)
        stratentry2 = tk.Entry(stratWinFrame, justify='center')
        stratentry2.grid(row=6, column=4, columnspan=19, sticky=W+E+N, pady=6)
 
        lb = tk.Label(stratWinFrame, text="Chronostratigraphy:")
        lb.grid(row=5, column=0, columnspan=4, sticky=W, pady=8)
        #Scrollbar
        scb = tk.Scrollbar(stratWinFrame, orient="vertical")

        #Listbox
        li = tk.Listbox(stratWinFrame, height=12, yscrollcommand=scb.set)
        scb["command"] = li.yview

        formations = []
        
        listbox_connect = sqlite3.connect('geocolors.db')
        listbox_cursor = listbox_connect.cursor()
        listbox_sql = "SELECT unit FROM stratcolor"
        listbox_cursor.execute(listbox_sql)
        for unit in listbox_cursor:
            formations.append(unit[0])

        listbox_connect.close()
        
        for formation in formations:
            li.insert("end", formation)

        li.grid(row=6, column=0, columnspan=3, sticky=W+E, pady=6)
        scb.grid(row=6, column=3, sticky=N+S+W, pady=6)

        


if __name__ == '__main__':
    firstStart()
    root = Tk()
    root.resizable(width=False, height=False)   #unveränderbares Fenster
    myApplication(root)
    mainloop()

print("hasta proxima vez")
