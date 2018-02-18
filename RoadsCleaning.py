import processing
import os, sys, glob

from qgis.core import *
from qgis.gui import QgsMapCanvas
from PyQt4.QtGui import *

stateslist = [\
'AF', 'AO', 'BD', 'BI', 'CD', 'CO', 'DZ',  'EH', 'ET', 'ID', 'IL', 'IN', 'IQ', 'IR', 'KH', 'LA', 'LB', 'LK', 'LR', 'LY', 'MA', 'MM', 'MZ', 'NP', 'PE', 'PH',  'PK', 'RU', 'RW', 'SD', 'SL', 'SN', 'SO', 'SS', 'TD', 'TH', 'TJ', 'TL', 'TR', 'UG', 'YE'
]

in_path = 'C:/Users/Jesse/Dropbox/RoadNetworks/Data/Roads/'
#in_path = '/media/jesse/Files/Dropbox/RoadNetworks/Data/Roads/'
out_path = 'C:/Users/Jesse/Dropbox/RoadNetworks/Data/Roads/'
#out_path = '/media/jesse/Files/Dropbox/RoadNetworks/Data/Roads/'

for state in stateslist:
    roads_shp_path = in_path + state + '_roads.shp'
    roads_out_path = out_path + state + '_roads_cleaned.shp'
    
        # Reproject to Lambert AEA
    ret = processing.runalg("qgis:reprojectlayer",roads_shp_path,"+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs",None)
    output = ret['OUTPUT']

    extent = QgsVectorLayer( output, '', 'ogr' ).extent()
    xmin = extent.xMinimum()
    xmax = extent.xMaximum()
    ymin = extent.yMinimum()
    ymax = extent.yMaximum()

    # Break vector lines
    ret = processing.runalg("grass7:v.clean",output,0,0.1,"%f, %f, %f, %f"%(xmin, xmax,ymin,ymax),-1,0.0001,None,None)
    output = ret['output']

    # Snap geometry, 100m threshold
    ret = processing.runalg("grass7:v.clean",output,1,100,"%f, %f, %f, %f"%(xmin, xmax,ymin,ymax),-1,0.0001,None,None)
    output = ret['output']
    
    #Break vector lines
    ret = processing.runalg("grass7:v.clean",output,0,0.1,"%f, %f, %f, %f"%(xmin, xmax,ymin,ymax),-1,0.0001,None,None)
    output = ret['output']
    
    # De-duplicate line sections
    ret = processing.runalg("grass7:v.clean",output,6,0.1,"%f, %f, %f, %f"%(xmin, xmax,ymin,ymax),-1,0.0001,None,None)
    output = ret['output']
    
    # Drop dangling lines less than 1000m
    ret = processing.runalg("grass7:v.clean",output,2,1000,"%f, %f, %f, %f"%(xmin, xmax,ymin,ymax),-1,0.0001,None,None)
    output = ret['output']
    
    # Prune lines, 500m threshold
    ret =processing.runalg("grass7:v.clean",output,9,500,"%f, %f, %f, %f"%(xmin, xmax,ymin,ymax),-1,0.0001,None,None)
    output = ret['output']
    
    # Break vector lines
    ret = processing.runalg("grass7:v.clean",output,0,0.1,"%f, %f, %f, %f"%(xmin, xmax,ymin,ymax),-1,0.0001,None,None)
    output = ret['output']
    
    # De-duplicate line sections
    ret = processing.runalg("grass7:v.clean",output,6,0.1,"%f, %f, %f, %f"%(xmin, xmax,ymin,ymax),-1,0.0001,None,None)
    output = ret['output']
    
    # Drop dangling lines less than 1000m
    ret = processing.runalg("grass7:v.clean",output,2,1000,"%f, %f, %f, %f"%(xmin, xmax,ymin,ymax),-1,0.0001,None,None)
    output = ret['output']
    
    # Drop length-0 lines and write to file
    processing.runalg("grass7:v.clean",output,11,0.1,"%f, %f, %f, %f"%(xmin, xmax,ymin,ymax),-1,0.0001,roads_out_path,None)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    