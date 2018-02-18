import processing
import os, sys, glob

from qgis.core import *
from qgis.gui import QgsMapCanvas
from PyQt4.QtGui import *

stateslist = [\
'AF', 'AO', 'BD', 'BI', 'CD', 'CO', 'DZ',  'EH', 'BD_1-126', 'ET', 'ET_1-133', 'ET_1-219', 'ID_1-171', 'IL', 'IN_1-152', 'IN_1-156', 'IN_1-169', 'IN_1-170', 'IN_1-29', 'IN_1-54', 'IQ', 'IQ_1-74', 'IR_1-143', 'IR_1-6', 'KH', 'LA', 'LB', 'LK', 'LR', 'LY', 'MM_1-23', 'MM_1-34', 'MM_1-67', 'MZ', 'NP', 'PE', 'PH_1-10', 'PH_1-112', 'PK_1-129', 'PK_1-209', 'RU_1-206', 'RU_1-257', 'RW', 'SD', 'SL', 'SN_1-180', 'SO', 'SS', 'TD', 'TH_1-248', 'TJ', 'TL', 'TR_1-159', 'UG', 'YE_1-207'
]

#in_path = 'C:/Users/Jesse/Dropbox/RoadNetworks/Data/Roads/'
in_path = '/media/jesse/Files/Dropbox/RoadNetworks/Data/Roads/'
#out_path = 'C:/Users/Jesse/Dropbox/RoadNetworks/Data/Roads/'
out_path = '/media/jesse/Files/Dropbox/RoadNetworks/Data/Roads/'

for state in stateslist:
    roads_shp_path = in_path + state + '_roads_cleaned.shp'
    roads_out_path = out_path + state + '_roads_cleaned2.shp'
    
    processing.runalg("qgis:deleteduplicategeometries",roads_shp_path,roads_out_path)