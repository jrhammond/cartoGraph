#!/usr/bin/env python
import grass.script as grass

def main():

	stateslist = [\
	'AF_1_137', 'AO_1_131', 'BD_1_126', 'BI_1_90'\
	, 'CD_1_86', 'CO_1_92', 'DZ_1_191', 'EH_1_135', 'ET_1_133'\
	, 'ET_1_70_1_78', 'ET_1_219', 'ID_1_171', 'IL_1_251_1_37'\
	, 'IN_1_29', 'IN_1_54', 'IN_1_152', 'IN_1_169', 'IN_1_170'\
	, 'IN_1_156', 'IQ_1_62', 'IQ_1_74', 'IR_1_6', 'IR_1_143', 'KH_1_103'\
	, 'LA_1_65', 'LB_1_63', 'LK_1_117_1_157', 'LR_1_146'\
	, 'LY_1_271', 'MM_1_23', 'MM_1_34', 'MM_1_67', 'MZ_1_136'\
	, 'NP_1_72', 'PE_1_95', 'PH_1_10', 'PH_1_112', 'PK_1_129'\
	, 'PK_1_209', 'RU_1_206', 'RU_1_257', 'RW_1_179', 'SD_1_113'\
	, 'SL_1_187', 'SN_1_180', 'SO_1_141', 'SS_1_270', 'TD_1_91'\
	, 'TH_1_248', 'TJ_1_200', 'TL_1_134', 'TR_1_159', 'UG_1_118', 'YE_1_207'\
	]

	

	for state in stateslist:
		stateroads = state + '_roads';
		state_out = '/media/jesse/Files/Dropbox/RoadNetworks/Data/Roads/' + state + '_buffer'

		## Command string 1: begin cleaning
		grass.run_command('v.clean'
			, input = stateroads
			, output = 'temp'
			, type = 'line'
			, tool = ['break', 'rmdupl', 'rmline', 'rmdangle', 'rmdangle', 'rmdangle']
			, thres = [0.00, 0.00, 0.00, 1500.00, 1500.00, 1500.00]
			, overwrite = True
			)

		## Command string 2: simplification

		grass.run_command('v.generalize'
			, input = 'temp'
			, output = 'temp2'
			, method = 'douglas_reduction'
			, threshold = 5000
			, reduction = 20
			, overwrite = True
			)


		## Command string 3: continue cleaning

		grass.run_command('v.clean'
			, input = 'temp2'
			, output = 'temp3'
			, type = 'line'
			, tool = ['break', 'snap', 'break', 'rmdupl', 'rmline', 'break']
			, thres = [0.0, 25.0, 0.0, 0.0, 0.0, 0.0]
			, overwrite = True
			)


		## Command string 4: remove <50m lines

		grass.run_command('v.edit'
			, map = 'temp3'
			, type = 'line'
			, tool = 'delete'
			, threshold = [-1, 0, -50]
			, query = 'length'
			)


		## Command string 5: finish cleaning

		grass.run_command('v.clean'
			, input = 'temp3'
			, output = 'temp4'
			, type = 'line'
			, tool = ['break', 'rmdupl', 'rmline', 'rmdangle']
			, thres = [0.0, 0.0, 0.0, 1000.0]
			, overwrite = True
			)


		## Command string 6: buffer

		grass.run_command('v.buffer'
			, input = 'temp4'
			, output = 'temp5'
			, distance = 51
			, overwrite = True
			)


		## Command string 7: write out

		grass.run_command('v.out.ogr'
			, input = 'temp5'
			, output = state_out
			, format = 'ESRI_Shapefile'
			, overwrite = True
			, 
			)
if __name__ == '__main__':
	main()