import os
import sys
import time
import zipfile

twelve_hours = 12 * 60 * 60

def fixdates(fname):
	current_time = time.gmtime(time.time() - twelve_hours)
	target_name = fname + '.tmp'

	with zipfile.ZipFile(fname, 'r') as zf:
		with zipfile.ZipFile(target_name, 'w') as zf_out:
			for member in zf.namelist():
				zi = zf.getinfo(member)
				data = zf.read(member)
				zi.date_time = (current_time.tm_year, current_time.tm_mon, current_time.tm_mday, current_time.tm_hour, current_time.tm_min, current_time.tm_sec)
				zf_out.writestr(zi, data)
	
	os.rename(target_name, fname)

if __name__ == '__main__':
	if len(sys.argv) < 2:
		print('usage: datefix.py package.vsix')
		sys.exit(1)

	fixdates(sys.argv[1])
