import subprocess

output = subprocess.Popen("Rscript --verbose fmr.R > ../Results/fmr.Rout 2> ../Results/TestR_errFile.Rout", stdout=subprocess.PIPE, shell=True)
print(output.stdout.read().decode())