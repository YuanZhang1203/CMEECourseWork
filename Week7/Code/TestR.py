import subprocess
subprocess.Popen("Rscript --verbose TestR.R > ../Results/TestR.Rout 2> ../Results/TestR_errFile.Rout", shell=True).wait()

subprocess.Popen("Rscript --verbose NonExistScript.R > ../Results/outputFile.Rout 2> ../Results/errorFile.Rout", shell=True).wait()

