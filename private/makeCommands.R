# Description:
# makes a .sh file that can be used to analyse each package in an individual R-instance


# # The following commands can be used to produce/run the .sh file (from within ./private/):
# (use 'screen' etc.!!!!!!!!)
# rm -r bashout
# rm -r Rout
# Rscript makeCommands.R > run_getFamilies.sh
#
# bash run_getFamilies.sh > output.txt 2>&1
# # or (if a previous run is continued):
# bash run_getFamilies.sh >> output.txt 2>&1
#
# # To check for processes that did not finish:
# ps -aux | grep -P "(?:getFamilies|^USER)" > stuckProcesses.txt 
# Some packages (COMPoissonReg) use all ram and cause the script to crash -> needs to be killed/skipped



pkgList <- installed.packages()

pkgList <- pkgList[1:dim(pkgList)[1], 1]
pkgList <- sort(pkgList)

# pkgList <- pkgList[1:50]

cat('mkdir -p ./bashout\n')
cat('mkdir -p ./Rout\n\n')

for(i in 1:length(pkgList)){
    cat('if [ ! -e bashout/output_')
    cat(pkgList[i])
    cat('.txt ]; then\n')
    cat('Rscript test_getFamilies.R "')
    cat(pkgList[i])
    cat('" > ./bashout/output_')
    cat(pkgList[i])
    cat('.txt 2>&1 & \n')
    cat('echo $(date "+%Y-%m-%d %H:%M:%S") ')
    cat(pkgList[i])
    cat('\nsleep 1\nfi\n\n')
}