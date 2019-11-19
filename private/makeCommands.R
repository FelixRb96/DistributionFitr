# makes a .sh file that can be used to analyse each package in an individual R-instance


# # The following commands can be used to produce/run the .sh file (from within ./private/):
# rm -r bashout
# rm -r Rout
# Rscript makeCommands.R > run_getFamilies.sh
# bash run_getFamilies.sh > output.txt 2>&1
# # To check for processes that did not finish:
# ps -aux | grep getFamilies.R > stuckProcesses.txt


pkgList <- installed.packages()

pkgList <- pkgList[1:dim(pkgList)[1], 1]

# pkgList <- pkgList[1:50]

cat('mkdir -p ./bashout\n')
cat('mkdir -p ./Rout\n')

for(i in 1:length(pkgList)){
    cat('Rscript test_getFamilies.R "')
    cat(pkgList[i])
    cat('" > ./bashout/output_')
    cat(pkgList[i])
    cat('.txt & \n')
    cat('echo $(date "+%Y-%m-%d %H:%M:%S") ')
    cat(pkgList[i])
    cat('\nsleep 1\n\n')
}