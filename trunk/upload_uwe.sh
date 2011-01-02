#
# Auto upload to MS/Windows R Check

#DEST=129.217.207.166
DEST=win-builder.r-project.org

TARGETREL=/R-release
TARGETDEV=/R-devel
TARGETD64=/R64-release

MAJOR=$(egrep '^MAJOR' src/rattle.R | cut -d\" -f 2)
MINOR=$(egrep '^MINOR' src/rattle.R | cut -d\" -f 2)
#SVNREVIS=$(svn info | egrep 'Revision:' |  cut -d" " -f 2)
#REVISION=$(svn info | egrep 'Revision:' |  cut -d" " -f 2 | awk '{print $1-480}')
REVISION=$(egrep '^REVISION' src/rattle.R | cut -d\" -f 2)
LATEST=rattle_${MAJOR}.${MINOR}.${REVISION}.tar.gz

echo "Upload 'repository/${LATEST}' to MS/Windows Checker"

ftp -n -i << _EOF_
open ${DEST}
user anonymous Graham.Williams@togaware.com
bin
lcd repository
cd ${TARGETREL}
put ${LATEST}
quit
_EOF_

#cd ../${TARGETDEV}
#put ${LATEST}
#cd ../${TARGETD64}
#put ${LATEST}
