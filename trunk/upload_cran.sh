#
# Auto upload to MS/Windows R Check

MAJOR=$(egrep '^MAJOR' src/rattle.R | cut -d\" -f 2)
MINOR=$(egrep '^MINOR' src/rattle.R | cut -d\" -f 2)
SVNREVIS=$(svn info | egrep 'Revision:' |  cut -d" " -f 2)
REVISION=$(svn info | egrep 'Revision:' |  cut -d" " -f 2 | awk '{print $1-480}')
LATEST=rattle_${MAJOR}.${MINOR}.${REVISION}.tar.gz

echo "Upload 'repository/${LATEST}' to CRAN."

ftp -n -i << _EOF_
open cran.r-project.org
user anonymous Graham.Williams@togaware.com
bin
cd /incoming
lcd repository
put ${LATEST}
quit
_EOF_
