#
# Auto upload to MS/Windows R Check

#DEST=129.217.207.166
DEST=win-builder.r-project.org

TARGETREL=/R-release
TARGETDEV=/R-devel

VERSION=$(head -1 ChangeLog | cut -d' ' -f 2 | tr -d '\(\)')
LATEST=rattle_${VERSION}.tar.gz

echo "Upload 'repository/${LATEST}' to MS/Windows Checker"

ftp -n -i << _EOF_
open ${DEST}
user anonymous Graham.Williams@togaware.com
bin
lcd repository
cd ${TARGETREL}
put ${LATEST}
cd ../${TARGETDEV}
put ${LATEST}
quit
_EOF_

