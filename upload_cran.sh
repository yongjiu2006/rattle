#
# Auto upload to MS/Windows R Check

VERSION=$(head -1 ChangeLog | cut -d' ' -f 2 | tr -d '\(\)')
LATEST=rattle_${VERSION}.tar.gz

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
