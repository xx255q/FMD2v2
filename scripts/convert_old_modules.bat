sed -i -s -E -f convert_old_modules_to_replace.txt modules/*.lua templates/*.lua

REM sed -i -s -E 's/\bpage\b/PAGENUMBER/g' modules/*.lua templates/*.lua
