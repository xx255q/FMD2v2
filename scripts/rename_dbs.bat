@echo off
pushd data
FOR /F "eol=; tokens=1,2 delims= " %%i in (..\modules.txt) do (
  if exist %%j.db (ren %%j.db %%i.db)
)
popd
