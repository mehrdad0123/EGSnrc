@echo off

FOR %%x IN (%*) DO CALL :myfunct3 %%x
echo .
GOTO :eof

:myfunct3
	echo .
	echo  Current directory is %1
	PUSHD %1
	call make egsnrc_win6432.F
	POPD
	GOTO :eof