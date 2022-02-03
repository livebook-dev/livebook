set RELEASE_MODE=interactive
if not defined RELEASE_COOKIE (
  for /f "skip=1" %%X in ('wmic os get localdatetime') do if not defined TIMESTAMP set TIMESTAMP=%%X
  set RELEASE_COOKIE=cookie-!TIMESTAMP:~0,11!-!RANDOM!
)
