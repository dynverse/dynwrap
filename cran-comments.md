Fixing the check problems as notified by Prof. Brian Ripley.

## Test environments
* local Fedora install, R 3.6.2
* ubuntu 16.04 (on travis-ci), R 3.6.0
* win-builder (devel and release)

## R CMD check results

0 errors | 1 warnings | 1 note

* This is a new release.

```
 checking top-level files ... WARNING
Conversion of 'README.md' failed:
pandoc.exe: Could not fetch https://codecov.io/gh/dynverse/dynwrap/branch/master/graph/badge.svg
HttpExceptionRequest Request {
  host                 = "codecov.io"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/gh/dynverse/dynwrap/branch/master/graph/badge.svg"
  queryString          = ""
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (InternalException (HostCannotConnect "codecov.io" [connect: failed (Connection timed out (WSAETIMEDOUT))]))
 ```
