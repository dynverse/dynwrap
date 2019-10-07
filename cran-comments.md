Initial submission of dyno: Representing and Inferring Single-Cell Trajectories
  Provides functionality to infer trajectories from single-cell data,
  represent them into a common format, and adapt them. Other biological information
  can also be added, such as cellular grouping, RNA velocity and annotation.
  
## Resubmission

> Thanks, if there are references describing the (theoretical background
of) methods in your package, please add these in the Description field
of your DESCRIPTION file.

Fixed!

> Please do not change the working directory in your functions.
If you really have to, please ensure with on.exit() that the old 
settings are reset.

Thanks! I changed the code to:
```
sink_meta <- .method_init_sinks_and_wd(
  name = method$method$id,
  verbose = verbose,
  return_verbose = return_verbose
)
on.exit(.method_close_sinks_and_wd(sink_meta))
```

> You are using installed.packages().

Make sense that I should not be using installed.packages().
I now use requireNamespace instead.
```
requireNamespace("dynmethods", quietly = TRUE)
```

## Test environments
* local Fedora install, R 3.6.0
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