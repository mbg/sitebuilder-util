# Command-line utilities for Sitebuilder

Utility program for managing Sitebuilder websites.

## Authentication

The `sitebuilder-util` program will expect two environment variables named `SB_USER` and `SB_PASSWORD` to be set for the username and password of an external user account, respectively.

## Edit a page

The following invocation of the program will replace the contents of the page located at `/fac/sci/dcs/test` with the contents of the local file named `foo.html`.

```
$ sitebuilder-util edit --page=/fac/sci/dcs/test --file=foo.html
```

A `--comment` flag may optionally specify a comment for the history of the Sitebuilder page.