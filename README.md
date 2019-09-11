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

## Usage with CI

The `sitebuilder-util` is ideal for use with continuous integration systems. The following sections explain how to set this up.

### Azure Pipelines

*Prerequisites*:

* You will need to have set up a GitHub service connection (assumed to be `connection-name` in the example below)
* You will need to have configured _secret_ variables for the username and password of the external user you want to use (assumed to be named `SB_USER` and `SB_PASSWORD` respectively in the example below)

*Pipeline configuration*:

The following pipeline configuration is set up to download the `latest` version of `sitebuilder-util`, changes permissions to make it executable, and then runs the `edit` to upload the contents of `file.html` to `/fac/sci/dcs/test`. 

```yaml
trigger:
- master

pool:
  vmImage: 'ubuntu-latest'

steps:
- task: DownloadGitHubRelease@0
  inputs:
    connection: 'connection-name'
    userRepository: 'mbg/sitebuilder-util'
    defaultVersionType: 'latest'
    downloadPath: '$(System.ArtifactsDirectory)'

- script: |
    chmod +x $(System.ArtifactsDirectory)/sitebuilder-util
    $(System.ArtifactsDirectory)/sitebuilder-util edit --page=/fac/sci/dcs/test --file=file.html
  displayName: Update Sitebuilder page
  env:
    SB_USER: $(SB_USER)
    SB_PASSWORD: $(SB_PASSWORD)
```