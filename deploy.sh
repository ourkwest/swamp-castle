#!/usr/bin/env bash

lein clean
lein cljsbuild once min

aws --profile deploy s3 sync --acl public-read ./resources/public s3://peterwestmacott/deploy/domination
