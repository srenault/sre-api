#!/usr/bin/bash

sam package --profile sre --template-file template.yml --output-template-file package.yml --s3-bucket sreapi
