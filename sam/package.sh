#!/bin/bash

sam package --template-file template.yml --output-template-file package.yml --s3-bucket sreapi
