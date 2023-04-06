#!/bin/bash

sam package --region=$AWS_REGION --template-file template.yml --output-template-file package.yml --s3-bucket sreapi-package
