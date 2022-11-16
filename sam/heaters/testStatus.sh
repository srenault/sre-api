#!/usr/bin/bash

sam local invoke HeatersApi -e status.json -t ../template.yml
