#!/bin/bash

sam local invoke FinanceResetVolumeStep -e reset.json -t ../template.yml
