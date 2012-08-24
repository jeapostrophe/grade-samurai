#!/bin/bash

rsync -h --progress -a --delete -L --exclude db --exclude compiled courses plt-etc:local/grade-samurai/
