#!/bin/bash

rsync -h --progress -a --delete -L --exclude db --exclude compiled --exclude .git . plt-etc:local/grade-samurai/
