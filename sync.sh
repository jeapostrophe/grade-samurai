#!/bin/bash

rsync -h --progress -a --delete courses plt-etc:local/grade-samurai/
