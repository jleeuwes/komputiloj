#!/bin/bash

sed -r 's/^(\s*sb_background).*$/\1 = "%1%w";/' default.theme

