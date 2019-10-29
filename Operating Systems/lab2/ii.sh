#!/bin/bash
grep -sh \(WW\) ~/.local/share/xorg/Xorg.0.log | sed -E "s/\(WW\)/\(Warning\)/" > full.log
grep -sh \(II\) ~/.local/share/xorg/Xorg.0.log | sed -E "s/\(II\)/\(Information\)/" >> full.log
