#!/bin/sh
ls *.tpr | awk 'BEGIN {print "#!/bin/sh"} {print "tprbuilder -q \"" $0 "\"; if [ $? -ne 0 ]; then echo FAILED building \"\\\"" $0 "\\\"\"; fi"} END{print "rm *.??z"; print "rm *.??y"; print "rm *.dbg"}' > build.sh
