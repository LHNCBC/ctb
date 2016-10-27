#!/bin/sh
#
# prepare umls files for synsetgen
#
if [ $# = 0 ]; then
    echo "usage: $0 data-set-name"
    exit 1
fi
DSNAME=$1
echo "Generating inverted index for $DSNAME."
mkdir -p data/ivf/${DSNAME}/tables data/ivf/${DSNAME}/ifindices

# Generate abbreviated version of MRSTY.RRF
if [ ! -e data/ivf/${DSNAME}/tables/zzsty ]; then
    echo "Converting MRSTY.RRF to zzsty"
    gawk 'BEGIN { FS="|" } { printf("%s|%s\n", $1, $4) }' data/input/${DSNAME}/MRSTY.RRF > data/ivf/${DSNAME}/tables/zzsty
fi

# link to full version of MRSTY.RRF
if [ ! -e data/ivf/${DSNAME}/tables/mrsty.rrf ]; then
    (cd data/ivf/${DSNAME}/tables && ln -s ../../../input/${DSNAME}/MRSTY.RRF mrsty.rrf )
fi

# Keep only English rows of MRCONSO.RRF
if [ ! -e data/ivf/${DSNAME}/tables/mrconso.eng ]; then
    gawk -F \| '$2 ~ /ENG/' data/input/${DSNAME}/MRCONSO.RRF > data/ivf/${DSNAME}/tables/mrconso.eng
fi

# link to full version of MRSAT.RRF
# if [ ! -e data/ivf/${DSNAME}/tables/mrsat.rrf ]; then
#    ln -s data/input/${DSNAME}/MRSAT.RRF data/ivf/${DSNAME}/tables/mrsat.rrf
# fi

# create inverted file configuraiion file
echo "NUM_TABLES: 6
zzsty|mrsty|2|0|cui|semtype|TXT|TXT
mrconso.eng|mrconso|18|0|cui|lat|termstatus|lui|termtype|sui|ispref|aui|saui|scui|sdui|sab|tty|code|string|srl|suppress|cvf|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT
zz.name.lbld|zz|4|1|label|casename|termstatus|ambiguity|TXT|TXT|TXT|TXT
mrconso.eng|mrconsostr|18|14|cui|lat|termstatus|lui|termtype|sui|ispref|aui|saui|scui|sdui|sab|tty|code|string|SRL|SUPPRESS|CVF|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT
mrsat.rrf|mrsat|13|0|cui|lui|sui|metaui|stype|code|atui|satui|atn|sab|atv|suppress|cvf|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT
mrsty.rrf|mrstyrrf|6|0|cui|tui|stn|sty|atui|cvf|TXT|TXT|TXT|TXT|TXT|TXT
" > data/ivf/${DSNAME}/tables/ifconfig

# build index after tables are generated
# setenv HEAPSIZE -Xmx1500m
export HEAPSIZE=-Xmx20000m
#export CLASSPATH=lib/irutils-2.0-SNAPSHOT.jar
export CLASSPATH=target/ctb-0.1.0-SNAPSHOT-standalone.jar
export PROJECTROOT=data/ivf/${DSNAME}

# for dbname in mrsty mrconso mrconsostr mrstyrrf mrsat ; do
for dbname in mrsty mrconso mrconsostr mrstyrrf ; do
    echo "Building index for $DSNAME/$dbname."
    java -Dindex.path=${PROJECTROOT}/ifindices \
	 -Dtable.path=${PROJECTROOT}/tables \
	 -Difbuild.lowercase.keys=true \
	 -Difbuild.verbose=true \
	 -Difread.mapped=true \
	 ${HEAPSIZE} irutils.IFBuild ${dbname}
done

