
#include <cstdio>
#include <cstdlib>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <kapplication.h>
#include <kcmdlineargs.h>
#include <kaboutdata.h>
#include <qtextcodec.h>
#include "preferences.h"

//returns 0 on success.
int SynToXML(Syn_SettingsForDoc &syn,const QString &destFileName)
{
	FILE *f=fopen(destFileName,"wt");
	if (!f)
		return 1;
	
    fclose(f);
    return 0;
}
