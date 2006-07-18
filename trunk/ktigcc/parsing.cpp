/*
   ktigcc - TIGCC IDE for KDE

   Copyright (C) 2006 Kevin Kofler

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
*/

// This file handles parsing of source files for the function list and for
// completion purposes.

#include "parsing.h"
#include "ktigcc.h"
#include <qstringlist.h>
#include <qtextcodec.h>
#include <qapplication.h>
#include <qeventloop.h>
#include <kprocio.h>
#include <kmessagebox.h>
#include <unistd.h>

SourceFileFunctions getCFunctions(const QString &text)
{
  // Parse C using Exuberant Ctags (http://ctags.sourceforge.net).
  SourceFileFunctions result;
  write_temp_file("parser_temp_source.c",text,0);
  {
    // The QTextCodec has to be passed explicitly, or it will default to
    // ISO-8859-1 regardless of the locale, which is just broken.
    KProcIO procio(QTextCodec::codecForLocale());
    // Use MergedStderr instead of Stderr so the messages get ordered
    // properly.
    procio.setComm(static_cast<KProcess::Communication>(
      KProcess::Stdout|KProcess::MergedStderr));
    procio.setWorkingDirectory(tempdir);
    procio<<"ctags"<<"-f"<<"-"<<"-n"<<"-u"<<"-h"<<".h"<<"--language-force=C"
           <<"--C-kinds=pf"<<"--fields=k"<<"parser_temp_source.c";
    if (!procio.start()) {
      delete_temp_file("parser_temp_source.c");
      KMessageBox::error(0,"Could not run ctags.\nThis feature requires "
                           "Exuberant Ctags, which can be obtained from: "
                           "http://ctags.sourceforge.net");
      return result;
    }
    QString line;
    int ret;
    while ((ret=procio.readln(line))>=0 || procio.isRunning()) {
      if (ret>=0) {
        QStringList columns=QStringList::split('\t',line,TRUE);
        QString identifier=columns[0];
        QString linenoString=columns[2];
        int semicolonPos=linenoString.find(';');
        if (semicolonPos>=0) linenoString.truncate(semicolonPos);
        int lineno=linenoString.toInt()-1;
        QString kind=columns[3];
        SourceFileFunctions::Iterator it=result.find(identifier);
        if (kind=="p") {
          if (it==result.end())
            result.append(SourceFileFunction(identifier,lineno,-1));
        } else if (kind=="f") {
          if (it==result.end())
            result.append(SourceFileFunction(identifier,-1,lineno));
          else
            (*it).implementationLine=lineno;
        } else qWarning("Invalid result from ctags.");
      } else {
        usleep(10000);
        QApplication::eventLoop()->processEvents(QEventLoop::ExcludeUserInput,10);
      }
    }
  }
  delete_temp_file("parser_temp_source.c");
  return result;
}  

SourceFileFunctions getASMFunctions(const QString &text)
{
  // Parse ASM by hand.
  QStringList lines=QStringList::split('\n',text,TRUE);
  unsigned lineno=0;
  SourceFileFunctions result;
  for (QStringList::Iterator it=lines.begin(); it!=lines.end(); ++it,++lineno) {
    QString line=*it;
    if (line.isEmpty()) continue;
    QString identifier;
    unsigned col=0, l=line.length();
    while (col<l) {
      QChar c=line[col++];
      if ((c>='A'&&c<='Z')||(c>='a'&&c<='z')||(c>='0'&&c<='9')||c=='_'||c=='$')
        identifier.append(c);
      else
        break;
    }
    if (line[col-1]==':') result.append(SourceFileFunction(identifier,-1,lineno));
  }
  return result;
}
