/*
   ktigcc - TIGCC IDE for KDE

   Copyright (C) 2006-2007 Kevin Kofler
   Copyright (C) 2007 Konrad Meyer

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
#include <QString>
#include <QStringList>
#include <QRegExp>
#include <QTextCodec>
#include <QApplication>
#include <QEventLoop>
#include <QDir>
#include <QLinkedList>
#include <kprocess.h>
#include <kmessagebox.h>
#include <unistd.h>

SourceFileFunctions getCFunctions(const QString &text)
{
  // Parse C using Exuberant Ctags (http://ctags.sourceforge.net).
  SourceFileFunctions result;
  write_temp_file("parser_temp_source.c",text,0);
  {
    KProcess process;
    process.setOutputChannelMode(KProcess::MergedChannels);
    process.setWorkingDirectory(tempdir);
    process<<"ctags"<<"-f"<<"-"<<"-n"<<"-u"<<"-h"<<".h"<<"--language-force=C"
           <<"--C-kinds=pf"<<"--fields=k"<<"-I"<<"CALLBACK,__ATTR_TIOS__,"
            "__ATTR_TIOS_NORETURN__,__ATTR_TIOS_CALLBACK__,__ATTR_GCC__,"
            "__ATTR_LIB_C__,__ATTR_LIB_ASM__,__ATTR_LIB_ASM_NORETURN__,"
            "__ATTR_LIB_CALLBACK_C__,__ATTR_LIB_CALLBACK_ASM__"
           <<"parser_temp_source.c";
    process.start();
    if (!process.waitForStarted()) {
      delete_temp_file("parser_temp_source.c");
      KMessageBox::error(0,"Could not run ctags.\nThis feature requires "
                           "Exuberant Ctags, which can be obtained from: "
                           "http://ctags.sourceforge.net");
      return result;
    }
    bool ret;
    while ((ret=process.canReadLine()) || process.state()!=QProcess::NotRunning) {
      if (ret) {
        QString line=process.readLine();
        line.chop(1); // zap newline
        QStringList columns=line.split('\t');
        int numColumns=columns.count();
        QString identifier;
        if (numColumns) identifier=columns[0];
        QString linenoString;
        if (numColumns>2) linenoString=columns[2];
        int semicolonPos=linenoString.find(';');
        if (semicolonPos>=0) linenoString.truncate(semicolonPos);
        int lineno=linenoString.toInt()-1;
        QString kind;
        if (numColumns>3) kind=columns[3];
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
        QCoreApplication::processEvents(QEventLoop::ExcludeUserInput,10);
      }
    }
  }
  delete_temp_file("parser_temp_source.c");
  return result;
}  

SourceFileFunctions getASMFunctions(const QString &text)
{
  // Parse ASM by hand.
  QStringList lines=text.split('\n');
  unsigned lineno=0;
  SourceFileFunctions result;
  foreach (const QString &line, lines) {
    if (line.isEmpty()) {lineno++; continue;}
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
    lineno++;
  }
  return result;
}

CompletionInfo parseFileCompletion(const QString &fileText,
                                   const QString &pathInProject,
                                   CompletionInfo result)
{
  // Parse included files.
  // Empty lines can be ignored here.
  QStringList lines=fileText.split('\n',QString::SkipEmptyParts);
  bool inComment=false;
  bool isSystemHeader=pathInProject.isNull();
  foreach (const QString &line, lines) {
    if (!inComment) {
      QString strippedLine=line.trimmed();
      if (strippedLine.startsWith("#include")) {
        QString includedName=strippedLine.mid(8).trimmed();
        if (!includedName.isEmpty()) {
          if (includedName[0]=='<') {
            int pos=includedName.find('>',1);
            if (pos>=0)
              result.includedSystem.append(includedName.mid(1,pos-1));
          } else if (includedName[0]=='\"') {
            int pos=includedName.find('\"',1);
            if (pos>=0) {
              if (isSystemHeader)
                // A system header can only include another system header.
                result.includedSystem.append(includedName.mid(1,pos-1));
              else
                result.included.append(QDir::cleanPath(pathInProject+"/"
                                                          +includedName.mid(1,pos-1)));
            }
          } // else ignore
        }
      }
    }
    int pos=0;
    if (inComment) {
      in_comment:
      pos=line.find("*/",pos);
      if (pos<0) continue; // Comment line only, next line.
      pos+=2;
      inComment=false;
    }
    pos=line.find("/*",pos);
    if (pos>=0) {
      pos+=2;
      inComment=true;
      goto in_comment;
    }
  }

  // Parse for prototypes etc. using ctags.
  QString fileTextCleaned=fileText;
  // ctags doesn't like asmspecs.
  fileTextCleaned.remove(QRegExp("\\b(asm|_asm|__asm)\\(\"%?[adAD][0-7]\"\\)"));
  write_temp_file("parser_temp_source.c",fileTextCleaned,0);
  {
    KProcess process;
    process.setOutputChannelMode(KProcess::MergedChannels);
    process.setWorkingDirectory(tempdir);
    process<<"ctags"<<"-f"<<"-"<<"-n"<<"-u"<<"-h"<<".h"<<"--language-force=C"
           <<"--C-kinds=defgpstuvx"<<"--fields=kS"<<"-I"<<"CALLBACK,__ATTR_TIOS__,"
             "__ATTR_TIOS_NORETURN__,__ATTR_TIOS_CALLBACK__,__ATTR_GCC__,"
             "__ATTR_LIB_C__,__ATTR_LIB_ASM__,__ATTR_LIB_ASM_NORETURN__,"
             "__ATTR_LIB_CALLBACK_C__,__ATTR_LIB_CALLBACK_ASM__"
           <<"parser_temp_source.c";
    process.start();
    if (!process.waitForStarted()) {
      delete_temp_file("parser_temp_source.c");
      KMessageBox::error(0,"Could not run ctags.\nThis feature requires "
                           "Exuberant Ctags, which can be obtained from: "
                           "http://ctags.sourceforge.net");
      result.dirty=true;
      return result;
    }
    bool ret;
    while ((ret=process.canReadLine()) || process.state()!=QProcess::NotRunning) {
      if (ret) {
        QString line=process.readLine();
        line.chop(1); // zap newline
        QStringList columns=line.split('\t');
        int numColumns=columns.count();
        QString identifier;
        if (numColumns) identifier=columns[0];
        QString linenoString;
        if (numColumns>2) linenoString=columns[2];
        int semicolonPos=linenoString.find(';');
        if (semicolonPos>=0) linenoString.truncate(semicolonPos);
        int lineno=linenoString.toInt()-1;
        QString kind;
        if (numColumns>3) kind=columns[3];
        QString type=(kind=="d")?"macro"
                     :(kind=="e")?"enum"
                     :(kind=="f" || kind=="p")?"func"
                     :(kind=="v" || kind=="x")?"var"
                     :"type";
        QString signature;
        if (numColumns>4) signature=columns[4];
        if (signature.startsWith("signature:")) signature.remove(0,10);
        signature.replace(QRegExp("\\s*,"),",").replace(QRegExp("\\s*\\)"),")");
        bool alreadyKnown=result.lineNumbers.contains(identifier);
        // This has to be done for system headers because there may already be
        // better information extracted from the .hsf files. However, .hsf files
        // obviously don't contain line number information.
        if (isSystemHeader) {
          foreach (const CompletionEntry &entry, result.entries) {
            if (entry.text == identifier) {
              alreadyKnown=true;
              break;
            }
          }
        }
        if (lineno>=0)
          result.lineNumbers.insert(identifier,lineno,(kind!="p" && kind!="x"));
        if (!alreadyKnown) {
          CompletionEntry entry;
          entry.text=identifier;
          entry.prefix=type;
          entry.postfix=signature;
          result.entries.append(entry);
        }
      } else {
        usleep(10000);
        QCoreApplication::processEvents(QEventLoop::ExcludeUserInput,10);
      }
    }
  }
  delete_temp_file("parser_temp_source.c");

  return result;
}
