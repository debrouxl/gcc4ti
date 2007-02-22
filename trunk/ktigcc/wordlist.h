#ifndef WORDLIST_H
#define WORDLIST_H

#include "ui_wordlist.h"

class WordList : public QDialog, public Ui::WordList
{
    Q_OBJECT

public:
    WordList(QWidget* parent = 0, const char* name = 0, bool modal = false, Qt::WindowFlags fl = 0);
    ~WordList();

protected slots:
    virtual void languageChange();

};

#endif // WORDLIST_H
