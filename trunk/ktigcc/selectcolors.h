#ifndef SELECTCOLORS_H
#define SELECTCOLORS_H

#include "ui_selectcolors.h"

class SelectColors : public QDialog, public Ui::SelectColors
{
    Q_OBJECT

public:
    SelectColors(QWidget* parent = 0, const char* name = 0, bool modal = false, Qt::WindowFlags fl = 0);
    ~SelectColors();

public slots:
    virtual void colorList_selectionChanged();
    virtual void addButton_clicked();
    virtual void removeButton_clicked();
    virtual void editButton_clicked();

protected slots:
    virtual void languageChange();

};

#endif // SELECTCOLORS_H
