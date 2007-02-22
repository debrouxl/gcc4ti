#include "mainform.h"

#include <qvariant.h>
#include <qimage.h>
#include <qpixmap.h>

#include "mainform.ui.h"
/*
 *  Constructs a MainForm as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 *
 */
MainForm::MainForm(QWidget* parent, const char* name, Qt::WindowFlags fl)
    : Q3MainWindow(parent, name, fl)
{
    setupUi(this);

    (void)statusBar();
    init();
}

/*
 *  Destroys the object and frees any allocated resources
 */
MainForm::~MainForm()
{
    destroy();
    // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void MainForm::languageChange()
{
    retranslateUi(this);
}

