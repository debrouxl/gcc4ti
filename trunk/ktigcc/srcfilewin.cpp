#include "srcfilewin.h"

#include <qvariant.h>
#include <qimage.h>
#include <qpixmap.h>

#include "srcfilewin.ui.h"
/*
 *  Constructs a SourceFileWindow as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 *
 */
SourceFileWindow::SourceFileWindow(QWidget* parent, const char* name, Qt::WindowFlags fl)
    : Q3MainWindow(parent, name, fl)
{
    setupUi(this);

    (void)statusBar();
}

/*
 *  Destroys the object and frees any allocated resources
 */
SourceFileWindow::~SourceFileWindow()
{
    destroy();
    // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void SourceFileWindow::languageChange()
{
    retranslateUi(this);
}

