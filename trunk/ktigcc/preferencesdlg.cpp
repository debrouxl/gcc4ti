#include "preferencesdlg.h"

#include <qvariant.h>
#include <qimage.h>
#include <qpixmap.h>

#include "preferencesdlg.ui.h"
/*
 *  Constructs a Preferences as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 *
 *  The dialog will by default be modeless, unless you set 'modal' to
 *  true to construct a modal dialog.
 */
Preferences::Preferences(QWidget* parent, const char* name, bool modal, Qt::WindowFlags fl)
    : QDialog(parent, name, modal, fl)
{
    setupUi(this);

    init();
}

/*
 *  Destroys the object and frees any allocated resources
 */
Preferences::~Preferences()
{
    destroy();
    // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void Preferences::languageChange()
{
    retranslateUi(this);
}

