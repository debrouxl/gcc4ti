#include "toolsdlg.h"

#include <qvariant.h>
#include <qimage.h>
#include <qpixmap.h>

#include "toolsdlg.ui.h"
/*
 *  Constructs a ToolsDialog as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 *
 *  The dialog will by default be modeless, unless you set 'modal' to
 *  true to construct a modal dialog.
 */
ToolsDialog::ToolsDialog(QWidget* parent, const char* name, bool modal, Qt::WindowFlags fl)
    : QDialog(parent, name, modal, fl)
{
    setupUi(this);

    init();
}

/*
 *  Destroys the object and frees any allocated resources
 */
ToolsDialog::~ToolsDialog()
{
    // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void ToolsDialog::languageChange()
{
    retranslateUi(this);
}

