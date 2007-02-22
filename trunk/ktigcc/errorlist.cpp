#include "errorlist.h"

#include <qvariant.h>
#include <qimage.h>
#include <qpixmap.h>

#include "errorlist.ui.h"
/*
 *  Constructs a ErrorList as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 */
ErrorList::ErrorList(QWidget* parent, const char* name, Qt::WindowFlags fl)
    : QWidget(parent, name, fl)
{
    setupUi(this);

}

/*
 *  Destroys the object and frees any allocated resources
 */
ErrorList::~ErrorList()
{
    // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void ErrorList::languageChange()
{
    retranslateUi(this);
}

