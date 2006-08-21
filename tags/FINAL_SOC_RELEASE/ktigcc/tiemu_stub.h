/****************************************************************************
**
** DCOP Stub Definition created by dcopidl2cpp from tiemu.kidl
**
** WARNING! All changes made in this file will be lost!
**
*****************************************************************************/

#ifndef __TIEMU_STUB__
#define __TIEMU_STUB__

#include <dcopstub.h>
#include <qstringlist.h>
#include <dcopobject.h>
#include <qobject.h>


class TiEmuDCOP_stub : virtual public DCOPStub
{
public:
    TiEmuDCOP_stub( const QCString& app, const QCString& id );
    TiEmuDCOP_stub( DCOPClient* client, const QCString& app, const QCString& id );
    explicit TiEmuDCOP_stub( const DCOPRef& ref );
    virtual bool image_loaded();
    virtual int emulated_calc_type();
    virtual int emulated_hw_version();
    virtual QString emulated_os_version();
    virtual bool ready_for_transfers();
    virtual bool send_file( QString  );
    virtual bool send_files( QStringList  );
    virtual bool debug_file( QString  );
    virtual bool reset_calc( bool  );
    virtual bool execute_command( QString command );
    virtual bool turn_calc_on();
    virtual bool enter_debugger();
protected:
    TiEmuDCOP_stub() : DCOPStub( never_use ) {};
};


#endif
