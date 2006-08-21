/****************************************************************************
**
** DCOP Stub Implementation created by dcopidl2cpp from tiemu.kidl
**
** WARNING! All changes made in this file will be lost!
**
*****************************************************************************/

#include "tiemu_stub.h"
#include <dcopclient.h>

#include <kdatastream.h>


TiEmuDCOP_stub::TiEmuDCOP_stub( const QCString& app, const QCString& obj )
  : DCOPStub( app, obj )
{
}

TiEmuDCOP_stub::TiEmuDCOP_stub( DCOPClient* client, const QCString& app, const QCString& obj )
  : DCOPStub( client, app, obj )
{
}

TiEmuDCOP_stub::TiEmuDCOP_stub( const DCOPRef& ref )
  : DCOPStub( ref )
{
}

bool TiEmuDCOP_stub::image_loaded()
{
    bool result = false;
    if ( !dcopClient()  ) {
	setStatus( CallFailed );
	return result;
    }
    QByteArray data, replyData;
    QCString replyType;
    if ( dcopClient()->call( app(), obj(), "image_loaded()", data, replyType, replyData ) ) {
	if ( replyType == "bool" ) {
	    QDataStream _reply_stream( replyData, IO_ReadOnly );
	    _reply_stream >> result;
	    setStatus( CallSucceeded );
	} else {
	    callFailed();
	}
    } else { 
	callFailed();
    }
    return result;
}

int TiEmuDCOP_stub::emulated_calc_type()
{
    int result = 0;
    if ( !dcopClient()  ) {
	setStatus( CallFailed );
	return result;
    }
    QByteArray data, replyData;
    QCString replyType;
    if ( dcopClient()->call( app(), obj(), "emulated_calc_type()", data, replyType, replyData ) ) {
	if ( replyType == "int" ) {
	    QDataStream _reply_stream( replyData, IO_ReadOnly );
	    _reply_stream >> result;
	    setStatus( CallSucceeded );
	} else {
	    callFailed();
	}
    } else { 
	callFailed();
    }
    return result;
}

int TiEmuDCOP_stub::emulated_hw_version()
{
    int result = 0;
    if ( !dcopClient()  ) {
	setStatus( CallFailed );
	return result;
    }
    QByteArray data, replyData;
    QCString replyType;
    if ( dcopClient()->call( app(), obj(), "emulated_hw_version()", data, replyType, replyData ) ) {
	if ( replyType == "int" ) {
	    QDataStream _reply_stream( replyData, IO_ReadOnly );
	    _reply_stream >> result;
	    setStatus( CallSucceeded );
	} else {
	    callFailed();
	}
    } else { 
	callFailed();
    }
    return result;
}

QString TiEmuDCOP_stub::emulated_os_version()
{
    QString result;
    if ( !dcopClient()  ) {
	setStatus( CallFailed );
	return result;
    }
    QByteArray data, replyData;
    QCString replyType;
    if ( dcopClient()->call( app(), obj(), "emulated_os_version()", data, replyType, replyData ) ) {
	if ( replyType == "QString" ) {
	    QDataStream _reply_stream( replyData, IO_ReadOnly );
	    _reply_stream >> result;
	    setStatus( CallSucceeded );
	} else {
	    callFailed();
	}
    } else { 
	callFailed();
    }
    return result;
}

bool TiEmuDCOP_stub::ready_for_transfers()
{
    bool result = false;
    if ( !dcopClient()  ) {
	setStatus( CallFailed );
	return result;
    }
    QByteArray data, replyData;
    QCString replyType;
    if ( dcopClient()->call( app(), obj(), "ready_for_transfers()", data, replyType, replyData ) ) {
	if ( replyType == "bool" ) {
	    QDataStream _reply_stream( replyData, IO_ReadOnly );
	    _reply_stream >> result;
	    setStatus( CallSucceeded );
	} else {
	    callFailed();
	}
    } else { 
	callFailed();
    }
    return result;
}

bool TiEmuDCOP_stub::send_file( QString arg0 )
{
    bool result = false;
    if ( !dcopClient()  ) {
	setStatus( CallFailed );
	return result;
    }
    QByteArray data, replyData;
    QCString replyType;
    QDataStream arg( data, IO_WriteOnly );
    arg << arg0;
    if ( dcopClient()->call( app(), obj(), "send_file(QString)", data, replyType, replyData ) ) {
	if ( replyType == "bool" ) {
	    QDataStream _reply_stream( replyData, IO_ReadOnly );
	    _reply_stream >> result;
	    setStatus( CallSucceeded );
	} else {
	    callFailed();
	}
    } else { 
	callFailed();
    }
    return result;
}

bool TiEmuDCOP_stub::send_files( QStringList arg0 )
{
    bool result = false;
    if ( !dcopClient()  ) {
	setStatus( CallFailed );
	return result;
    }
    QByteArray data, replyData;
    QCString replyType;
    QDataStream arg( data, IO_WriteOnly );
    arg << arg0;
    if ( dcopClient()->call( app(), obj(), "send_files(QStringList)", data, replyType, replyData ) ) {
	if ( replyType == "bool" ) {
	    QDataStream _reply_stream( replyData, IO_ReadOnly );
	    _reply_stream >> result;
	    setStatus( CallSucceeded );
	} else {
	    callFailed();
	}
    } else { 
	callFailed();
    }
    return result;
}

bool TiEmuDCOP_stub::debug_file( QString arg0 )
{
    bool result = false;
    if ( !dcopClient()  ) {
	setStatus( CallFailed );
	return result;
    }
    QByteArray data, replyData;
    QCString replyType;
    QDataStream arg( data, IO_WriteOnly );
    arg << arg0;
    if ( dcopClient()->call( app(), obj(), "debug_file(QString)", data, replyType, replyData ) ) {
	if ( replyType == "bool" ) {
	    QDataStream _reply_stream( replyData, IO_ReadOnly );
	    _reply_stream >> result;
	    setStatus( CallSucceeded );
	} else {
	    callFailed();
	}
    } else { 
	callFailed();
    }
    return result;
}

bool TiEmuDCOP_stub::reset_calc( bool arg0 )
{
    bool result = false;
    if ( !dcopClient()  ) {
	setStatus( CallFailed );
	return result;
    }
    QByteArray data, replyData;
    QCString replyType;
    QDataStream arg( data, IO_WriteOnly );
    arg << arg0;
    if ( dcopClient()->call( app(), obj(), "reset_calc(bool)", data, replyType, replyData ) ) {
	if ( replyType == "bool" ) {
	    QDataStream _reply_stream( replyData, IO_ReadOnly );
	    _reply_stream >> result;
	    setStatus( CallSucceeded );
	} else {
	    callFailed();
	}
    } else { 
	callFailed();
    }
    return result;
}

bool TiEmuDCOP_stub::execute_command( QString arg0 )
{
    bool result = false;
    if ( !dcopClient()  ) {
	setStatus( CallFailed );
	return result;
    }
    QByteArray data, replyData;
    QCString replyType;
    QDataStream arg( data, IO_WriteOnly );
    arg << arg0;
    if ( dcopClient()->call( app(), obj(), "execute_command(QString)", data, replyType, replyData ) ) {
	if ( replyType == "bool" ) {
	    QDataStream _reply_stream( replyData, IO_ReadOnly );
	    _reply_stream >> result;
	    setStatus( CallSucceeded );
	} else {
	    callFailed();
	}
    } else { 
	callFailed();
    }
    return result;
}

bool TiEmuDCOP_stub::turn_calc_on()
{
    bool result = false;
    if ( !dcopClient()  ) {
	setStatus( CallFailed );
	return result;
    }
    QByteArray data, replyData;
    QCString replyType;
    if ( dcopClient()->call( app(), obj(), "turn_calc_on()", data, replyType, replyData ) ) {
	if ( replyType == "bool" ) {
	    QDataStream _reply_stream( replyData, IO_ReadOnly );
	    _reply_stream >> result;
	    setStatus( CallSucceeded );
	} else {
	    callFailed();
	}
    } else { 
	callFailed();
    }
    return result;
}

bool TiEmuDCOP_stub::enter_debugger()
{
    bool result = false;
    if ( !dcopClient()  ) {
	setStatus( CallFailed );
	return result;
    }
    QByteArray data, replyData;
    QCString replyType;
    if ( dcopClient()->call( app(), obj(), "enter_debugger()", data, replyType, replyData ) ) {
	if ( replyType == "bool" ) {
	    QDataStream _reply_stream( replyData, IO_ReadOnly );
	    _reply_stream >> result;
	    setStatus( CallSucceeded );
	} else {
	    callFailed();
	}
    } else { 
	callFailed();
    }
    return result;
}


