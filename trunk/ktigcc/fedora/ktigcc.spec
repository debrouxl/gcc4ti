%define name ktigcc
%define date %(date +%%Y%%m%%d)
%define release 1
%define tigccdir /usr/local/tigcc

Name: %{name}
Version: 0.0.%{date}
Release: %{release}
Vendor: TIGCC Team (http://tigcc.ticalc.org)
Packager: Kevin Kofler <Kevin@tigcc.ticalc.org>
Source: %{name}.tar.bz2
Group: Development/Tools
License: GPL
BuildRequires: qt-devel >= 1:3.3.0 kdelibs-devel >= 6:3.5.2 glib2-devel >= 2.10.0 libticonv >= 20060524 libticables2 >= 20060524 libtifiles2 >= 20060524 libticalcs2 >= 20060524 desktop-file-utils >= 0.10
Requires: kdelibs >= 6:3.5.2 tigcc >= 1:0.96b06r1 /usr/bin/assistant
Requires(post): desktop-file-utils >= 0.10
Requires(postun): desktop-file-utils >= 0.10
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
Summary: KTIGCC is an IDE for TIGCC using KDE
%description
KTIGCC is an IDE for the TIGCC cross-toolchain on *nix/X11 platforms, using the KDE 3 libraries.

%prep
%setup -n %{name}

%build
CXXFLAGS="$RPM_OPT_FLAGS" qmake
make

%install
if [ -d $RPM_BUILD_ROOT ]; then rm -rf $RPM_BUILD_ROOT; fi
mkdir -p $RPM_BUILD_ROOT
make install INSTALL_ROOT=$RPM_BUILD_ROOT
mkdir -p ${RPM_BUILD_ROOT}/usr/share/applications
cat >${RPM_BUILD_ROOT}/usr/share/applications/ktigcc.desktop <<EOF
[Desktop Entry]
Name=KTIGCC
Comment=TIGCC IDE for KDE
GenericName=TIGCC IDE for KDE
Encoding=UTF-8
Version=1.0
Type=Application
Exec=%{tigccdir}/bin/ktigcc
Icon=kdevelop
Terminal=false
Categories=Development;
EOF
desktop-file-install --delete-original --vendor tigcc     \
  --dir ${RPM_BUILD_ROOT}%{_datadir}/applications          \
  ${RPM_BUILD_ROOT}/usr/share/applications/ktigcc.desktop

%post
update-desktop-database %{_datadir}/applications > /dev/null 2>&1 || :

%postun
update-desktop-database %{_datadir}/applications > /dev/null 2>&1 || :

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-, root, root)
%{tigccdir}/bin/ktigcc
%{_datadir}/apps/katepart/syntax/gnuasm68k.xml
%{_datadir}/apps/katepart/syntax/masm68k.xml
%{_datadir}/applications/tigcc-ktigcc.desktop
%doc %{tigccdir}/doc/ktigcc

%changelog
* Sat Jul 17 2006 Kevin Kofler <Kevin@tigcc.ticalc.org>
BuildRequire all the version 2 tilibs.

* Sat Jul 15 2006 Kevin Kofler <Kevin@tigcc.ticalc.org>
Require at least kdelibs 3.5.2 (needed for KProcess::MergedStderr).

* Fri Jun 16 2006 Kevin Kofler <Kevin@tigcc.ticalc.org>
First Fedora RPM.
