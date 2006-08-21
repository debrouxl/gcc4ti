%define tigccdir /usr/local/tigcc

Name: ktigcc
Version: 1.01
Release: 1
Vendor: TIGCC Team (http://tigcc.ticalc.org)
Packager: Kevin Kofler <Kevin@tigcc.ticalc.org>
Source: %{name}.tar.bz2
Group: Development/Tools
License: GPL
BuildRequires: qt-devel >= 1:3.3.0 kdelibs-devel >= 6:3.5.2 glib2-devel >= 2.10.0 libticonv-devel >= 20060723 libticables2-devel >= 20060723 libtifiles2-devel >= 20060723 libticalcs2-devel >= 20060723 desktop-file-utils >= 0.10
Requires: kdelibs >= 6:3.5.2 kdebase >= 6:3.5.2 tigcc >= 1:0.96b07r1 ktigcc-completion-data >= 0.96b07r1 /usr/bin/assistant ctags
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
# Install KTIGCC
mkdir -p $RPM_BUILD_ROOT
make install INSTALL_ROOT=$RPM_BUILD_ROOT
# KTIGCC icon
mkdir -p ${RPM_BUILD_ROOT}%{_datadir}/icons/hicolor/16x16/apps
cp -pf images/ktigcc.png ${RPM_BUILD_ROOT}%{_datadir}/icons/hicolor/16x16/apps/ktigcc.png
mkdir -p ${RPM_BUILD_ROOT}%{_datadir}/icons/hicolor/32x32/apps
cp -pf images/icon.png ${RPM_BUILD_ROOT}%{_datadir}/icons/hicolor/32x32/apps/ktigcc.png
mkdir -p ${RPM_BUILD_ROOT}%{_datadir}/icons/hicolor/16x16/mimetypes
pushd ${RPM_BUILD_ROOT}%{_datadir}/icons/hicolor/16x16/mimetypes
ln -s ../apps/ktigcc.png gnome-mime-application-x-tigcc-project.png
popd
mkdir -p ${RPM_BUILD_ROOT}%{_datadir}/icons/hicolor/32x32/mimetypes
pushd ${RPM_BUILD_ROOT}%{_datadir}/icons/hicolor/32x32/mimetypes
ln -s ../apps/ktigcc.png gnome-mime-application-x-tigcc-project.png
popd
# Menu entry
mkdir -p ${RPM_BUILD_ROOT}%{_datadir}/applications
cat >${RPM_BUILD_ROOT}%{_datadir}/applications/ktigcc.desktop <<EOF1
[Desktop Entry]
Name=KTIGCC
Comment=TIGCC IDE for KDE
GenericName=TIGCC IDE for KDE
Encoding=UTF-8
Version=1.0
Type=Application
Exec=%{tigccdir}/bin/ktigcc
Icon=ktigcc
Terminal=false
Categories=Development;
MimeType=application/x-tigcc-project
EOF1
desktop-file-install --delete-original --vendor tigcc     \
  --dir ${RPM_BUILD_ROOT}%{_datadir}/applications          \
  ${RPM_BUILD_ROOT}%{_datadir}/applications/ktigcc.desktop
# GNOME (shared-mime-info) MIME type registration
mkdir -p ${RPM_BUILD_ROOT}%{_datadir}/mime/packages
cat >${RPM_BUILD_ROOT}%{_datadir}/mime/packages/ktigcc.xml <<EOF2
<?xml version="1.0" encoding="UTF-8"?>
<mime-info xmlns="http://www.freedesktop.org/standards/shared-mime-info">
  <mime-type type="application/x-tigcc-project">
    <sub-class-of type="text/plain" />
    <comment>TIGCC Project</comment>
    <glob pattern="*.[tT][pP][rR]" />
  </mime-type>
</mime-info>
EOF2
# KDE (legacy) MIME type registration
mkdir -p ${RPM_BUILD_ROOT}%{_datadir}/mimelnk/application
cat >${RPM_BUILD_ROOT}%{_datadir}/mimelnk/application/x-tigcc-project.desktop <<EOF3
[Desktop Entry]
Encoding=UTF-8
Type=MimeType
MimeType=application/x-tigcc-project
Icon=gnome-mime-application-x-tigcc-project
Patterns=*.tpr;*.TPR;
Comment=TIGCC Project
[Property::X-KDE-text]
Type=bool
Value=true
EOF3

%post
touch --no-create %{_datadir}/icons/hicolor || :
%{_bindir}/gtk-update-icon-cache --quiet %{_datadir}/icons/hicolor || :
update-mime-database %{_datadir}/mime > /dev/null 2>&1 || :
update-desktop-database %{_datadir}/applications > /dev/null 2>&1 || :

%postun
touch --no-create %{_datadir}/icons/hicolor || :
%{_bindir}/gtk-update-icon-cache --quiet %{_datadir}/icons/hicolor || :
update-mime-database %{_datadir}/mime > /dev/null 2>&1 || :
update-desktop-database %{_datadir}/applications > /dev/null 2>&1 || :

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-, root, root)
%{tigccdir}/bin/ktigcc
%{_datadir}/icons/hicolor/16x16/apps/ktigcc.png
%{_datadir}/icons/hicolor/16x16/mimetypes/gnome-mime-application-x-tigcc-project.png
%{_datadir}/icons/hicolor/32x32/apps/ktigcc.png
%{_datadir}/icons/hicolor/32x32/mimetypes/gnome-mime-application-x-tigcc-project.png
%{_datadir}/applications/tigcc-ktigcc.desktop
%{_datadir}/mime/packages/ktigcc.xml
%{_datadir}/mimelnk/application/x-tigcc-project.desktop
%doc %{tigccdir}/doc/ktigcc

%changelog
* Mon Aug 21 2006 Kevin Kofler <Kevin@tigcc.ticalc.org>
Bump version to 1.01.

* Sun Aug 20 2006 Kevin Kofler <Kevin@tigcc.ticalc.org>
Drop date from version and bump to 1.00.
Don't use macros to define nvr.

* Fri Aug 18 2006 Kevin Kofler <Kevin@tigcc.ticalc.org>
Install both 16x16 and 32x32 versions of the icon.
Associate *.tpr with KTIGCC (in both KDE and GNOME).
Don't require desktop-file-utils for post and postun (not needed in FC5+).

* Sun Aug 13 2006 Kevin Kofler <Kevin@tigcc.ticalc.org>
Require ktigcc-completion-data.

* Mon Aug 7 2006 Kevin Kofler <Kevin@tigcc.ticalc.org>
Bump version from 0.0.date (alpha) to 0.80.date (beta).

* Fri Aug 4 2006 Kevin Kofler <Kevin@tigcc.ticalc.org>
Remove no longer needed Kate ASM syntax descriptions.

* Sun Jul 30 2006 Kevin Kofler <Kevin@tigcc.ticalc.org>
Bump TIGCC requirement.

* Wed Jul 26 2006 Kevin Kofler <Kevin@tigcc.ticalc.org>
Require kdebase (needed at least for proxy settings).
Install icon and use it in the .desktop file.

* Sun Jul 23 2006 Kevin Kofler <Kevin@tigcc.ticalc.org>
Use libti*-devel instead of libti* in BuildRequires.
BuildRequire libti* 20060723.

* Tue Jul 18 2006 Kevin Kofler <Kevin@tigcc.ticalc.org>
Require ctags.

* Mon Jul 17 2006 Kevin Kofler <Kevin@tigcc.ticalc.org>
BuildRequire all the version 2 tilibs.

* Sat Jul 15 2006 Kevin Kofler <Kevin@tigcc.ticalc.org>
Require at least kdelibs 3.5.2 (needed for KProcess::MergedStderr).

* Fri Jun 16 2006 Kevin Kofler <Kevin@tigcc.ticalc.org>
First Fedora RPM.
