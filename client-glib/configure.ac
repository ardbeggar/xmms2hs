AC_INIT(xmms2-client-glib, [0.0.2.0], [upwawet@gmail.com])
AC_CONFIG_SRCDIR([xmms2-client-glib.cabal])

AC_ARG_WITH(compiler, [])

c2hs --copy-library

BUILD_PACKAGE_BOOL=True

PKG_CHECK_MODULES(XMMS2_CLIENT_GLIB, xmms2-client-glib, , no_xmms2_client_glib=yes)
if test "$no_xmms2_client_glib" = yes
then BUILD_PACKAGE_BOOL=False
fi

PKG_CHECK_MODULES(GLIB, glib-2.0, , no_glib=yes)
if test "$no_glib" = yes
then BUILD_PACKAGE_BOOL=False
fi

AC_SUBST(BUILD_PACKAGE_BOOL)

AC_CONFIG_FILES(xmms2-client-glib.buildinfo
                Makefile
                src/Makefile
                src/XMMS2/Makefile
                src/XMMS2/Client/Makefile)
AC_OUTPUT
