AC_INIT(xmms2-client, [0.0.1.0], [upwawet@gmail.com])
AC_CONFIG_SRCDIR([xmms2-client.cabal])

AC_ARG_WITH(compiler, [])

c2hs --copy-library

PKG_CHECK_MODULES(XMMS2_CLIENT, xmms2-client, , no_xmms2_client=yes)
if test "$no_xmms2_client" = yes
then BUILD_PACKAGE_BOOL=False
else BUILD_PACKAGE_BOOL=True
fi
AC_SUBST(BUILD_PACKAGE_BOOL)

AC_CONFIG_FILES(xmms2-client.buildinfo
                Makefile
				src/Makefile
                src/Control/Makefile
                src/Control/Monad/Makefile
				src/XMMS2/Makefile
				src/XMMS2/Client/Makefile
				src/XMMS2/Client/Monad/Makefile)
AC_OUTPUT
