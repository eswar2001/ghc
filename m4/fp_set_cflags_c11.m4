# FP_SET_CFLAGS_C11
# ----------------------------------
# figure out which CFLAGS are needed to place the compiler into C11 mode
# $1 is name of CC variable (unmodified)
# $2 is name of CC flags variable (augmented if needed)
# $3 is name of CPP flags variable (augmented if needed)
# TODO: drop me when we drop centos7
AC_DEFUN([FP_SET_CFLAGS_C11],
[
    AC_MSG_CHECKING([the C compiler for C11 support])
    FPTOOLS_WRITE_FILE([conftest.c], [
      #include <stdio.h>
      #if !defined(__STDC_VERSION__) || __STDC_VERSION__ < 201112L
      # error "Compiler does not advertise C11 conformance"
      #endif
    ])
    if "$$1" $$3 $$2 -c conftest.c -o conftest.o >/dev/null 2>&1
    then
      AC_MSG_RESULT([yes])
    else
      if "$$1" $$3 $$2 -std=gnu11 -c conftest.c -o conftest.o >/dev/null 2>&1
      then
        AC_MSG_RESULT([yes with -std=gnu11 added])
        $2="-std=gnu11 $$2"
        $3="-std=gnu11 $$3"
      else
        AC_MSG_ERROR([no])
      fi
    fi
    rm -f conftest.c conftest.o
])
