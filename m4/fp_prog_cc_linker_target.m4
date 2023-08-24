# FP_PROG_CC_LINKER_TARGET
# -------------------
# Check to see if the C compiler used as a linker supports `--target`
#
# $1 - Variable which contains the options passed to the C compiler when compiling a C file
# $2 - Variable which contains the options passed to the C compiler when used as
#      a linker
AC_DEFUN([FP_PROG_CC_LINKER_TARGET],
[
    AC_MSG_CHECKING([whether $CC used as a linker understands --target])
    echo 'int foo() { return 0; }' > conftest1.c
    echo 'int main() { return 0; }' > conftest2.c
    "${CC}" $$1 -c conftest1.c || AC_MSG_ERROR([Failed to compile conftest1.c])
    "${CC}" $$1 -c conftest2.c || AC_MSG_ERROR([Failed to compile conftest2.c])
    if "$CC" $$2 --target=$LlvmTarget -o conftest conftest1.o conftest2.o;
    then
        $2="--target=$LlvmTarget $$2"
        AC_MSG_RESULT([yes])
    else
        AC_MSG_RESULT([no])
    fi
    rm -rf conftest*
])# FP_PROG_CC_LINKER_TARGET
