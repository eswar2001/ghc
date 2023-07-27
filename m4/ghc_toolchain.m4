dnl $1  argument name
dnl $2  variable
AC_DEFUN([ADD_GHC_TOOLCHAIN_ARG],
[
    set -- $2
    for x do
        echo "--$1=$x" >> acargs
    done
])

dnl $1 argument name
dnl $2 first variable to try
dnl $3 variable to add if the first variable is empty
AC_DEFUN([ADD_GHC_TOOLCHAIN_ARG_CHOOSE],
[
    if test -z "$2"; then
        ADD_GHC_TOOLCHAIN_ARG([$1],[$3])
    else
        ADD_GHC_TOOLCHAIN_ARG([$1],[$2])
    fi
])

AC_DEFUN([ENABLE_GHC_TOOLCHAIN_ARG],
[
    if test "$2" = "YES"; then
        echo "--enable-$1" >> acargs
    elif test "$2" = "yes"; then
        echo "--enable-$1" >> acargs
    elif test "$2" = "NO"; then
        echo "--disable-$1" >> acargs
    elif test "$2" = "no"; then
        echo "--disable-$1" >> acargs
    fi
])

AC_DEFUN([ENABLE_GHC_TOOLCHAIN_NOT_ARG],
[
    if test "$2" = "NO"; then
        echo "--enable-$1" >> acargs
    elif test "$2" = "YES"; then
        echo "--disable-$1" >> acargs
    elif test "$2" = "no"; then
        echo "--enable-$1" >> acargs
    elif test "$2" = "yes"; then
        echo "--disable-$1" >> acargs
    fi
])

AC_DEFUN([INVOKE_GHC_TOOLCHAIN],
[
    (
        set --
        while read -r arg; do
            set -- "[$]@" "$arg"
        done
        # For now, we don't 'exit' even if ghc-toolchain fails. We don't want to
        # fail configure due to it, since the target file is still being generated by configure.
        "$GHC_TOOLCHAIN_BIN" -v2 "[$]@"
    ) <acargs
])

dnl $1 is the path to the directory where to put the configured default.host.target.ghc-toolchain and default.target.ghc-toolchain
dnl $2 is YES or NO,
dnl     * YES means we're calling GHC_TOOLCHAIN from the bindist configure script,
dnl         and that ghc-toolchain is already an available binary
dnl     * NO means we're calling GHC_TOOLCHAIN from the source tree configure script,
dnl         and that we must compile ghc-toolchain before invoking it
AC_DEFUN([FIND_GHC_TOOLCHAIN],
[
    case "$2" in
        YES)
            # We're configuring the bindist, and the binary is already available
            GHC_TOOLCHAIN_BIN="bin/ghc-toolchain-bin"
            ;;
        NO)
            # We're in the source tree, so compile ghc-toolchain
            "$GHC" -v0 \
                -ilibraries/ghc-platform/src -iutils/ghc-toolchain/src \
                -XNoImplicitPrelude \
                -odir actmp-ghc-toolchain -hidir actmp-ghc-toolchain \
                utils/ghc-toolchain/exe/Main.hs -o acghc-toolchain
            GHC_TOOLCHAIN_BIN="./acghc-toolchain"
            ;;
        *)
            AC_MSG_ERROR([In m4/ghc_toolchain.m4, expecting $2 to be either YES or NO.])
            ;;
    esac

    # (1) Configure a toolchain for the build and host platform (we require that BUILD=HOST, so one toolchain suffices)
    rm -f acargs
    echo "--triple=$HostPlatform" >> acargs
    echo "--output=$1/default.host.target.ghc-toolchain" >> acargs
    echo "--cc=$CC_STAGE0" >> acargs
    echo "--cc-link=$CC_STAGE0" >> acargs
    echo "--ar=$AR_STAGE0" >> acargs
    dnl The remaining tools we don't configure for the host.
    dnl See Note [The dummy values in the HOST target description]

    INVOKE_GHC_TOOLCHAIN()

    # (2) Configure a toolchain for the target platform (the toolchain is based
    # on the triple (or manually specified), and runs on the platform
    # configuring it and produces code for the given target)
    # We might not find the correct toolchain, and fallback to the default
    # toolchain. We should handle it more graciously.
    #
    # We pass the paths to the programs found by configure.
    # The flags for the toolchain configured by ghc-toolchain will still be
    # validated against those configured by configure, but ghc-toolchain
    # doesn't take into account things like environment variables or bundled
    # (windows) toolchains, so we explicitly pass them as arguments here.
    # ghc-toolchain is still able to find programs if not explicitly given in
    # the usual system locations, including the PATH, we are just explicit when
    # calling it through configure.
    rm -f acargs

    echo "--triple=$target" >> acargs
    echo "--output=$1/default.target.ghc-toolchain" >> acargs
    echo "--llvm-triple=$LlvmTarget" >> acargs
    echo "--cc=$CC" >> acargs
    echo "--cxx=$CXX" >> acargs
    echo "--cpp=$CPPCmd" >> acargs
    echo "--hs-cpp=$HaskellCPPCmd" >> acargs
    echo "--cc-link=$CC" >> acargs
    echo "--ar=$AR" >> acargs
    echo "--ranlib=$RANLIB" >> acargs
    echo "--nm=$NM" >> acargs
    echo "--merge-objs=$MergeObjsCmd" >> acargs
    echo "--readelf=$READELF" >> acargs
    echo "--windres=$WindresCmd" >> acargs

    ENABLE_GHC_TOOLCHAIN_NOT_ARG([locally-executable], [$CrossCompiling])
    ENABLE_GHC_TOOLCHAIN_ARG([unregisterised], [$Unregisterised])
    ENABLE_GHC_TOOLCHAIN_ARG([tables-next-to-code], [$TablesNextToCode])
    ENABLE_GHC_TOOLCHAIN_ARG([ld-override], [$enable_ld_override])
    ENABLE_GHC_TOOLCHAIN_ARG([libffi-adjustors], [$UseLibffiForAdjustors])

    dnl We store USER_* variants of all user-specified flags to pass them over to ghc-toolchain.
    ADD_GHC_TOOLCHAIN_ARG_CHOOSE([cc-opt], [$USER_CONF_CC_OPTS_STAGE2], [$USER_CFLAGS])
    ADD_GHC_TOOLCHAIN_ARG_CHOOSE([cc-link-opt], [$USER_CONF_GCC_LINKER_OPTS_STAGE2], [$USER_LDFLAGS])
    ADD_GHC_TOOLCHAIN_ARG([cc-link-opt], [$USER_LIBS])
    ADD_GHC_TOOLCHAIN_ARG_CHOOSE([cxx-opt], [$USER_CONF_CXX_OPTS_STAGE2], [$USER_CXXFLAGS])
    ADD_GHC_TOOLCHAIN_ARG([cpp-opt], [$USER_CPP_ARGS])
    ADD_GHC_TOOLCHAIN_ARG([hs-cpp-opt], [$USER_HS_CPP_ARGS])

    INVOKE_GHC_TOOLCHAIN()

    rm -Rf acargs acghc-toolchain actmp-ghc-toolchain

    dnl Note: if we weren't passing the paths to the programs explicitly, to make
    dnl ghc-toolchain use the bundled windows toolchain, simply add it to the search PATH
])


dnl Note [ghc-toolchain consistency checking]
dnl ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dnl ghc-toolchain is the brand new way (2023) to configure toolchains for GHC,
dnl but this novelty musn't break user's installations, so we still
dnl conservatively use the toolchains configured by configure (see also m4/prep_target_file.m4).
dnl
dnl However, we already ship and run ghc-toolchain at configure time to /validate/ ghc-toolchain:
dnl * PREP_TARGET_FILE substitutes the toolchain into default.target.in and default.host.target.in
dnl * FIND_GHC_TOOLCHAIN generates a target description file through ghc-toolchain
dnl * VALIDATE_GHC_TOOLCHAIN compares the output of the two, warning about the differences.
dnl
dnl This is crucial to validate ghc-toolchain and preemptively fix bugs before it is the default.
dnl
dnl (And the configure flag --enable-ghc-toolchain makes hadrian use the target
dnl files generated by ghc-toolchain instead).

dnl $1 like "default.target"
dnl $2 like "default.target.ghc-toolchain"
AC_DEFUN([VALIDATE_GHC_TOOLCHAIN],[
    diff_output=`diff "$1" "$2" 2>&1`
    if test -z "$diff_output"; then
      true
    else
      AC_MSG_WARN([
There are some differences between the toolchain configured by "configure" ($1) and the toolchain configured by the "ghc-toolchain" program ($2).

$diff_output

Don't worry! This won't affect your ghc in any way.
However, in a near future, we will move to configuring toolchains with "ghc-toolchain" by default, so you might have discovered a future bug.
In light of it, if you've spotted this difference, please report a GHC bug at https://www.haskell.org/ghc/reportabug])

    case "$EnableStrictGhcToolchainCheck" in
        YES)
          AC_MSG_ERROR([Failing due to --enable-strict-ghc-toolchain-check])
    esac
    fi
])

dnl Note [The dummy values in the HOST target description]
dnl ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dnl In configure, we don't configure nearly as much tools for
dnl the HOST toolchain as we do for the TARGET toolchain. This is because
dnl Hadrian only depends on certain properties and tools of the HOST toolchain,
dnl and, ultimately, the shipped GHC has in `settings` the TARGET toolchain and
dnl properties. (In constrast, ghc-toolchain can as easily configure 1
dnl toolchain as it can 100)
dnl
dnl Unfortunately, we need to produce a valid Target value to write to default.host.target.
dnl Since we don't configure the values required to substitute into the
dnl toolchain, we simply use /dummy/ values, as conservatively as possible.
dnl Regardless of the conservative values, we assume that hadrian will never
dnl look at these settings, as they previously didn't exist.
dnl
dnl In practice, Hadrian should only access the *_STAGE0 settings that were
dnl available before the ghc-toolchain: Toolchain Selection commit.

