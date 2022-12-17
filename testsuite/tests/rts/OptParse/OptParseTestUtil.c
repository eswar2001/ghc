#include <stdio.h>
#include "Rts.h"
#include "OptParseTestUtil.h"

extern bool ERROR;

void _TEST( char* flagToTest
          , int expectedFlagKey
          , char* expectedLongName
          , char* expectedShortName
          , RtsFlagValueType expectedFlagValueType
          , bool safe
          , RtsFlagValue expectedValue
          ) {
    debugBelch("\n(TEST) input: %s\n", flagToTest);
    printf("\n(TEST) input: %s\n", flagToTest);
    RtsFlagValue flagValue = parseArg(flagToTest, &ERROR);
    CHECK(!ERROR);
    RtsFlagName flag = rtsFlags[flagValue.key];

    printf("%i: %s %s %s\n", flagValue.key , flag.longName, flag.shortName, safe ? "SAFE": "UNSAFE");
    debugBelch("%i: %s %s %s\n", flagValue.key , flag.longName, flag.shortName, safe ? "SAFE": "UNSAFE");
    CHECK(flagValue.key == expectedFlagKey);
    CHECK(flag.longName == expectedLongName);
    CHECK(flag.shortName == expectedShortName);
    CHECK(flag.valueType == expectedFlagValueType);
    CHECK(flag.optionSafe == safe);
    RtsFlagValueType valueTy = flag.valueType;
    if (valueTy == BOOL) {
        printf("\tvalue: %s\n", flagValue.as.boolean ? "true" : "false");
        CHECK(expectedValue.as.boolean == flagValue.as.boolean);
    }
    if (valueTy == ENUM) {
        printf("\tvalue: %i\n", flagValue.as._enum);
        CHECK(expectedValue.as._enum == flagValue.as._enum);
    }
    if (valueTy == DOUBLE) {
        debugBelch("expected: %f actual: %f\n", expectedValue.as._double, flagValue.as._double);
        printf("\tvalue: %f\n", flagValue.as._double);
        CHECK(expectedValue.as._double == flagValue.as._double);
    }
    if (valueTy == STGWORD64) {
        debugBelch("expected: %" FMT_Word64 " actual: %" FMT_Word64 "\n", expectedValue.as.stgWord64, flagValue.as.stgWord64);
        printf("\tvalue: %" FMT_Word64 "\n", flagValue.as.stgWord64);
        CHECK(expectedValue.as.stgWord64 == flagValue.as.stgWord64);
    }
}

void _FAIL_TEST(char* flagToTest)
{
    debugBelch("\n(FAIL_TEST) input: %s\n", flagToTest);
    RtsFlagValue flagValue = parseArg(flagToTest, &ERROR);
    CHECK(ERROR);
    ERROR = false;
}

void _VOID_FLAG_TEST(const RtsFlagKey i)
{
    RtsFlagName name = rtsFlags[i];
    char buffer[100];
    snprintf(buffer, sizeof(buffer), "--%s", name.longName);
    _TEST( buffer, i, name.longName, name.shortName
        , name.valueType, name.optionSafe, NO_VAL(i));
    snprintf(buffer, sizeof(buffer), "-%s", name.shortName);
    _TEST( buffer, i, name.longName, name.shortName
        , name.valueType, name.optionSafe, NO_VAL(i));
    snprintf(buffer, sizeof(buffer), "-%s=", name.longName);
    _FAIL_TEST(buffer);
    snprintf(buffer, sizeof(buffer), "--%s=123G", name.longName);
    _FAIL_TEST(buffer);
    snprintf(buffer, sizeof(buffer), "--%s=false", name.longName);
    _FAIL_TEST(buffer);
    snprintf(buffer, sizeof(buffer), "--%s=true", name.longName);
    _FAIL_TEST(buffer);
    snprintf(buffer, sizeof(buffer), "-%s=", name.shortName);
    _FAIL_TEST(buffer);
    snprintf(buffer, sizeof(buffer), "-%s3621", name.shortName);
    _FAIL_TEST(buffer);
    snprintf(buffer, sizeof(buffer), "-%s=3622", name.shortName);
    _FAIL_TEST(buffer);
    snprintf(buffer, sizeof(buffer), "-%s=true", name.shortName);
    _FAIL_TEST(buffer);
    snprintf(buffer, sizeof(buffer), "-%s=", name.shortName);
    _FAIL_TEST(buffer);
    snprintf(buffer, sizeof(buffer), "-%s3622", name.shortName);
    _FAIL_TEST(buffer);
    snprintf(buffer, sizeof(buffer), "-%s=3600", name.shortName);
    _FAIL_TEST(buffer);
}

void _BOOL_FLAG_TEST(const RtsFlagKey i)
{
    RtsFlagName name = rtsFlags[i];
    char buffer[100];
    if (name.longName != NULL) {
        snprintf(buffer, sizeof(buffer), "--%s", name.longName);
        _TEST( buffer, i
            , name.longName, name.shortName
            , BOOL, name.optionSafe, BOOL_VAL(i, true));
        snprintf(buffer, sizeof(buffer), "--%s=yes", name.longName);
        _TEST( buffer, i
            , name.longName, name.shortName
            , BOOL, name.optionSafe, BOOL_VAL(i, true));
        snprintf(buffer, sizeof(buffer), "--%s=no", name.longName);
        _TEST( buffer, i
            , name.longName, name.shortName
            , BOOL, name.optionSafe, BOOL_VAL(i, false));
        snprintf(buffer, sizeof(buffer), "--%s=", name.longName);
        _FAIL_TEST(buffer);
        snprintf(buffer, sizeof(buffer), "--%s=foo", name.longName);
        _FAIL_TEST(buffer);
        snprintf(buffer, sizeof(buffer), "--%s=1", name.longName);
        _FAIL_TEST(buffer);
        snprintf(buffer, sizeof(buffer), "--%sjhgl", name.longName);
        _FAIL_TEST(buffer);
    }
    if (name.shortName != NULL) {
        snprintf(buffer, sizeof(buffer), "-%s", name.shortName);
        _TEST( buffer, i
            , name.longName, name.shortName
            , BOOL, name.optionSafe, BOOL_VAL(i, true));
    }
}
