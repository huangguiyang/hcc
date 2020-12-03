#ifndef CONFIG_H
#define CONFIG_H

#define CONFIG_VERSION "0.6.0-beta1"
#define CONFIG_TARGET_X86_64 1
#define CONFIG_CC_INCLUDE_DIR "/usr/local/lib/hcc/include"

#define LINUX

#ifdef LINUX

#define CONFIG_TARGET_LINUX 1
#define CONFIG_CRT_PREFIX "/usr/lib64"
#define CONFIG_LIBC_PREFIX "/usr/lib64"
#define CONFIG_DYNAMIC_LINKER "/lib64/ld-linux-x86-64.so.2"

// #define CONFIG_CRT_PREFIX "/usr/lib/x86_64-linux-gnu"
// #define CONFIG_LIBC_PREFIX "/usr/lib/x86_64-linux-gnu"
// #define CONFIG_DYNAMIC_LINKER "/lib64/ld-linux-x86-64.so.2"

/*
  Other prefix:
  CONFIG_CRT_PREFIX "/usr/lib64"
  CONFIG_LIBC_PREFIX "/usr/lib64"
  CONFIG_LIBGCC_PREFIX:
  "/usr/lib/gcc/`gcc -dumpmachine`/`gcc -dumpversion|cut -d. -f1`"
 */

#elif defined DARWIN

/*
 xcrun --show-sdk-path
 xcrun --show-sdk-version
 */
#define CONFIG_TARGET_DARWIN 1
#define CONFIG_MACOS_INCLUDE_DIR "/Applications/Xcode.app/Contents/Developer/\
Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include"

#else
#error "unsupported platform"
#endif

#endif
