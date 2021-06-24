# hcc
A tiny C99 compiler

The compiler is designed to be able to compile itself, so it is written in C. And it is intended to support all C99 language features while keeping the code as simple and small as possible.


HOWTO build
----------------
Make sure you have Linux installed. (Any distribution is fine)

To build the compiler, run command:

```
make
make install	# root priviledge required 
```

To build the bootstrap compiler, run command:

```
make bootstrap
```

If you use Debian based distribution like Ubuntu, just modify these macros in **config.h**.

```
CONFIG_CRT_PREFIX
CONFIG_LIBC_PREFIX
```


Author
-------
huangguiyangAToutlook.com

Feel free to report [issues](https://github.com/huangguiyang/hcc/issues) to me.


License
-------
GPLv3.


Reference
---------
1. C Standard Draft: 
    C99: http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf
    C11: http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf

2. X86-64 ABI: 
    v0.90 (Dec 2, 2003): http://people.freebsd.org/~obrien/amd64-elf-abi.pdf
    v0.99.6 (Oct 7, 2013): http://www.x86-64.org/documentation/abi.pdf

3. "Intel 64 and IA-32 Architecture Software Developer Manuals": 
http://www.intel.com/content/www/us/en/processors/architectures-software-developer-manuals.html
